{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module Blockchain.Setup (
  oneTimeSetup
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger (runNoLoggingT,runStdoutLoggingT)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C
import qualified Database.LevelDB as DB
import Database.Persist.Postgresql hiding (get)
import System.Directory
import System.FilePath
import System.Console.Readline
import Data.Maybe
-- import Data.Aeson
import Data.Yaml
import qualified Data.Map as Map
import qualified Data.Text as T
import System.Entropy
import System.Process

import qualified Blockchain.Colors as CL
import qualified Blockchain.Database.MerklePatricia as MP
import Blockchain.Data.DataDefs
import Blockchain.Data.GenesisBlock
import Blockchain.DB.CodeDB
import Blockchain.DB.HashDB
import Blockchain.DB.StateDB
import Blockchain.DB.SQLDB
import Blockchain.Constants
import Blockchain.EthConf
import Blockchain.KafkaTopics
import Blockchain.PeerUrls

data SetupDBs =
  SetupDBs {
    stateDB::StateDB,
    hashDB::HashDB,
    codeDB::CodeDB,
    sqlDB::SQLDB
    }

type SetupDBM = StateT SetupDBs (ResourceT IO)
instance HasStateDB SetupDBM where
  getStateDB = do
    cxt <- get
    return $ stateDB cxt
  setStateDBStateRoot sr = do
    cxt <- get
    put cxt{stateDB=(stateDB cxt){MP.stateRoot=sr}}

{-instance HasStorageDB SetupDBM where
  getStorageDB = do
    cxt <- get
    return $ MPDB.ldb $ setupDBStateDB cxt --storage and states use the same database!-}

instance HasHashDB SetupDBM where
  getHashDB = fmap hashDB get

instance HasCodeDB SetupDBM where
  getCodeDB = fmap codeDB get

instance HasSQLDB SetupDBM where
  getSQLDB = fmap sqlDB get

{-
connStr::ConnectionString
connStr = "host=localhost dbname=eth user=postgres password=api port=5432"
-}

defaultSqlConfig :: SqlConf
defaultSqlConfig = 
    SqlConf {
      user = "postgres",
      password = "api",
      host = "localhost",
      port = 5432,
      database = "eth",
      poolsize = 10
    } 


defaultLevelDBConfig :: LevelDBConf
defaultLevelDBConfig =
    LevelDBConf { 
      table = "",
      path = ""
    }

defaultBlockConfig :: BlockConf
defaultBlockConfig = 
    BlockConf {
      blockTime = 13
    }


defaultEthUniqueId :: EthUniqueId
defaultEthUniqueId = 
    EthUniqueId {
      peerId = "",
      genesisHash = "",
      networkId = 0  
    } 

defaultQuarryConfig :: QuarryConf
defaultQuarryConfig = 
    QuarryConf { 
      coinbaseAddress = 0xab,
      lazyBlocks = False
    }

defaultConfig :: EthConf
defaultConfig = 
    EthConf { 
      ethUniqueId = defaultEthUniqueId,
      sqlConfig = defaultSqlConfig,
      levelDBConfig = defaultLevelDBConfig,
      blockConfig = defaultBlockConfig,  
      quarryConfig = defaultQuarryConfig
    }
                   
defaultPeers::[(String,Int)]
defaultPeers = undefined


kafkaPath :: FilePath
kafkaPath = "/home" </> "kafka" </> "kafka" </> "bin"

type Topic' = String

createKafkaTopic :: Topic' -> IO () 
createKafkaTopic topic = callProcess 
                           (kafkaPath </> "kafka-topics.sh") 
                           ([ "--create",
                              "--zookeeper localhost:2181", 
                              "--replication-factor 1", 
                              "--partitions 1",
                              "--topic " ++ topic ] )

topics :: [Topic']
topics = [ "block",
           "unminedblock",
           "blockapps-data",
           "strato-p2p-client",
           "queryStrato" ]

createKafkaTopics :: [Topic'] -> IO ()
createKafkaTopics top = sequence_ . (map createKafkaTopic) $ top


{-
  CONFIG: 

  oneTimeSetup now creates .ethereumH and moves config files into it.
  It then creates the databases namespaced by UUIDs. We could probably use local paths here,
  but those strings might get annoyingly long. 

  To be safe, this operation should be idempotent. Thus we check for the presence of ~/.ethereumH.

  Preconditions: installed LevelDB, Postgres, Kafka.
-}

oneTimeSetup :: String -> IO ()
oneTimeSetup genesisBlockName = do
  dirExists <- doesDirectoryExist ".ethereumH"
  if dirExists
    then do  
        putStrLn ".ethereumH exists, unsafe to run setup"
        return ()
    else do  

     {- CONFIG create default config files -} 

      putStrLn $ "writing config"

      maybePGuser <- readline "enter a postgres user with database credentials (postgres): "
      maybePGpass <- readline "enter password for database user: "
      bytes <- getEntropy 20

      createDirectoryIfMissing True $ dbDir "h"

      let user' =  case maybePGuser of 
                        Nothing -> "postgres"
                        Just "" -> "postgres"
                        Just user' -> user'

          cfg = defaultConfig { 
                  sqlConfig = defaultSqlConfig { 
                    user = user',
                    password = fromMaybe "" maybePGpass
                  }
                }

     
     {- CONFIG: create database and write default config files-}
     
      let uniqueString = C.unpack . B16.encode $ bytes 
          pgCfg = sqlConfig cfg
          pgCfg' = pgCfg { database = "" } 
          db = database pgCfg
          db' = db ++ "_" ++ uniqueString
          pgCfg'' = pgCfg { database = db' }
          pgConn = postgreSQLConnectionString pgCfg
          pgConn' = postgreSQLConnectionString pgCfg'
          cfg' = cfg { 
                   sqlConfig = pgCfg'', 
                   ethUniqueId = defaultEthUniqueId {
                     peerId = uniqueString
                   }
                 }

      encodeFile ".ethereumH/ethconf.yaml" cfg'

      liftIO $ putStrLn $ CL.yellow ">>>> Creating Database " ++ db'
      liftIO $ putStrLn $ CL.blue $ "  connection is " ++ (show pgConn')

      let query = T.pack $ "CREATE DATABASE " ++ (show db') ++ ";"

      runNoLoggingT $ withPostgresqlConn pgConn' $ runReaderT $ rawExecute query []

     {- CONFIG: create kafka topics -} 

      let uniqueTopicMap = foldr (\topic tmpMap -> (Map.insert 
                                                     topic 
                                                     (topic ++ "_" ++ uniqueString)
                                                     tmpMap)) 
                                 Map.empty 
                                 topics

      encodeFile ".ethereumH/topics.yaml" uniqueTopicMap

    {- kafkaTopics implicitly defined by ethconf.yaml above & unsafePerformIO -}

      createKafkaTopics (Map.elems kafkaTopics)
  
     {- CONFIG: define tables and indices -}
     {- connStr implicitly defined by ethconf.yaml above, & unsafePerformIO -}  
     
      runNoLoggingT $ withPostgresqlConn connStr $ runReaderT $ do
         liftIO $ putStrLn $ CL.yellow ">>>> Migrating SQL DB"
         liftIO $ putStrLn $ CL.blue $ "  connection is " ++ (show connStr)

         _ <- runMigrationSilent migrateAll

         liftIO $ putStrLn $ CL.yellow ">>>> Creating SQL Indexes"
         rawExecute "CREATE INDEX CONCURRENTLY ON block_data_ref (block_id);" []
         rawExecute "CREATE INDEX CONCURRENTLY ON block_data_ref (number);" []
         rawExecute "CREATE INDEX CONCURRENTLY ON block_data_ref (hash);" []
         rawExecute "CREATE INDEX CONCURRENTLY ON block_data_ref (parent_hash);" []
         rawExecute "CREATE INDEX CONCURRENTLY ON block_data_ref (coinbase);" []
         rawExecute "CREATE INDEX CONCURRENTLY ON block_data_ref (total_difficulty);" []

         rawExecute "CREATE INDEX CONCURRENTLY ON address_state_ref (address);" []

         rawExecute "CREATE INDEX CONCURRENTLY ON raw_transaction (from_address);" []
         rawExecute "CREATE INDEX CONCURRENTLY ON raw_transaction (to_address);" []
         rawExecute "CREATE INDEX CONCURRENTLY ON raw_transaction (block_number);" [] 
         rawExecute "CREATE INDEX CONCURRENTLY ON raw_transaction (tx_hash);" [] 

         rawExecute "CREATE INDEX CONCURRENTLY ON storage (key);" []

     {- create directory and dbs -} 
      _ <-
          runResourceT $ do
              liftIO $ putStrLn $ CL.yellow ">>>> Setting UP DB handles"

          {- CONFIG: localized -}

              sdb <- DB.open (dbDir "h" ++ stateDBPath)
                     DB.defaultOptions{DB.createIfMissing=True, DB.cacheSize=1024}
              hdb <- DB.open (dbDir "h" ++ hashDBPath)
                     DB.defaultOptions{DB.createIfMissing=True, DB.cacheSize=1024}
              cdb <- DB.open (dbDir "h" ++ codeDBPath)
                     DB.defaultOptions{DB.createIfMissing=True, DB.cacheSize=1024}
              let smpdb = MP.MPDB{MP.ldb=sdb, MP.stateRoot=error "stateRoot not defined in oneTimeSetup"}

              pool <- runNoLoggingT $ createPostgresqlPool connStr 20

              flip runStateT (SetupDBs smpdb hdb cdb pool) $ do
                addCode B.empty --blank code is the default for Accounts, but gets added nowhere else.
                liftIO $ putStrLn $ CL.yellow ">>>> Initializing Genesis Block"
                initializeGenesisBlock genesisBlockName

              return ()

                    

     {- create Kafka topics -} 

      return ()

