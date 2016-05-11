{-# LANGUAGE OverloadedStrings, ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}
    
module Blockchain.Data.Blockchain
    ( 
      createDBAndInsertBlockchain
    ) where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql hiding (get)

import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Reader


{- global registry of blockchains -}

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Blockchain
    path String
    uuid String
    deriving Show
|]

createDBAndInsertBlockchain pgConn path uuid = runNoLoggingT $ withPostgresqlConn pgConn $ runReaderT $ do
    runMigration migrateAll
    insert $ Blockchain { 
                 blockchainPath = path,
                 blockchainUuid = uuid
             }      
