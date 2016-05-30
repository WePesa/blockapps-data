{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
    
module Blockchain.Data.TXOrigin where


import Database.Persist
import Database.Persist.TH
import Database.Persist.Quasi

import Crypto.Types.PubKey.ECC

import Data.Aeson
import Data.Text
import Data.Time

import Blockchain.Data.Address
import Blockchain.Data.PersistTypes ()
import Blockchain.MiscJSON ()
import Blockchain.Data.TransactionDef
import Blockchain.Database.MerklePatricia

import qualified Data.ByteString as BS

import Blockchain.SHA
import Blockchain.ExtWord
import Data.Word

import GHC.Generics

data TXOrigin = API | BlockHash SHA | PeerString String deriving (Show, Read, Eq, Generic)

derivePersistField "TXOrigin"

instance ToJSON TXOrigin where
instance FromJSON TXOrigin where


  
