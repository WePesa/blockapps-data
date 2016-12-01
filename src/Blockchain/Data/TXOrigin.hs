{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
    
module Blockchain.Data.TXOrigin where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Database.Persist.TH

import Data.Aeson

import Blockchain.Data.PersistTypes ()
import Blockchain.MiscJSON ()

import Blockchain.Format
import Blockchain.SHA

import qualified GHC.Word as GW
import GHC.Generics

data TXOrigin = Direct | API | BlockHash SHA | PeerString String deriving (Show, Read, Eq, Generic)

derivePersistField "TXOrigin"

instance ToJSON TXOrigin where
instance FromJSON TXOrigin where

instance Binary TXOrigin where
    put Direct          = putWord8 0
    put API             = putWord8 1
    put (BlockHash sha) = putWord8 2 >> put sha
    put (PeerString p)  = putWord8 3 >> put p
    get = do
        tag <- getWord8
        case tag of
            0 -> return Direct
            1 -> return API
            2 -> BlockHash  <$> get
            3 -> PeerString <$> get

instance Format TXOrigin where
    format = show
