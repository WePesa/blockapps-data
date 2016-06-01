{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
    
module Blockchain.Data.TXOrigin where


import Database.Persist.TH

import Data.Aeson

import Blockchain.Data.PersistTypes ()
import Blockchain.MiscJSON ()

import Blockchain.SHA

import GHC.Generics

data TXOrigin = API | BlockHash SHA | PeerString String deriving (Show, Read, Eq, Generic)

derivePersistField "TXOrigin"

instance ToJSON TXOrigin where
instance FromJSON TXOrigin where


  
