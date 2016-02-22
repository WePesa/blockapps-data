{-# LANGUAGE DeriveGeneric #-}
     
module Blockchain.Data.Code where

import qualified Data.ByteString as B
import GHC.Generics 
    
import Blockchain.Data.RLP
import Blockchain.ExtWord
       
data Code =
  Code{codeBytes::B.ByteString}
  | PrecompiledCode Int deriving (Show, Eq, Read, Ord, Generic)

instance RLPSerializable Code where
    rlpEncode (Code bytes) = rlpEncode bytes
    rlpDecode = Code . rlpDecode

-- instance Format Code where
--    format Code {codeBytes=c} = B.unpack c
