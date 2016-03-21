
module Blockchain.Data.BlockHeader (
  BlockHeader(..)
  ) where

import qualified Data.ByteString as B
import Data.Time
import Data.Time.Clock.POSIX
import Data.Word
import Numeric

import qualified Blockchain.Colors as CL
import Blockchain.Data.Address
import Blockchain.Data.RLP
import qualified Blockchain.Database.MerklePatricia as MP
import Blockchain.Format
import Blockchain.SHA
import Blockchain.Util


data BlockHeader =
  BlockHeader {
    parentHash::SHA,
    ommersHash::SHA,
    beneficiary::Address,
    stateRoot::MP.SHAPtr,
    transactionsRoot::MP.SHAPtr,
    receiptsRoot::MP.SHAPtr,
    logsBloom::B.ByteString,
    difficulty::Integer,
    number::Integer,
    gasLimit::Integer,
    gasUsed::Integer,
    timestamp::UTCTime,
    extraData::Integer,
    mixHash::SHA,
    nonce::Word64
    } deriving (Show)

instance Format BlockHeader where
  format (BlockHeader ph oh b sr tr rr lb d number gl gu ts ed mh nonce) =
    CL.blue ("BlockData #" ++ show number) ++ " " ++
    tab ("parentHash: " ++ format ph ++ "\n" ++
         "ommersHash: " ++ format oh ++ 
         (if oh == hash (B.pack [0xc0]) then " (the empty array)\n" else "\n") ++
         "beneficiary: " ++ format b ++ "\n" ++
         "stateRoot: " ++ format sr ++ "\n" ++
         "transactionsRoot: " ++ format tr ++ "\n" ++
         "receiptsRoot: " ++ format rr ++ "\n" ++
         "difficulty: " ++ show d ++ "\n" ++
         "gasLimit: " ++ show gl ++ "\n" ++
         "gasUsed: " ++ show gu ++ "\n" ++
         "timestamp: " ++ show ts ++ "\n" ++
         "extraData: " ++ show ed ++ "\n" ++
         "nonce: " ++ showHex (nonce) "")

instance RLPSerializable BlockHeader where
  rlpEncode (BlockHeader ph oh b sr tr rr lb d number gl gu ts ed mh nonce) = undefined
  rlpDecode (RLPArray [ph, oh, b, sr, tr, rr, lb, d, number, gl, gu, ts, ed, mh, nonce]) = 
    BlockHeader {
      parentHash=rlpDecode ph,
      ommersHash=rlpDecode oh,
      beneficiary=rlpDecode b,
      stateRoot=rlpDecode sr,
      transactionsRoot=rlpDecode tr,
      receiptsRoot=rlpDecode rr,
      logsBloom=rlpDecode lb,
      difficulty=rlpDecode d,
      number=rlpDecode number,
      gasLimit=rlpDecode gl,
      gasUsed=rlpDecode gu,
      timestamp=posixSecondsToUTCTime $ fromInteger $ rlpDecode ts,
      extraData=rlpDecode ed,
      mixHash=rlpDecode mh,
      nonce=fromInteger $ rlpDecode nonce
      }
  rlpDecode x = error $ "can not run rlpDecode on BlockHeader for value " ++ show x
