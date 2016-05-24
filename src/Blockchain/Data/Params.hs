{-# LANGUAGE OverloadedStrings #-}

module Blockchain.Data.Params where

import qualified Data.Text as T

toParam :: (T.Text,T.Text) -> Param
toParam a = Param a

fromParam :: Param -> (T.Text,T.Text)
fromParam (Param a) = a

data Param = Param (T.Text,T.Text)

instance Eq Param where
  Param a == Param b = fst a == fst b

instance Ord Param where
  (Param a) `compare` (Param b) = (fst a) `compare` (fst b)


