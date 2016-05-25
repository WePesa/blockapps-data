{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Blockchain.Data.Params where

import Control.Monad (foldM)

import qualified Database.Esqueleto as E

import qualified Data.Text as T

import Data.Set (fromList,toList,insert)

import Blockchain.ExtWord

import Numeric

toParam :: (T.Text,T.Text) -> Param
toParam a = Param a

fromParam :: Param -> (T.Text,T.Text)
fromParam (Param a) = a

data Param = Param (T.Text,T.Text)

instance Eq Param where
  Param a == Param b = fst a == fst b

instance Ord Param where
  (Param a) `compare` (Param b) = (fst a) `compare` (fst b)

sortToOrderBy :: (E.Esqueleto query expr backend, E.PersistField a)
              => Maybe T.Text -> expr (E.Value a) -> (expr E.OrderBy)
sortToOrderBy (Just "asc")  x  = E.asc  x
sortToOrderBy (Just "desc") x  = E.desc x
sortToOrderBy _             x  = E.asc  x

extractValue :: String -> [(T.Text, T.Text)] -> String -> Maybe String
extractValue name ts zero = Control.Monad.foldM toFold zero (map selectPage ts)
     where
       toFold :: String -> Maybe String -> Maybe String
       toFold n Nothing = Just n
       toFold n (Just m) = Just (maximum [n, m])
       selectPage :: (T.Text, T.Text) -> Maybe String
       selectPage (s, v) | T.unpack s == name = Just $ T.unpack v
                         | otherwise = Nothing

toId :: E.ToBackendKey E.SqlBackend record => T.Text -> E.Key record
toId v = E.toSqlKey (fromIntegral $ (read $ T.unpack v :: Integer) )

fromHexText :: T.Text -> Word256
fromHexText v = res
  where ((res,_):_) = readHex $ T.unpack $ v :: [(Word256,String)]

extractPage :: String -> [(T.Text, T.Text)] ->  Maybe Integer
extractPage name ts = foldM toFold 0 (map selectPage ts)
     where
       toFold :: Integer -> Maybe Integer -> Maybe Integer
       toFold n Nothing = Just n
       toFold n (Just m) = Just (maximum [n, m])
       selectPage :: (T.Text, T.Text) -> Maybe Integer
       selectPage (s, v) | T.unpack s == name = Just (read $ T.unpack v :: Integer)
                         | otherwise = Nothing

if' :: Bool -> a -> b -> Either a b
if' x a b = if x == True then Left a else Right b

appendIndex :: [(T.Text, T.Text)] -> [(T.Text,T.Text)] -- this sould be using URL encoding code from Yesod        
appendIndex l = map fromParam (Data.Set.toList $ Data.Set.insert (toParam ("index", "")) $ Data.Set.fromList $ map
 toParam l)

extraFilter :: (T.Text,T.Text) -> T.Text -> (T.Text,T.Text)
extraFilter ("index", _) v' = ("index", v')
extraFilter (a,b) _        = (a,b)
