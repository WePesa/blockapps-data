{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

module Blockchain.Data.StatusClass (
  Status(..)
  ) where

{- Statuses need to identify their owner with a label,
   furnish us with a status message, and tell us whether
   they are up and running.                              -}

class Status a where
  isOperational :: a -> Bool
  message :: a -> String
  label :: a -> String

  

       
