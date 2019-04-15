{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Noblit
(
  Error (..),
  latest,
  query,
  transact,
  run,
)
where

import Prelude hiding (Read)

import Database.Noblit.Query (Read, Write)
import Database.Noblit.Schema (TransactionId)

type Text = String -- For now.

data Noblit a

instance Functor Noblit where
  fmap = undefined

instance Applicative Noblit where
  pure = undefined
  _ <*> _ = undefined

instance Monad Noblit where
  _ >>= _ = undefined

newtype Error = Error Text

latest :: Noblit TransactionId
latest = undefined

query :: TransactionId -> Read a  -> Noblit [a]
query = undefined

transact :: TransactionId -> Write a -> Noblit ([a], TransactionId)
transact = undefined

run :: Noblit a -> IO (Either Error a)
run = undefined
