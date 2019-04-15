{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Noblit.Query
(
  Clause,
  Operation (..),
  Query (..),
  Read,
  Transaction (..),
  Write,
  datom,
  triplet,
)
where

import Prelude hiding (Read)

import Database.Noblit.Primitive (Value, EntityId, Variable)
import Database.Noblit.Schema (Attribute, TransactionId)

data Operation = Assert | Retract

data Clause a

instance Functor Clause where
  fmap = undefined

instance Applicative Clause where
  pure = undefined
  _ <*> _ = undefined

triplet
  :: Value u EntityId
  => Value v a
  => u
  -> Attribute a
  -> v
  -> Clause ()
triplet = undefined

datom
  :: Value u EntityId
  => Value v a
  => Value w TransactionId
  => u
  -> Attribute a
  -> v
  -> w
  -> Operation
  -> Clause ()
datom = undefined

class Query q where
  variable      :: q (Variable a)
  where_        :: Clause a   -> q a
  select        :: Variable a -> q a
  orderBy       :: Variable a -> q ()

class Transaction q where
  assert :: Clause a -> q a
  retract :: Clause a -> q a

data Read a
instance Functor Read where
  fmap = undefined

instance Applicative Read where
  pure = undefined
  _ <*> _ = undefined

instance Monad Read where
  _ >>= _ = undefined

instance Query Read where
  variable = undefined
  where_ = undefined
  select = undefined
  orderBy = undefined

data Write a

instance Functor Write where
  fmap = undefined

instance Applicative Write where
  pure = undefined
  _ <*> _ = undefined

instance Monad Write where
  _ >>= _ = undefined

instance Query Write where
  variable = undefined
  where_ = undefined
  select = undefined
  orderBy = undefined

instance Transaction Write where
  assert = undefined
  retract = undefined
