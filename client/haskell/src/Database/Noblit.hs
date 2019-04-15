{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Noblit
(
)
where

import Database.Noblit.Query (Clause, Write)
import Database.Noblit.Primitive (EntityId, Value)
import Database.Noblit.Schema (TransactionId)

type Text = String -- For now.

data Noblit a
instance Functor Noblit
instance Applicative Noblit
instance Monad Noblit

newtype Error = Error Text
latest    :: Noblit TransactionId
latest = undefined
query     :: TransactionId -> Read a  -> Noblit [a]
query = undefined
transact  :: TransactionId -> Write a -> Noblit ([a], TransactionId)
transact = undefined
runNoblit :: Noblit a -> IO (Either Error a)
runNoblit = undefined
