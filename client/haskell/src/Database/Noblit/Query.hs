{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

import Control.Monad.Trans.RWS.CPS (RWS)
import Data.List (intercalate)
import Data.Word (Word32)
import Prelude hiding (Read)

import qualified Control.Monad.Trans.RWS.CPS as Rws

import Database.Noblit.Primitive (Blank, EntityId, Value, Variable, VariableId (..))
import Database.Noblit.Schema (Attribute, TransactionId)

import qualified Database.Noblit.Primitive as Primitive

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

class Monad q => Query q where
  variable      :: Blank a => q (Variable a)
  where_        :: Clause a   -> q a
  select        :: Variable a -> q a
  orderBy       :: Variable a -> q ()

class Transaction q where
  assert :: Clause a -> q a
  retract :: Clause a -> q a

-- We use an RWS monad to construct queries. The context is empty (for now), the
-- output is the query (string log for now), and the state is the counter to
-- supply fresh variables.
type Context = ()
type Output  = [String]
type State   = Word32

newtype Read a = Read (RWS Context Output State a)
  deriving (Functor, Applicative, Monad)

instance Show (Read a) where
  show (Read rws) =
    let
      (_, xs) = Rws.evalRWS rws () 0
    in
      intercalate "\n" xs

instance Query Read where
  variable = Read $ do
    i <- Rws.get
    Rws.put $ i + 1
    pure $ Primitive.variable $ VariableId i

  where_ _clause = undefined
  select _var = undefined
  orderBy _var = undefined

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
