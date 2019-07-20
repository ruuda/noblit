{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

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

import Database.Noblit.Primitive (Blank, EntityId, QueryValue, Value, Variable, VariableId (..))
import Database.Noblit.Schema (Attribute, AttributeId, TransactionId, attributeEntityId)

import qualified Database.Noblit.Primitive as Primitive

data Operation = Assert | Retract

data Triplet = forall a. Triplet (QueryValue EntityId) (QueryValue AttributeId) (QueryValue a)

encodeTriplet :: Triplet -> String
encodeTriplet (Triplet e a v) =
  let
    e' = Primitive.encodeQueryValue e
    a' = Primitive.encodeQueryValue a
    v' = Primitive.encodeQueryValue v
  in
    "  " <> e' <> " " <> a' <> " " <> v'

data ClauseData = ClauseData
  { clauseWhere :: [Triplet]
  }

instance Semigroup ClauseData where
  xs <> ys = ClauseData
    { clauseWhere = clauseWhere xs <> clauseWhere ys
    }

instance Monoid ClauseData where
  mempty = ClauseData
    { clauseWhere = []
    }

data Clause a
  = Clause ClauseData a
  deriving (Functor)

instance Applicative Clause where
  pure x = Clause mempty x
  (Clause xs f) <*> (Clause ys x) = Clause (xs <> ys) (f x)

triplet
  :: Value u EntityId
  => Value v a
  => u
  -> Attribute a
  -> v
  -> Clause ()
triplet entity attribute value =
  let
    e = Primitive.value entity
    a = Primitive.value $ attributeEntityId attribute
    v = Primitive.value value
    clauseData = mempty { clauseWhere = [Triplet e a v] }
  in
    Clause clauseData ()

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
  variable :: Blank a    => q (Variable a)
  where_   :: Clause a   -> q a
  select   :: Variable a -> q a
  orderBy  :: Variable a -> q ()

class Transaction q where
  assert :: Clause a -> q a
  retract :: Clause a -> q a

-- We use an RWS monad to construct queries. The context is empty (for now), the
-- output is the query (string log for now), and the state is the counter to
-- supply fresh variables.
type Context = ()
type Output  = ClauseData
type State   = Word32

newtype Read a = Read (RWS Context Output State a)
  deriving (Functor, Applicative, Monad)

instance Show (Read a) where
  show (Read rws) =
    let
      (_, clauseData) = Rws.evalRWS rws () 0
      wheres = clauseWhere clauseData
    in
      "where:\n" <> (intercalate "\n" $ fmap encodeTriplet wheres)

instance Query Read where
  variable = Read $ do
    i <- Rws.get
    Rws.put $ i + 1
    pure $ Primitive.variable $ VariableId i

  where_ (Clause clauseData x) = Read $ do
    Rws.tell clauseData
    pure x

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
