{-# LANGUAGE DeriveFunctor #-}
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
import Database.Noblit.Schema (Attribute, TransactionId, attributeEntityId)

import qualified Database.Noblit.Primitive as Primitive

data Operation = Assert | Retract

data QueryTriplet = QueryTriplet String String String

data Clause a
  = Clause [QueryTriplet] a
  deriving (Functor)

instance Applicative Clause where
  pure x = Clause [] x
  (Clause xs f) <*> (Clause ys x) = Clause (xs ++ ys) (f x)

triplet
  :: Value u EntityId
  => Value v a
  => u
  -> Attribute a
  -> v
  -> Clause ()
triplet entity attribute value =
  let
    e = Primitive.encodeQueryValue $ Primitive.value entity
    a = Primitive.encodeQueryValue $ Primitive.value $ attributeEntityId attribute
    v = Primitive.encodeQueryValue $ Primitive.value value
  in
    Clause [QueryTriplet e a v] ()

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

  where_ (Clause xs x) =
    let
      showTriplet (QueryTriplet e a v) = "  " ++ e ++ " " ++ a ++ " " ++ v
    in
      Read $ do
        Rws.tell $ "where:" : fmap showTriplet xs
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
