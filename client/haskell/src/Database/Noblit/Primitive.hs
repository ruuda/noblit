{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}

module Database.Noblit.Primitive
(
  Constant (..),
  EntityId (..),
  Blank (..),
  Value (..),
  Variable (..),
  VariableId (..),
  QueryValue (..),
  encodeQueryValue,
)
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word32, Word64)

newtype EntityId = EntityId Word64
newtype VariableId = VariableId Word32

data Variable :: * -> * where
  VariableBool     :: VariableId -> Variable Bool
  VariableRef      :: VariableId -> Variable EntityId
  VariableUint64   :: VariableId -> Variable Word64
  VariableBytes    :: VariableId -> Variable ByteString
  VariableString   :: VariableId -> Variable Text

data Constant :: * -> * where
  ConstantBool     :: Bool       -> Constant Bool
  ConstantRef      :: EntityId   -> Constant EntityId
  ConstantUint64   :: Word64     -> Constant Word64
  ConstantBytes    :: ByteString -> Constant ByteString
  ConstantString   :: Text       -> Constant Text

data QueryValue :: * -> * where
  ValueVariable :: Variable a -> QueryValue a
  ValueConstant :: Constant a -> QueryValue a

class Blank a where
  variable :: VariableId -> Variable a

class Value v a | v -> a where
  value :: v -> QueryValue a

variableVariableId :: Variable a -> VariableId
variableVariableId var = case var of
  VariableBool   i -> i
  VariableRef    i -> i
  VariableUint64 i -> i
  VariableBytes  i -> i
  VariableString i -> i

instance Blank Bool       where variable = VariableBool
instance Blank EntityId   where variable = VariableRef
instance Blank Word64     where variable = VariableUint64
instance Blank ByteString where variable = VariableBytes
instance Blank Text       where variable = VariableString

instance Value (QueryValue a) a          where value = id
instance Value (Variable a)   a          where value v = ValueVariable v
instance Value Bool           Bool       where value x = ValueConstant $ ConstantBool x
instance Value EntityId       EntityId   where value x = ValueConstant $ ConstantRef x
instance Value Word64         Word64     where value x = ValueConstant $ ConstantUint64 x
instance Value ByteString     ByteString where value x = ValueConstant $ ConstantBytes x
instance Value Text           Text       where value x = ValueConstant $ ConstantString x

encodeConstant :: Constant a -> String
encodeConstant = \case
  ConstantBool x           -> show x
  ConstantRef (EntityId x) -> "# " ++ show x
  ConstantUint64 x         -> show x
  ConstantBytes x          -> show x
  ConstantString x         -> show x

encodeVariable :: Variable a -> String
encodeVariable var =
  let
    VariableId i = variableVariableId var
  in
    "$" ++ show i

-- TODO: Encode to wire format.
encodeQueryValue :: QueryValue a -> String
encodeQueryValue = \case
  ValueVariable var -> encodeVariable var
  ValueConstant cst -> encodeConstant cst
