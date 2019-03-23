module Main where

import Data.ByteString.Strict (ByteString)

type Text = String

newtype EntityId = EntityId Word64
newtype VariableId = VariableId Word64

data Datatype :: * -> * where
  Uint64 :: Datatype Word64
  Bytes  :: Datatype ByteString
  String :: Datatype Text
  Ref    :: Datatype EntityId

data Value :: * -> * where
  Uint64 :: Word64 -> Value Word64
  Bytes  :: ByteString -> Value BytesString
  String :: Text -> Value String
  Ref    :: EntityId -> Value EntityId

data Variable :: * -> * where
  Uint64 :: Variable Word64
  Bytes  :: Variable ByteString
  String :: Variable Text
  Ref    :: Variable EntityId

data Operation = Assert | Retract

class Rhs where
instance Rhs Value where
instance Rhs Variable where

data Triplets
instance Semigroup Triplets
instance Monoid Triplets

triplet
  :: Rhs value
  => Variable a
  -> Attribute a
  -> value a
  -> Triplets

data Datoms
instance Semigroup Datoms
instance Monoid Datoms

datom
  :: Rhs value
  => Variable a
  -> Attribute a
  -> value a
  -> Variable EntityId
  -> Operation
  -> Datoms

dbTypeName        :: Attribute String
dbAttributeName   :: Attribute String
dbAttributeType   :: Attribute EntityId
dbAttributeUnique :: Attribute Uint64
dbAttributeMany   :: Attribute Uint64

attribute
  :: Variable EntityId
  -> Value String
  -> Value EntityId
  -> Value Uint64
  -> Value Uint64
  -> Triplets
attribute a name datatype unique many = do
  triplet a dbAttributeName name
  triplet a dbAttributeType datatype
  triplet a dbAttributeUnique unique
  triplet a dbAttributeMany many

class Query q where
  variable      :: q (Variable a)
  where_        :: Triplets -> q ()
  whereHistoric :: Datoms -> q ()
  select        :: Variable a -> q a

data Read a
instance Functor     Read
instance Applicative Read
instance Monad       Read
instance Query       Read

data Write a
instance Functor     Write
instance Applicative Write
instance Monad       Write
instance Query       Write

assert  :: Triplets -> Write ()
retract :: Triplets -> Write ()

query    :: Database -> Read a  -> IO [a]
transact :: Database -> Write a -> IO ([a], Database)

buildSchema :: Write (EntityId, EntitiyId)
buildSchema = do
  string_t <- variable
  uint64_t <- variable
  ref_t    <- variable
  author_name <- variable
  issue_title <- variable
  issue_description <- variable

  where_ $ do
    dbTypeName string_t (String "string")
    dbTypeName uint64_t (String "uint64")
    dbTypeName ref_t (String "ref")

  assert $ do
    attribute author_name (String "author.name") string_t (Uint64 1) (Uint64 0)
    attribute issue_title (String "issue.title") string_t (Uint64 1) (Uint64 0)

  (,) <$> select author_name <*> select issue_title
