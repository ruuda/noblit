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

class Rhs where
instance Rhs Value where
instance Rhs Variable where

data Triplets a
instance Functor Triplet
instance Applicative Triplet

triplet :: Rhs v => Variable a -> Attribute a -> v a -> Triplets ()

data Query a
instance Functor Query
instance Applicative Query
instance Monad Query

variable :: Query (Variable a)

dbTypeName :: Attribute String
dbAttributeName :: Attribute String
dbAttributeType :: Attribute EntityId
dbAttributeUnique :: Attribute Uint64
dbAttributeMany :: Attribute Uint64

attribute
  :: Variable EntityId
  -> Value String
  -> Value EntityId
  -> Value Uint64
  -> Value Uint64
  -> Triplets ()
attribute a name datatype unique many = do
  triplet a dbAttributeName name
  triplet a dbAttributeType datatype
  triplet a dbAttributeUnique unique
  triplet a dbAttributeMany many

where_ :: Triplets a -> Query a
assert :: Triplets a -> Query a
retract :: Triplets a -> Query a

buildSchema :: Query ()
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
