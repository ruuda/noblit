module Main where

import Data.ByteString.Strict (ByteString)

type Text = String -- For now.

newtype EntityId = EntityId Word64
newtype VariableId = VariableId Word32

type TransatctionId = EntityId
type AttributeId = EntityId
type TypeId = EntityId

data Variable :: * -> * where
  VariableBool     :: VariableId -> Variable Bool
  VariableRef      :: VariableId -> Variable EntityId
  VariableUint64   :: VariableId -> Variable Word64
  VariableBytes    :: VariableId -> Variable ByteString
  VariableString   :: VariableId -> Variable Text

data Constant :: * -> * where
  ConstantBool     :: Bool       -> Value Bool
  ConstantRef      :: EntityId   -> Value EntityId
  ConstantUint64   :: Word64     -> Value Word64
  ConstantBytes    :: ByteString -> Value BytesString
  ConstantString   :: Text       -> Value Text

data QueryValue :: * -> * where
  ValueVariable :: Variable a -> QueryValue a
  ValueConstant :: Constant a -> QueryValue a

class Value v a | v -> a where
  encode :: v -> QueryValue a

instance Value (Variable a) a        where encode v = ValueVariable v
instance Value Bool       Bool       where encode x = ValueConstant $ ConstantBool x
instance Value EntityId   EntityId   where encode x = ValueConstant $ ConstantRef x
instance Value Word64     Word64     where encode x = ValueConstant $ ConstantUint64 x
instance Value ByteString ByteString where encode x = ValueConstant $ ConstantBytes x
instance Value Text       Text       where encode x = ValueConstant $ ConstantString x

data Attribute :: * -> * where
  AttrBool   :: QueryValue EntityId -> Attribute Bool
  AttrRef    :: QueryValue EntityId -> Attribute EntityId
  AttrUint64 :: QueryValue EntityId -> Attribute Word64
  AttrBytes  :: QueryValue EntityId -> Attribute ByteString
  AttrString :: QueryValue EntityId -> Attribute Text

attributeEntityId :: Attribute a -> EntityId
attributeEntityId a = case a of
  AttrBool aid -> aid
  _ -> error "TODO"

data Datatype :: * -> * where
  TypeBool   :: QueryValue EntityId -> Datatype Bool
  TypeRef    :: QueryValue EntityId -> Datatype EntityId
  TypeUint64 :: QueryValue EntityId -> Datatype Word64
  TypeBytes  :: QueryValue EntityId -> Datatype ByteString
  TypeString :: QueryValue EntityId -> Datatype Text

typeEntityId :: Datatype a -> EntityId
typeEntityId a = case a of
  TypeBool tyid -> tyid
  _ -> error "TODO"

data Operation = Assert | Retract

data Clause a
instance Functor Clause
instance Applicative Clause
instance Semigroup a => Semigroup (Clause a)
instance Monoid a => Semigroup (Clause a)

triplet
  :: Value u
  => Value v
  -> u EntityId
  -> Attribute a
  -> v a
  -> Clause ()

datom
  :: Value u
  => Value v
  => Value w
  :: u EntityId
  -> Attribute a
  -> v a
  -> w TransactionId
  -> Operation
  -> Clause ()

dbTypeName        :: Attribute String
dbAttributeName   :: Attribute String
dbAttributeType   :: Attribute EntityId
dbAttributeUnique :: Attribute Uint64
dbAttributeMany   :: Attribute Uint64

attribute
  :: Value va
  => Value vn
  => Value vu
  => Value vm
  => va EntityId
  -> vn String
  -> Datatype a
  -> vu Bool
  -> vm Bool
  -> Clause (Attribute a)
attribute a name datatype unique many = do
  triplet a dbAttributeName name
  triplet a dbAttributeType (typeEntityId datatype)
  triplet a dbAttributeUnique unique
  triplet a dbAttributeMany many
  pure $ case datatype of
    TypeBool _ -> AttrBool a
    TypeRef _  -> AttrRef a

class Query q where
  variable      :: q (Variable a)
  where_        :: Clause a   -> q a
  select        :: Variable a -> q a
  orderBy       :: Variable a -> q ()

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

data Noblit a
instance Functor Noblit
instance Applicative Noblit
instance Monad Noblit

newtype Error = Error Text
latest    :: Noblit TransactionId
query     :: TransactionId -> Read a  -> Noblit [a]
transact  :: TransactionId -> Write a -> Noblit ([a], TransactionId)
runNoblit :: Noblit a -> IO (Either Error a)

-- Domain specific code below.

data Schema a = Schema
  { attrUserName      :: a Text
  , attrIssueTitle    :: a Text
  , attrIssueBody     :: a Text
  , attrIssuePriority :: a Word64
  , attrIssueAuthor   :: a EntityId
  , attrCommentBody   :: a Text
  , attrCommentAuthor :: a EntityId
  , attrIssueComment  :: a Ref
  }

-- Construct a query for the bug tracker database schema.
-- The function `f` controls the type of query. If we merely
-- want to retrieve the attribute ids of the attributes used
-- in the schema at startup time,
schema
  :: Query q
  => (Triplets -> q ())
  -> q (Schema Attribute)
schema f = do
  dbTypeString  <- variable
  dbTypeUint64  <- variable
  dbTypeRef     <- variable
  userName      <- variable
  issueTitle    <- variable
  issueBody     <- variable
  issueAuthor   <- variable
  commentBody   <- variable
  commentAuthor <- variable
  issueComment  <- variable

  where_ $ do
    triplet dbTypeString dbTypeName "string"
    triplet dbTypeUint64 dbTypeName "uint64"
    triplet dbTypeRef    dbTypeName "ref"

  let
    -- db.attribute.unique and db.attribute.many
    -- have boolean type. I could define ADTs and
    -- implement Value for them though.
    unique    = True
    nonUnique = False
    many      = True
    single    = False

  -- We might be asserting, or we might be selecting.
  f $ do
    attribute userName      dbTypeString unique    single
    attribute issueTitle    dbTypeString unique    single
    attribute issueBody     dbTypeString nonUnique single
    attribute issueAuthor   dbTypeRef    nonUnique single
    attribute commentBody   dbTypeString nonUnique single
    attribute commentAuthor dbTypeRef    nonUnique single
    attribute issueComment  dbTypeRef    nonUnique many

  Schema
    -- TODO: Turen selected entity ids into attributes.
    -- Can it be done in a more type-safe manner?
    <$> select userName
    <*> select issueTitle
    <*> select issueBody
    <*> select issueAuthor
    <*> select commentBody
    <*> select commentAuthor
    <*> select issueComment
