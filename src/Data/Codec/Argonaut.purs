module Data.Codec.Argonaut
  ( FieldResult(..)
  , JIndexedCodec
  , JPropCodec
  , JsonCodec
  , JsonCodecPerField
  , JsonDecodeError(..)
  , PropOpts(..)
  , array
  , boolean
  , char
  , codePoint
  , coercible
  , defaultPropOpts
  , fix
  , index
  , indexedArray
  , int
  , jarray
  , jobject
  , json
  , mod
  , modMandatory
  , modNormalize
  , modOptional
  , modWithDefault
  , modWithDefaultSparse
  , module Codec
  , named
  , null
  , number
  , object
  , printJsonDecodeError
  , prismaticCodec
  , prop
  , record
  , recordProp
  , recordPropOptional
  , recordPropWithOpts
  , string
  , void
  )
  where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Array as A
import Data.Bifunctor (bimap, lmap)
import Data.Bifunctor as BF
import Data.Codec (Codec(..), Codec')
import Data.Codec (Codec(..), Codec', codec, codec', decode, encode, hoist, identity, (<~<), (>~>), (~)) as Codec
import Data.Codec as C
import Data.Either (Either(..), note)
import Data.Either as Either
import Data.Generic.Rep (class Generic)
import Data.Int as I
import Data.List (List, (:))
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Foreign.Object as FO
import Prim.Coerce (class Coercible)
import Prim.Row as Row
import Record as Record
import Safe.Coerce (coerce)
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

-- | Codec type for `Json` values.
type JsonCodec a = Codec' (Either JsonDecodeError) J.Json a

-- | Error type for failures while decoding.
data JsonDecodeError
  = TypeMismatch String
  | UnexpectedValue J.Json
  | AtIndex Int JsonDecodeError
  | AtKey String JsonDecodeError
  | Named String JsonDecodeError
  | MissingValue

derive instance eqJsonDecodeError ∷ Eq JsonDecodeError
derive instance ordJsonDecodeError ∷ Ord JsonDecodeError
derive instance genericJsonDecodeError ∷ Generic JsonDecodeError _

instance showJsonDecodeError ∷ Show JsonDecodeError where
  show = case _ of
    TypeMismatch s → "(TypeMismatch " <> show s <> ")"
    UnexpectedValue j → "(UnexpectedValue " <> J.stringify j <> ")"
    AtIndex i e → "(AtIndex " <> show i <> " " <> show e <> ")"
    AtKey k e → "(AtKey " <> show k <> " " <> show e <> ")"
    Named s e → "(Named " <> show s <> " " <> show e <> ")"
    MissingValue → "MissingValue"

-- | Prints a `JsonDecodeError` as a somewhat readable error message.
printJsonDecodeError ∷ JsonDecodeError → String
printJsonDecodeError err =
  "An error occurred while decoding a JSON value:\n" <> go err
  where
  go = case _ of
    TypeMismatch ty → "  Expected value of type '" <> ty <> "'."
    UnexpectedValue val → "  Unexpected value " <> J.stringify val <> "."
    AtIndex ix inner → "  At array index " <> show ix <> ":\n" <> go inner
    AtKey key inner → "  At object key " <> key <> ":\n" <> go inner
    Named name inner → "  Under '" <> name <> "':\n" <> go inner
    MissingValue → "  No value was found."

-- | The "identity codec" for `Json` values.
json ∷ JsonCodec J.Json
json = Codec.codec' pure identity

-- | A codec for `null` values in `Json`.
null ∷ JsonCodec Unit
null = jsonPrimCodec "Null" J.toNull (const J.jsonNull)

-- | A codec for `Boolean` values in `Json`.
boolean ∷ JsonCodec Boolean
boolean = jsonPrimCodec "Boolean" J.toBoolean J.fromBoolean

-- | A codec for `Number` values in `Json`.
number ∷ JsonCodec Number
number = jsonPrimCodec "Number" J.toNumber J.fromNumber

-- | A codec for `Int` values in `Json`.
int ∷ JsonCodec Int
int = jsonPrimCodec "Int" (I.fromNumber <=< J.toNumber) (J.fromNumber <<< I.toNumber)

-- | A codec for `String` values in `Json`.
string ∷ JsonCodec String
string = jsonPrimCodec "String" J.toString J.fromString

-- | A codec for `Codepoint` values in `Json`.
codePoint ∷ JsonCodec S.CodePoint
codePoint = jsonPrimCodec "CodePoint" (S.codePointAt 0 <=< J.toString) (J.fromString <<< S.singleton)

-- | A codec for `Char` values in `Json`.
char ∷ JsonCodec Char
char = jsonPrimCodec "Char" (SCU.toChar <=< J.toString) (J.fromString <<< SCU.singleton)

-- | A codec for `Void` values.
void ∷ JsonCodec Void
void = jsonPrimCodec "Void" (const Nothing) absurd

-- | A codec for `Array Json` values in `Json`. This does not decode the values
-- | of the array, for that use `array` for a general array decoder, or
-- | `indexedArray` with `index` to decode fixed length array encodings.
jarray ∷ JsonCodec (Array J.Json)
jarray = jsonPrimCodec "Array" J.toArray J.fromArray

-- | A codec for `JObject` values in `Json`.
jobject ∷ JsonCodec (FO.Object J.Json)
jobject = jsonPrimCodec "Object" J.toObject J.fromObject

-- | A codec for arbitrary length `Array`s where every item in the array
-- | shares the same type.
-- |
-- | ``` purescript
-- | import Data.Codec.Argonaut as CA
-- |
-- | codecIntArray ∷ CA.JsonCodec (Array Int)
-- | codecIntArray = CA.array CA.int
-- | ```
array ∷ ∀ a. JsonCodec a → JsonCodec (Array a)
array codec =
  Codec.codec'
    (\j → traverseWithIndex (\ix j' → BF.lmap (AtIndex ix) (Codec.decode codec j')) =<< Codec.decode jarray j)
    (\a → J.fromArray (map (Codec.encode codec) a))

-- | Codec type for specifically indexed `JArray` elements.
type JIndexedCodec a =
  Codec
    (Either JsonDecodeError)
    (Array J.Json)
    (L.List J.Json)
    a
    a

-- | A codec for types that are encoded as an array with a specific layout.
-- |
-- | For example, if we'd like to encode a `Person` as a 2-element array, like
-- | `["Rashida", 37]`, we could write the following codec:
-- |
-- | ```purescript
-- | import Data.Codec ((~))
-- | import Data.Codec.Argonaut as CA
-- |
-- | type Person = { name ∷ String, age ∷ Int }
-- |
-- | codecPerson ∷ CA.JsonCodec Person
-- | codecPerson = CA.indexedArray "Test Object" $
-- |   { name: _, age: _ }
-- |     <$> _.name ~ CA.index 0 CA.string
-- |     <*> _.age ~ CA.index 1 CA.int
-- | ```
indexedArray ∷ ∀ a. String → JIndexedCodec a → JsonCodec a
indexedArray name codec =
  Codec.codec'
    (\j → lmap (Named name) (Codec.decode codec =<< Codec.decode jarray j))
    (\a → Codec.encode jarray (A.fromFoldable (Codec.encode codec a)))

-- | A codec for an item in an `indexedArray`.
index ∷ ∀ a. Int → JsonCodec a → JIndexedCodec a
index ix codec =
  Codec.codec
    (\xs → BF.lmap (AtIndex ix) (maybe (Left MissingValue) (Codec.decode codec) (A.index xs ix)))
    (pure <<< Codec.encode codec)

-- | Codec type for `JObject` prop/value pairs.
type JPropCodec a =
  Codec
    (Either JsonDecodeError)
    (FO.Object J.Json)
    (L.List (Tuple String J.Json))
    a
    a

-- | A codec for objects that are encoded with specific properties.
-- |
-- | See also `Data.Codec.Argonaut.Record.object` for a more commonly useful
-- | version of this function.
object ∷ ∀ a. String → JPropCodec a → JsonCodec a
object name codec =
  Codec.codec'
    (\j → lmap (Named name) (Codec.decode codec =<< Codec.decode jobject j))
    (\a → Codec.encode jobject (FO.fromFoldable (Codec.encode codec a)))

-- | A codec for a property of an object.
prop ∷ ∀ a. String → JsonCodec a → JPropCodec a
prop key codec =
  Codec.codec
    (\obj → BF.lmap (AtKey key) (maybe (Left MissingValue) (Codec.decode codec) (FO.lookup key obj)))
    (pure <<< Tuple key <<< Codec.encode codec)

-- | The starting value for a object-record codec. Used with `recordProp` it
-- | provides a convenient method for defining codecs for record types that
-- | encode into JSON objects of the same shape.
-- |
-- | For example, to encode a record as the JSON object
-- | `{ "name": "Karl", "age": 25 }` we would define a codec like this:
-- | ```
-- | import Data.Codec.Argonaut as CA
-- | import Type.Proxy (Proxy(..))
-- |
-- | type Person = { name ∷ String, age ∷ Int }
-- |
-- | codecPerson ∷ CA.JsonCodec Person
-- | codecPerson =
-- |   CA.object "Person" $ CA.record
-- |     # CA.recordProp (Proxy :: _ "name") CA.string
-- |     # CA.recordProp (Proxy :: _ "age") CA.int
-- | ```
-- |
-- | See also `Data.Codec.Argonaut.Record.object` for a more commonly useful
-- | version of this function.
record ∷ JPropCodec {}
record = Codec (const (pure {})) pure

-- | Used with `record` to define codecs for record types that encode into JSON
-- | objects of the same shape. See the comment on `record` for an example.
recordProp
  ∷ ∀ p a r r'
  . IsSymbol p
  ⇒ Row.Cons p a r r'
  ⇒ Row.Lacks p r
  ⇒ Proxy p
  → JsonCodec a
  → JPropCodec (Record r)
  → JPropCodec (Record r')
recordProp p codec = recordPropWithOpts
  { mapLabel: identity
  }
  p
  (modMandatory codec)

-- | Used with `record` to define an optional field.
-- |
-- | This will only decode the property as `Nothing` if the field does not exist
-- | in the object - having a values such as `null` assigned will need handling
-- | separately.
-- |
-- | The property will be omitted when encoding and the value is `Nothing`.
recordPropOptional
  ∷ ∀ p a r r'
  . IsSymbol p
  ⇒ Row.Cons p (Maybe a) r r'
  ⇒ Row.Lacks p r
  ⇒ Proxy p
  → JsonCodec a
  → JPropCodec (Record r)
  → JPropCodec (Record r')
recordPropOptional p codec = recordPropWithOpts
  { mapLabel: identity
  }
  p
  (modOptional codec)

type JsonCodecPerField a = Codec' (Either JsonDecodeError) (FieldResult Json) a

-- | Used with `record` to define a field with further options.
recordPropWithOpts
  ∷ ∀ p a r r'
  . IsSymbol p
  ⇒ Row.Cons p a r r'
  ⇒ Row.Lacks p r
  ⇒ PropOpts
  → Proxy p
  → JsonCodecPerField a
  → JPropCodec (Record r)
  → JPropCodec (Record r')
recordPropWithOpts { mapLabel } p codecA codecR = Codec.codec dec' enc'
  where
  key ∷ String
  key = mapLabel $ reflectSymbol p

  dec' ∷ FO.Object J.Json → Either JsonDecodeError (Record r')
  dec' obj = do
    r ← Codec.decode codecR obj ∷ _ (Record r)
    b ←
      case FO.lookup key obj of
        Just j →
          C.decode codecA (FieldPresent j)

        Nothing →
          BF.lmap (AtKey key) $
            C.decode codecA FieldMissing ∷ _ a

    pure $ Record.insert p b r

  enc' ∷ Record r' → L.List (Tuple String J.Json)
  enc' val = do
    let
      w ∷ List (Tuple String Json)
      w = Codec.encode codecR (unsafeForget val)

      v ∷ a
      v = Record.get p val

    case C.encode codecA v of
      FieldPresent a → Tuple key a : w
      FieldMissing → w

  unsafeForget ∷ Record r' → Record r
  unsafeForget = unsafeCoerce

jsonPrimCodec ∷ ∀ a. String → (J.Json → Maybe a) → (a → J.Json) → JsonCodec a
jsonPrimCodec ty f = Codec.codec' (maybe (Left (TypeMismatch ty)) pure <<< f)

-- | Helper function for defining recursive codecs in situations where the codec
-- | definition causes a _"The value of <codec> is undefined here"_ error.
-- |
-- | ```purescript
-- | import Data.Codec.Argonaut as CA
-- | import Data.Codec.Argonaut.Common as CAC
-- | import Data.Codec.Argonaut.Record as CAR
-- | import Data.Maybe (Maybe)
-- | import Data.Newtype (class Newtype)
-- | import Data.Profunctor (wrapIso)
-- |
-- | newtype IntList = IntList { cell ∷ Int, rest ∷ Maybe IntList }
-- |
-- | derive instance newtypeLoopyList ∷ Newtype IntList _
-- |
-- | codecIntList ∷ CA.JsonCodec IntList
-- | codecIntList =
-- |   CA.fix \codec →
-- |     wrapIso IntList $
-- |       CAR.object "IntList" { cell: CA.int, rest: CAC.maybe codec }
-- | ```
fix ∷ ∀ a. (JsonCodec a → JsonCodec a) → JsonCodec a
fix f =
  Codec.codec'
    (\x → Codec.decode (f (fix f)) x)
    (\x → Codec.encode (f (fix f)) x)

-- | A codec for introducing names into error messages - useful when definiting a codec for a type
-- | synonym for a record, for instance.
named ∷ ∀ a. String → JsonCodec a → JsonCodec a
named name codec =
  Codec.codec'
    (lmap (Named name) <<< Codec.decode codec)
    (Codec.encode codec)

-- | A codec for types that can be safely coerced.
-- |
-- | Accepts the name of the target type as an argument to improve error messaging when the inner
-- | codec fails.
coercible ∷ ∀ a b. Coercible a b ⇒ String → JsonCodec a → JsonCodec b
coercible name codec =
  Codec.codec'
    (bimap (Named name) coerce <<< Codec.decode codec)
    (coerce (Codec.encode codec))

-- | Adapts an existing codec with a pair of functions to allow a value to be
-- | further refined. If the inner decoder fails an `UnexpectedValue` error will
-- | be raised for JSON input.
-- |
-- | This function is named as such as the pair of functions it accepts
-- | correspond with the `preview` and `review` functions of a `Prism`-style lens.
-- |
-- | An example of this would be a codec for `Data.String.NonEmpty.NonEmptyString`:
-- |
-- | ```purescript
-- | nonEmptyString ∷ CA.JsonCodec NES.NonEmptyString
-- | nonEmptyString = CA.prismaticCodec "NonEmptyString" NES.fromString NES.toString CA.string
-- | ```
-- |
-- | Another example might be to handle a mapping from a small sum type to
-- | strings:
-- |
-- | ```purescript
-- | data Direction = North | South | West | East
-- |
-- | directionCodec :: JsonCodec Direction
-- | directionCodec = CA.prismaticCodec "Direction" dec enc string
-- |   where
-- |     dec = case _ of
-- |       "N" -> Just North
-- |       "S" -> Just South
-- |       "W" -> Just West
-- |       "E" -> Just East
-- |       _ -> Nothing
-- |
-- |     enc = case _ of
-- |       North -> "N"
-- |       South -> "S"
-- |       West -> "W"
-- |       East -> "E"
-- | ```
-- |
-- | Although for this latter case there are some other options too, in the form
-- | of `Data.Codec.Argonaut.Generic.nullarySum` and `Data.Codec.Argonaut.Sum.enumSum`.
prismaticCodec ∷ ∀ a b. String → (a → Maybe b) → (b → a) → JsonCodec a → JsonCodec b
prismaticCodec name f g codec =
  Codec.codec'
    (\j → note (Named name (UnexpectedValue j)) <<< f =<< Codec.decode codec j)
    (Codec.encode codec <<< g)

data FieldResult a
  = FieldMissing
  | FieldPresent a

fieldResultToMaybe ∷ ∀ a. FieldResult a → Maybe a
fieldResultToMaybe = case _ of
  FieldPresent a → Just a
  FieldMissing → Nothing

fieldResultFromMaybe ∷ ∀ a. Maybe a → FieldResult a
fieldResultFromMaybe = case _ of
  Just a → FieldPresent a
  Nothing → FieldMissing

derive instance Functor FieldResult

type PropOpts =
  { mapLabel ∷ String → String
  }

defaultPropOpts ∷ PropOpts
defaultPropOpts =
  { mapLabel: identity
  }

modMandatory ∷ ∀ a. JsonCodec a → JsonCodecPerField a
modMandatory = mod
  (fieldResultToMaybe >>> Either.note MissingValue)
  FieldPresent

modOptional ∷ ∀ a. JsonCodec a → JsonCodecPerField (Maybe a)
modOptional = mod
  (fieldResultToMaybe >>> Right)
  fieldResultFromMaybe

modWithDefault ∷ ∀ a. a → JsonCodec a → JsonCodecPerField a
modWithDefault def = mod
  (fieldResultToMaybe >>> maybe (Right def) Right)
  (\val → FieldPresent val)

modNormalize ∷ ∀ a b. (FieldResult a → b) → (b → FieldResult a) → JsonCodec a → JsonCodecPerField b
modNormalize f g = mod
  (f >>> Right)
  g

modWithDefaultSparse ∷ ∀ a. a → (a → Boolean) → JsonCodec a → JsonCodecPerField a
modWithDefaultSparse def isNull = mod
  (fieldResultToMaybe >>> maybe (Right def) Right)
  (\val → if isNull val then FieldMissing else FieldPresent val)

mod ∷ ∀ a b. (FieldResult a → Either JsonDecodeError b) → (b → FieldResult a) → JsonCodec a → JsonCodecPerField b
mod f g codec = C.codec' dec enc
  where
  dec ∷ FieldResult Json → Either JsonDecodeError b
  dec = case _ of
    FieldPresent j → do
      ret ← C.decode codec j ∷ _ a
      f (FieldPresent ret) ∷ _ b

    FieldMissing → f FieldMissing

  enc ∷ b → FieldResult Json
  enc = g >>> map (C.encode codec)

