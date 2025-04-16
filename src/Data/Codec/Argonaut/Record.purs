module Data.Codec.Argonaut.Record
  ( OptionalWith
  , class RowListCodec
  , object
  , optional
  , optionalWith
  , record
  , rowListCodec
  )
  where

import Data.Codec.Argonaut as CA
import Data.Function (identity)
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol)
import Prim.Row as R
import Prim.RowList as RL
import Record as Rec
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Constructs a `JsonCodec` for a `Record` from a name and a record of codecs.
-- | The name is used in the error message produced when decoding fails.
-- |
-- | ```purescript
-- | type Person = { name ∷ String, age ∷ Int }
-- |
-- | personCodec ∷ CA.JsonCodec Person
-- | personCodec = CAR.object "Person" { name: CA.string, age: CA.int }
-- | ```
object
  ∷ ∀ ri ro rl
  . RL.RowToList ri rl
  ⇒ RowListCodec rl ri ro
  ⇒ String
  → Record ri
  → CA.JsonCodec (Record ro)
object name rec = CA.object name (record rec)

-- | Constructs a `JPropCodec` for a `Record` from a record of codecs. Commonly
-- | the `object` function in this module will be the preferred choice, as that
-- | produces a `JsonCodec` instead.
record
  ∷ ∀ ri ro rl
  . RL.RowToList ri rl
  ⇒ RowListCodec rl ri ro
  ⇒ Record ri
  → CA.JPropCodec (Record ro)
record = rowListCodec (Proxy ∷ Proxy rl)


newtype OptionalWith a b = OptionalWith
  { normalize ∷ Maybe a → b
  , denormalize ∷ b → Maybe a
  , codec ∷ CA.JsonCodec a
  }

-- | Used to wrap codec values provided in `record` to indicate the field is optional.
-- |
-- | This will only decode the property as `Nothing` if the field does not exist
-- | in the object - having a values such as `null` assigned will need handling
-- | separately.
-- |
-- | The property will be omitted when encoding and the value is `Nothing`.
optional ∷ ∀ a. CA.JsonCodec a → OptionalWith a (Maybe a)
optional = optionalWith identity identity

-- | Like `Optional`, but more general. It allows you to provide a function to transform the
-- | `Maybe a` value into a different type `b`. This is useful when you want to
-- | provide a default value or perform some other transformation when the
-- | property is not present in the JSON object.
optionalWith ∷ ∀ a b. (Maybe a → b) → (b → Maybe a) → CA.JsonCodec a → OptionalWith a b
optionalWith normalize denormalize codec = OptionalWith { normalize, denormalize, codec }

-- | The class used to enable the building of `Record` codecs by providing a
-- | record of codecs.
class RowListCodec (rl ∷ RL.RowList Type) (ri ∷ Row Type) (ro ∷ Row Type) | rl → ri ro where
  rowListCodec ∷ ∀ proxy. proxy rl → Record ri → CA.JPropCodec (Record ro)

instance rowListCodecNil ∷ RowListCodec RL.Nil () () where
  rowListCodec _ _ = CA.record

instance rowListCodecConsOptionalWith ∷
  ( RowListCodec rs ri' ro'
  , R.Cons sym (OptionalWith a b) ri' ri
  , R.Cons sym b ro' ro
  , R.Lacks sym ro'
  , R.Lacks sym ri'
  , IsSymbol sym
  ) ⇒
  RowListCodec (RL.Cons sym (OptionalWith a b) rs) ri ro where
  rowListCodec _ codecs =
    CA.recordPropOptionalWith ret.normalize ret.denormalize (Proxy ∷ Proxy sym) ret.codec tail

    where
    ret ∷ { normalize ∷ Maybe a → b, denormalize ∷ b → Maybe a, codec ∷ CA.JsonCodec a }
    ret = coerce (Rec.get (Proxy ∷ Proxy sym) codecs ∷ OptionalWith a b)

    tail ∷ CA.JPropCodec (Record ro')
    tail = rowListCodec (Proxy ∷ Proxy rs) ((unsafeCoerce ∷ Record ri → Record ri') codecs)

else instance rowListCodecCons ∷
  ( RowListCodec rs ri' ro'
  , R.Cons sym (CA.JsonCodec a) ri' ri
  , R.Cons sym a ro' ro
  , IsSymbol sym
  ) ⇒
  RowListCodec (RL.Cons sym (CA.JsonCodec a) rs) ri ro where
  rowListCodec _ codecs =
    CA.recordProp (Proxy ∷ Proxy sym) codec tail
    where
    codec ∷ CA.JsonCodec a
    codec = Rec.get (Proxy ∷ Proxy sym) codecs

    tail ∷ CA.JPropCodec (Record ro')
    tail = rowListCodec (Proxy ∷ Proxy rs) ((unsafeCoerce ∷ Record ri → Record ri') codecs)
