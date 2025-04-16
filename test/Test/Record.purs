module Test.Record where

import Prelude

import Control.Monad.Gen as Gen
import Control.Monad.Gen.Common as GenC
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Core as Json
import Data.Codec.Argonaut.Common as CA
import Data.Codec.Argonaut.Common as Car
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (dimap)
import Data.String.Gen (genAsciiString)
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object as Object
import Test.QuickCheck (assertEquals, quickCheck, quickCheckGen)
import Test.QuickCheck.Gen (Gen)
import Test.Util (genInt, propCodec)

type OuterR =
  { a ∷ Int
  , b ∷ String
  , c ∷ Maybe InnerR
  }

type InnerR =
  { n ∷ Int
  , m ∷ Boolean
  , o ∷ Maybe Boolean
  }

type Sample =
  { p ∷ Int
  , q ∷ Boolean
  }

newtype Outer = Outer OuterR

derive instance newtypeOuter ∷ Newtype Outer _

instance showOuter ∷ Show Outer where
  show (Outer r) = "Outer " <> stringify (CA.encode outerCodec r)

instance eqOuter ∷ Eq Outer where
  eq (Outer o1) (Outer o2) =
    o1.a == o2.a
      && o1.b == o2.b
      && case o1.c, o2.c of
        Nothing, Nothing → true
        Just i1, Just i2 → i1.n == i2.n && i1.m == i2.m
        _, _ → false

outerCodec ∷ CA.JsonCodec OuterR
outerCodec =
  CA.object "Outer" $ CAR.record
    { a: CA.int
    , b: CA.string
    , c: CA.maybe innerCodec
    }

innerCodec ∷ CA.JsonCodec InnerR
innerCodec =
  CA.object "Inner" $ CAR.record
    { n: CA.int
    , m: Car.boolean
    , o: CAR.optional CA.boolean
    }

sampleCodec ∷ CA.JsonCodec Sample
sampleCodec =
  CA.object "Sample" $ CAR.record
    { p: CA.int
    , q: CAR.optionalWith (fromMaybe false) (if _ then Just true else Nothing) CA.boolean
    }

genOuter ∷ Gen OuterR
genOuter = do
  a ← genInt
  b ← genAsciiString
  c ← GenC.genMaybe genInner
  pure { a, b, c }

genInner ∷ Gen InnerR
genInner = do
  n ← genInt
  m ← Gen.chooseBool
  o ← GenC.genMaybe Gen.chooseBool
  pure { n, m, o }

genSample ∷ Gen Sample
genSample = do
  p ← genInt
  q ← Gen.chooseBool
  pure { p, q }

main ∷ Effect Unit
main = do
  log "Checking record codec"
  quickCheck $ propCodec (Outer <$> genOuter) (dimap unwrap wrap outerCodec)

  log "Check optional Nothing is missing from json"
  quickCheckGen do
    v ← genInner
    let obj = Json.toObject $ CA.encode innerCodec (v { o = Nothing })
    pure $ assertEquals (Just [ "m", "n" ]) (Object.keys <$> obj)

  log "Check optional Just is present in the json"
  quickCheckGen do
    b ← Gen.chooseBool
    v ← genInner
    let obj = Json.toObject $ CA.encode innerCodec (v { o = Just b })
    pure $ assertEquals (Just [ "m", "n", "o" ]) (Object.keys <$> obj)

  log "Check `false` is not present in the json"
  quickCheckGen do
    v ← genSample
    let obj = Json.toObject $ CA.encode sampleCodec (v { q = false })
    pure $ assertEquals (Just [ "p" ]) (Object.keys <$> obj)

  log "Check `true` is present in the json"
  quickCheckGen do
    v ← genSample
    let obj = Json.toObject $ CA.encode sampleCodec (v { q = true })
    pure $ assertEquals (Just [ "p", "q" ]) (Object.keys <$> obj)

  pure unit
