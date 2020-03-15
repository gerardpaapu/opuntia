module Main where

import Prelude

import Control.Monad.State (State, evalState, get, put)
import Data.Array.NonEmpty (NonEmptyArray, (!!))
import Data.Array.NonEmpty as NonEmpty
import Data.Char (fromCharCode)
import Data.Char.Unicode (isAlpha, isLatin1)
import Data.Either (Either(..))
import Data.Int (rem)
import Data.Maybe (Maybe(..), fromJust)
import Data.String (CodePoint, Pattern(..))
import Data.String.CodePoints as String
import Data.String.CodeUnits as CodeUnits
import Data.Traversable (traverse)
import Data.Unfoldable (range, replicate)
import Effect (Effect)
import Effect.Console (log)
import FormatJS.IntlMessageFormat as Intl
import Partial.Unsafe (unsafePartial)
import Random.LCG as LCG


example :: String
example = """{gender_of_host, select, 
  female {
    {num_guests, plural, offset:1 
      =0 {{host} does not give a party.}
      =1 {{host} invites {guest} to her party.}
      =2 {{host} invites {guest} and one other person to her party.}
      other {{host} invites {guest} and # other people to her party.}}}
  male {
    {num_guests, plural, offset:1 
      =0 {{host} does not give a party.}
      =1 {{host} invites {guest} to his party.}
      =2 {{host} invites {guest} and one other person to his party.}
      other {{host} invites {guest} and # other people to his party.}}}
  other {
    {num_guests, plural, offset:1 
      =0 {{host} does not give a party.}
      =1 {{host} invites {guest} to their party.}
      =2 {{host} invites {guest} and one other person to their party.}
      other {{host} invites {guest} and # other people to their party.}}}}"""
          
main :: Effect Unit
main = do
  case (Intl.parse example) of
    Left e -> log ("failed to parse: " <> show e)
    Right v -> do
      let
        c = v # traverse (repeatVowels >=> accentifyString)
      log $ Intl.print $ evalState c (LCG.lcgPerturb 21313.0 (LCG.mkSeed 2034))

combiners :: NonEmptyArray Char
combiners =
  unsafePartial ( range 0x0300 0x034E
      # traverse fromCharCode
      >>= NonEmpty.fromArray
      # fromJust
  )

type Rand a
  = State LCG.Seed a

random :: Rand Int
random = do
  seed <- get
  let
    v = LCG.unSeed seed
  put (LCG.lcgNext seed)
  pure v

choose :: forall a. NonEmptyArray a -> Rand a
choose arr = do
  let n = NonEmpty.length arr
  f <- random
  let idx = f `rem` n
  pure $ unsafePartial (fromJust (arr !! idx))


isPlainAlpha :: CodePoint -> Boolean
isPlainAlpha c = do
  let str = String.fromCodePointArray [ c ]
  if CodeUnits.length str == 1 then do
    case CodeUnits.charAt 0 str of
      Just ch -> isAlpha ch && isLatin1 ch
      Nothing -> false
  else
    false

isVowel :: CodePoint -> Boolean
isVowel a = String.contains (Pattern $ String.singleton a) "aeiouAEIOU"

repeat :: CodePoint -> Rand (Array CodePoint)
repeat c = do
  if isVowel c then do
    n <- choose dist
    pure $ replicate n c
  else
    pure [c]
  where
    dist = NonEmpty.cons' 1 [1, 1, 1, 1, 2, 2, 2, 3, 3, 4]
  

accentify :: CodePoint -> Rand (Array CodePoint)
accentify c = do
  if isPlainAlpha c then do
    d :: Char <- choose combiners
    pure [c, String.codePointFromChar d]
  else
    pure [c]

repeatVowels :: String -> Rand String
repeatVowels s = do
  let ls = String.toCodePointArray s
  p  <- traverse repeat ls
  pure $ String.fromCodePointArray (join p)

accentifyString :: String -> Rand String
accentifyString s = do
  let ls = String.toCodePointArray s
  p  <- traverse accentify ls
  pure $ String.fromCodePointArray (join p)
