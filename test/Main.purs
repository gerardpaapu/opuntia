module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import FormatJS.IntlMessageFormat as Intl
import Test.QuickCheck (Result(..), (===))
import Test.Unit (test)
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)

roundTrips :: Intl.MessageFormatPattern String -> Result
roundTrips ast = do
  let src = Intl.print ast
  case Intl.parse src of
    Right ast' -> Intl.print ast' === Intl.print ast
    _ -> Failed "failed to parse"

main :: Effect Unit
main = runTest do
  test "Roundtripping" do
    quickCheck roundTrips 
