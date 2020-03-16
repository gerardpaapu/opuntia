module Main where

import Prelude

import Cactus.PseudoTranslate (pseudoTranslate)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import FormatJS.IntlMessageFormat as Intl
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS

example :: String
example =
  """{gender_of_host, select, 
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
main = launchAff_ do
  src <- FS.readTextFile UTF8 "example.json"
  obj <- Intl.readMessages src
  log $ Intl.writeMessages (pseudoTranslate <$> obj)

log :: String -> Aff Unit
log = liftEffect <<< Console.log

