module FormatJS.IntlMessageFormat
  ( readMessages
  , writeMessages
  , parse
  , print
  , MessageFormatPattern(..)
  , MessageFormatElement(..)
  , Location
  , LocationDetails
  , SimpleFormatElement
  , DateSkeleton
  , NumberSkeleton
  , TimeSkeleton
  , BaseElement
  , PluralOrSelectOption
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (throwError)
import Data.Either (either)
import Data.Foldable (class Foldable, foldMap, foldrDefault, foldlDefault)
import Data.List.NonEmpty (head)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable, traverse, sequenceDefault)
import Effect.Exception (Error, error, message)
import Foreign (Foreign, ForeignError(..), MultipleErrors, fail, renderForeignError)
import Foreign.Object (Object)
import Prim.Row as Row
import Record as Record
import Simple.JSON (class ReadForeign, class WriteForeign, E, read, readImpl, readJSON, write, writeImpl, writeJSON)
import Type.Prelude (SProxy(..))

collectErrors :: MultipleErrors -> Error
collectErrors = head >>> renderForeignError >>> error

lift' :: forall m a. MonadThrow Error m => E a -> m a
lift' = either (throwError <<< collectErrors) pure

readMessages' :: String -> E (Object (MessageFormatPattern String))
readMessages' obj = do
    plain :: Object String <- readJSON obj
    traverse parse plain

readMessages :: forall m. MonadThrow Error m => String -> m (Object (MessageFormatPattern String))
readMessages = lift' <<< readMessages'

writeMessages :: Object (MessageFormatPattern String) -> String
writeMessages obj = do
    let plain = print <$> obj
    jsonStringify plain

foreign import parseImpl :: forall a. String -> (Foreign -> a) -> (Error -> a) -> a

foreign import printImpl :: Foreign -> String

foreign import jsonStringify :: Object String -> String

print :: (MessageFormatPattern String) -> String
print ast = printImpl (write ast)

parse :: String -> E (MessageFormatPattern String)
parse s = parseImpl s read throwAsForeign

throwAsForeign :: forall a b c. MonadThrow (c ForeignError) a => Applicative c => Error -> a b
throwAsForeign e = throwError (pure $ ForeignError (message e))

newtype MessageFormatPattern text
  = MessageFormatPattern (Array (MessageFormatElement text))

derive newtype instance showMFP :: Show a => Show (MessageFormatPattern a)

instance foldableMFP :: Foldable MessageFormatPattern where
  foldMap f (MessageFormatPattern arr) = foldMap intoElement arr
    where
    intoElement = case _ of
      (LiteralElement { value }) -> f value
      (SelectElement r) -> intoOptions r
      (PluralElement r) -> intoOptions r
      _ -> mempty

    intoOptions :: forall r. { options :: _ | r } -> _
    intoOptions r@{ options } = options # foldMap \item -> foldMap (intoElement) item.value
  foldr f m = foldrDefault f m
  foldl f m = foldlDefault f m

instance traversableMFP :: Traversable MessageFormatPattern where
  traverse f (MessageFormatPattern arr) = MessageFormatPattern <$> traverse intoElement arr
    where
    intoElement = case _ of
      LiteralElement { value, location } -> ado
        v <- f value
        in LiteralElement { value: v, location }
      PluralElement r@{ options } -> ado
        options' <-
          options
            # traverse \r@{ value, location } -> ado
                v <- traverse intoElement value
                in r { value = v }
        in PluralElement (r { options = options' })
      -- TODO: figure out how to remove this duplication
      SelectElement r@{ options } -> ado
        options' <-
          options
            # traverse \{ value, location } -> ado
                v <- traverse intoElement value
                in { value: v, location }
        in SelectElement (r { options = options' })
      (ArgumentElement v) -> pure (ArgumentElement v)
      (NumberElement v) -> pure (NumberElement v)
      (DateElement v) -> pure (DateElement v)
      (TimeElement v) -> pure (TimeElement v)
      (PoundElement v) -> pure (PoundElement v)
      (TagElement v) -> pure (TagElement v)
  sequence = sequenceDefault

derive instance functorMFP :: Functor MessageFormatPattern

derive newtype instance readForeignMFP :: ReadForeign a => ReadForeign (MessageFormatPattern a)

derive newtype instance writeForeignMFP :: WriteForeign a => WriteForeign (MessageFormatPattern a)

type BaseElement r
  = { value :: String
    , location :: Maybe Location
    | r
    }

type SimpleFormatElement r
  = BaseElement ( style :: Maybe r )

type NumberSkeleton
  = Foreign -- TODO

type DateSkeleton
  = Foreign -- TODO

type TimeSkeleton
  = Foreign -- TODO

type PluralOrSelectOption text
  = { value :: Array (MessageFormatElement text)
    , location :: Maybe Location
    }

data MessageFormatElement text
  = LiteralElement { value :: text, location :: Maybe Location }
  | ArgumentElement { value :: String, location :: Maybe Location }
  | NumberElement (SimpleFormatElement NumberSkeleton)
  | DateElement (SimpleFormatElement DateSkeleton)
  | TimeElement (SimpleFormatElement TimeSkeleton)
  | SelectElement (BaseElement ( options :: Object (PluralOrSelectOption text) ))
  | PluralElement
    ( BaseElement
        ( options :: Object (PluralOrSelectOption text)
        , offset :: Number
        , pluralType :: String
        )
    )
  | PoundElement Foreign
  | TagElement Foreign

instance showMFE :: Show a => Show (MessageFormatElement a) where
  show = case _ of
    (LiteralElement v) -> "(LiteralElement " <> show v <> ")"
    (ArgumentElement v) -> "(ArgumentElement " <> show v <> ")"
    (NumberElement v) -> "(NumberElement " <> writeJSON v <> ")"
    (DateElement v) -> "(DateElement " <> writeJSON v <> ")"
    (TimeElement v) -> "(TimeElement " <> writeJSON v <> ")"
    (SelectElement v) -> "(SelectElement " <> show v <> ")"
    (PluralElement v) -> "(PluralElement " <> show v <> ")"
    (PoundElement v) -> "(PoundElement " <> writeJSON v <> ")"
    (TagElement v) -> "(TagElement " <> writeJSON v <> ")"

derive instance functorElement :: Functor MessageFormatElement

type_ = SProxy :: SProxy "type"

tagObject :: forall a. Row.Lacks "type" a => Number -> Record a -> { type :: Number | a }
tagObject s = Record.insert type_ s

instance readForeignElement :: ReadForeign a => ReadForeign (MessageFormatElement a) where
  readImpl obj = do
    tag :: { type :: Number } <- readImpl obj
    case tag.type of
      0.0 -> LiteralElement <$> readImpl obj
      1.0 -> ArgumentElement <$> readImpl obj
      2.0 -> NumberElement <$> readImpl obj
      3.0 -> DateElement <$> readImpl obj
      4.0 -> TimeElement <$> readImpl obj
      5.0 -> SelectElement <$> readImpl obj
      6.0 -> PluralElement <$> readImpl obj
      7.0 -> PoundElement <$> readImpl obj
      8.0 -> TagElement <$> readImpl obj
      _ -> fail $ ForeignError "Bad tag reading Element"

instance writeForeignElement :: WriteForeign a => WriteForeign (MessageFormatElement a) where
  writeImpl = case _ of
    LiteralElement v -> tagObject 0.0 v # writeImpl
    ArgumentElement v -> tagObject 1.0 v # writeImpl
    NumberElement v -> v # writeImpl
    DateElement v -> v # writeImpl
    TimeElement v -> v # writeImpl
    SelectElement v -> tagObject 5.0 v # writeImpl
    PluralElement v -> tagObject 6.0 v # writeImpl
    PoundElement v -> v # writeImpl
    TagElement v -> v # writeImpl

newtype Selector
  = Selector String

derive instance selectorNewtype :: Newtype Selector _

instance readForeignSelector :: ReadForeign Selector where
  readImpl obj = Selector <$> readImpl obj

instance writeForeignSelector :: WriteForeign Selector where
  writeImpl (Selector s) = writeImpl s

type LocationDetails
  = { offset :: Number
    , line :: Number
    , column :: Number
    }

type Location
  = { start :: LocationDetails
    , end :: LocationDetails
    }
