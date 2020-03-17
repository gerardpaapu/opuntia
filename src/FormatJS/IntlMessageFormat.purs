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
  , DateTimeSkeleton
  , NumberSkeleton
  , NumberSkeletonToken
  , BaseElement
  , PluralOrSelectOption
  , StringOrElse
  ) where

import Prelude

import Control.Alternative ((<|>))
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

readMessages' :: String -> E (Object (MessageFormatPattern String))
readMessages' obj = do
  plain :: Object String <- readJSON obj
  traverse parse plain

readMessages :: forall m. MonadThrow Error m => String -> m (Object (MessageFormatPattern String))
readMessages = lift' <<< readMessages'
  where
  lift' = either (throwError <<< collectErrors) pure

writeMessages :: Object (MessageFormatPattern String) -> String
writeMessages obj = do jsonStringify (print <$> obj)

foreign import parseImpl :: forall a. String -> (Foreign -> a) -> (Error -> a) -> a

foreign import printImpl :: Foreign -> String

foreign import jsonStringify :: Object String -> String

newtype TypeTag = TypeTag Number
derive instance newtypeTag :: Newtype TypeTag _
derive newtype instance eqTypeTag :: Eq TypeTag
derive newtype instance readForeign :: ReadForeign TypeTag
derive newtype instance writeForeign :: WriteForeign TypeTag

foreign import type_literal :: TypeTag
foreign import type_argument :: TypeTag
foreign import type_number :: TypeTag
foreign import type_date :: TypeTag
foreign import type_time :: TypeTag
foreign import type_select :: TypeTag
foreign import type_plural :: TypeTag
foreign import type_pound :: TypeTag
foreign import type_tag :: TypeTag

foreign import skeleton_type_number :: TypeTag
foreign import skeleton_type_dateTime :: TypeTag


print :: (MessageFormatPattern String) -> String
print ast = printImpl (write ast)

parse :: String -> E (MessageFormatPattern String)
parse s = parseImpl s read throwAsForeign
  where
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
      (TagElement { children }) -> foldMap intoElement children
      _ -> mempty

    intoOptions :: forall r. { options :: _ | r } -> _
    intoOptions r@{ options } = options # foldMap \item -> foldMap intoElement item.value
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

      (TagElement r@{ children }) -> ado
        children' <- traverse intoElement children
        in TagElement (r { children = children'})

      (ArgumentElement v) -> pure (ArgumentElement v)
      (NumberElement v) -> pure (NumberElement v)
      (DateElement v) -> pure (DateElement v)
      (TimeElement v) -> pure (TimeElement v)
      (PoundElement v) -> pure (PoundElement v)
  sequence = sequenceDefault

derive instance functorMFP :: Functor MessageFormatPattern

derive newtype instance readForeignMFP :: ReadForeign a => ReadForeign (MessageFormatPattern a)

derive newtype instance writeForeignMFP :: WriteForeign a => WriteForeign (MessageFormatPattern a)

type BaseElement r
  = { value :: String
    , location :: Maybe Location
    | r
    }

data StringOrElse r
  = String_ String
  | Else_ r

instance readForeignStringOrElse :: ReadForeign a => ReadForeign (StringOrElse a) where
  readImpl obj = (String_ <$> readImpl obj)
                 <|> (Else_ <$> readImpl obj)

instance writeForeignStringOrElse :: WriteForeign a => WriteForeign (StringOrElse a) where
  writeImpl = case _ of
    String_ s -> writeImpl s
    Else_ c -> writeImpl c

instance showStringOrElse :: Show a => Show (StringOrElse a) where
  show = case _ of
    String_ s -> "(String_ " <> show s <> ")"
    Else_ c -> "(Else_ " <> show c <> ")"

type SimpleFormatElement r
  = BaseElement ( style :: Maybe (StringOrElse r))

type NumberSkeleton =
  { tokens :: Array NumberSkeletonToken
  , location :: Maybe Location
  }

type NumberSkeletonToken =
  { stem :: String
  , options :: Array String
  }

type DateTimeSkeleton =
  { pattern :: String
  , location :: Maybe Location
  }

type PluralOrSelectOption text
  = { value :: Array (MessageFormatElement text)
    , location :: Maybe Location
    }

data MessageFormatElement text
  = LiteralElement { value :: text, location :: Maybe Location }
  | ArgumentElement { value :: String, location :: Maybe Location }
  | NumberElement (SimpleFormatElement NumberSkeleton)
  | DateElement (SimpleFormatElement DateTimeSkeleton)
  | TimeElement (SimpleFormatElement DateTimeSkeleton)
  | SelectElement (BaseElement (options :: Object (PluralOrSelectOption text) ))
  | PluralElement
    ( BaseElement
        ( options :: Object (PluralOrSelectOption text)
        , offset :: Number
        , pluralType :: String
        )
    )
  | PoundElement { location :: Maybe Location }
  | TagElement { value :: String
               , children :: Array (MessageFormatElement text)
               , location :: Maybe Location
               }

instance showMFE :: Show a => Show (MessageFormatElement a) where
  show = case _ of
    (LiteralElement v) -> "(LiteralElement " <> show v <> ")"
    (ArgumentElement v) -> "(ArgumentElement " <> show v <> ")"
    (NumberElement v) -> "(NumberElement " <> show v <> ")"
    (DateElement v) -> "(DateElement " <> show v <> ")"
    (TimeElement v) -> "(TimeElement " <> show v <> ")"
    (SelectElement v) -> "(SelectElement " <> show v <> ")"
    (PluralElement v) -> "(PluralElement " <> show v <> ")"
    (PoundElement v) -> "(PoundElement " <> show v <> ")"
    (TagElement v) -> "(TagElement " <> show v <> ")"

derive instance functorElement :: Functor MessageFormatElement

type_ = SProxy :: SProxy "type"

tagObject :: forall a. Row.Lacks "type" a => TypeTag -> Record a -> { type :: TypeTag | a }
tagObject s = Record.insert type_ s

instance readForeignElement :: ReadForeign a => ReadForeign (MessageFormatElement a) where
  readImpl obj = do
    tag :: { type :: TypeTag } <- readImpl obj
    case tag.type of
      t | t == type_literal -> LiteralElement <$> readImpl obj
        | t == type_argument -> ArgumentElement <$> readImpl obj
        | t == type_number -> NumberElement <$> readImpl obj
        | t == type_date -> DateElement <$> readImpl obj
        | t == type_time -> TimeElement <$> readImpl obj
        | t == type_select -> SelectElement <$> readImpl obj
        | t == type_plural -> PluralElement <$> readImpl obj
        | t == type_pound -> PoundElement <$> readImpl obj
        | t == type_tag -> TagElement <$> readImpl obj
        | otherwise -> fail $ ForeignError "Bad tag reading Element"

instance writeForeignElement :: WriteForeign a => WriteForeign (MessageFormatElement a) where
  writeImpl = case _ of
    LiteralElement v -> tagObject type_literal v # writeImpl
    ArgumentElement v -> tagObject type_argument v # writeImpl
    NumberElement v -> tagObject type_number v # writeImpl
    DateElement v -> tagObject type_date v # writeImpl
    TimeElement v -> tagObject type_time v # writeImpl
    SelectElement v -> tagObject type_select v # writeImpl
    PluralElement v -> tagObject type_tag v # writeImpl
    PoundElement v -> tagObject type_pound v # writeImpl
    TagElement v -> tagObject type_tag v # writeImpl

type LocationDetails
  = { offset :: Number
    , line :: Number
    , column :: Number
    }

type Location
  = { start :: LocationDetails
    , end :: LocationDetails
    }
