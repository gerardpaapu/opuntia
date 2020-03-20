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
  , Options
  ) where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (throwError)
import Data.Either (either)
import Data.Foldable (class Foldable, foldMap, foldrDefault, foldlDefault)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (head)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (class Traversable, traverse, sequenceDefault)
import Data.Tuple (Tuple)
import Effect.Exception (Error, error, message)
import Foreign (Foreign, ForeignError(..), MultipleErrors, fail, renderForeignError)
import Foreign.Object (Object, fromFoldable)
import Prim.Row as Row
import Record as Record
import Simple.JSON (class ReadForeign, class WriteForeign, E, read, readImpl, readJSON, write, writeImpl)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)
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

derive instance newtypeMFP :: Newtype (MessageFormatPattern a) _
derive newtype instance showMFP :: Show a => Show (MessageFormatPattern a)
derive instance genericMFP :: Generic (MessageFormatPattern a) _
instance eqMFP :: Eq a => Eq (MessageFormatPattern a) where
  eq = genericEq
  
derive newtype instance arbitraryMFP :: Arbitrary a => Arbitrary (MessageFormatPattern a)

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
    intoOptions r@{ options } = options # unwrap # foldMap \item -> foldMap intoElement item.value
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
            # unwrap
            # traverse \r@{ value } -> ado
                v <- traverse intoElement value
                in r { value = v }
        in PluralElement (r { options = wrap options' })
      -- TODO: figure out how to remove this duplication
      SelectElement r@{ options } -> ado
        options' <-
          options
            # unwrap
            # traverse \{ value, location } -> ado
                v <- traverse intoElement value
                in { value: v, location }
        in SelectElement (r { options = wrap options' })

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
    , location :: Location
    | r
    }

data StringOrElse r
  = String_ String
  | Else_ r


derive instance genericSOE :: Generic (StringOrElse r) _
instance eqSOE :: Eq r => Eq (StringOrElse r) where
  eq = genericEq


instance readForeignStringOrElse :: ReadForeign a => ReadForeign (StringOrElse a) where
  readImpl obj = (String_ <$> readImpl obj)
                 <|> (Else_ <$> readImpl obj)

instance writeForeignStringOrElse :: WriteForeign a => WriteForeign (StringOrElse a) where
  writeImpl = case _ of
    String_ s -> writeImpl s
    Else_ c -> writeImpl c

instance showStringOrElse :: Show a => Show (StringOrElse a) where
  show = genericShow

instance arbitraryStringOrElse :: Arbitrary a => Arbitrary (StringOrElse a) where
  arbitrary = genericArbitrary

type SimpleFormatElement r
  = BaseElement ( style :: Maybe (StringOrElse r))

type NumberSkeleton =
  { tokens :: Array NumberSkeletonToken
  , location :: Location
  }

type NumberSkeletonToken =
  { stem :: String
  , options :: Array String
  }

type DateTimeSkeleton =
  { pattern :: String
  , location :: Location
  }

type PluralOrSelectOption text
  = { value :: Array (MessageFormatElement text)
    , location :: Location
    }

newtype Options text =
  Options (Object (PluralOrSelectOption text))

derive instance genericOptions :: Generic (Options a) _
derive instance newtypeOptions :: Newtype (Options a) _
derive instance functorOptions :: Functor Options
derive newtype instance readForeignOption :: ReadForeign a => ReadForeign (Options a)
derive newtype instance writeForeignOption :: WriteForeign a => WriteForeign (Options a)
instance showOptions :: Show a => Show (Options a) where
  show = genericShow

instance eqOptions :: Eq a => Eq (Options a) where
  eq = genericEq

instance arbitraryOptions :: Arbitrary a => Arbitrary (Options a) where
  arbitrary = do
    ls :: Array (Tuple String _) <- arbitrary
    pure $ Options (fromFoldable ls)


data MessageFormatElement text
  = LiteralElement { value :: text, location :: Location }
  | ArgumentElement { value :: String, location :: Location }
  | NumberElement (SimpleFormatElement NumberSkeleton)
  | DateElement (SimpleFormatElement DateTimeSkeleton)
  | TimeElement (SimpleFormatElement DateTimeSkeleton)
  | SelectElement (BaseElement (options :: Options text))
  | PluralElement
    ( BaseElement
        ( options :: Options text
        , offset :: Number
        , pluralType :: String
        )
    )
  | PoundElement { location :: Location }
  | TagElement { value :: String
               , children :: Array (MessageFormatElement text)
               , location :: Location
               }

derive instance genericMFE :: Generic (MessageFormatElement a) _
    
instance showMFE :: Show a => Show (MessageFormatElement a) where
  show = genericShow

instance eq :: Eq a => Eq (MessageFormatElement a) where
  eq = genericEq

instance arbitraryMFE :: Arbitrary a => Arbitrary (MessageFormatElement a) where
  arbitrary = genericArbitrary

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

newtype Location
  = Location (Maybe { start :: LocationDetails
                    , end :: LocationDetails
                    })
    
derive instance genericLocation :: Generic Location _
derive instance newtypeLocation :: Newtype Location _
derive newtype instance readForeignLocation :: ReadForeign Location
derive newtype instance writeForeignLocation :: WriteForeign Location
                        
instance showLocation :: Show Location where
  show = genericShow

instance arbitraryLocation :: Arbitrary Location where
  arbitrary = pure $ Location Nothing

instance eqLocation :: Eq Location where
  eq = genericEq
