{-# LANGUAGE TemplateHaskell #-}

module UnionSums (unionSumTypes) where

import Control.Error.Util
import Control.Monad (join)
import Data.Text (pack, unpack)
import qualified Data.Text as Text

import Language.Haskell.TH

-- | Makes a new sum type with the given name from a list of (names of) other
--   sum types. Note that the input types must have constructors suffixed with
--   their type name. The newly constructed type will have constructors where
--   the original type name suffix has been replaced with the name of the new
--   type. E.g.
--
--   data FooType = Foo1FooType | Foo2FooType
--   data BarType = BarBarType
--   unionSumTypes "FooBarType" [''FooType, ''BarType]
--
--   provides
--
--   data FooBarType = Foo1FooBarType | Foo2FooBarType | BarFooBarType
unionSumTypes :: String -> [Name] -> Q [Dec]
unionSumTypes _ [] = return []
unionSumTypes newNameStr typeNames =
  do
    newConstructors <- join <$> mapM mkNewConstructors typeNames
    return [DataD [] (mkName newNameStr) [] Nothing newConstructors []]
  where
    mkNewConstructors typeName = do
      info <- reify typeName
      case info of
        (TyConI (DataD _ _ _ _ constructors _)) ->
          either fail return $ mapM (modConstructor typeName) constructors
        _ -> fail $ nameBase typeName ++ " must be a sum type"

    modConstructor :: Name -> Con -> Either String Con
    modConstructor typeName (NormalC conName args) =
        (\n -> (NormalC (mkName $ n ++ newNameStr) args)) <$>
        note "Constructor name missing type name suffix"
          (stripNameSuffix typeName conName)
    modConstructor _ _ = fail "Unrecognised constructor pattern"

    stripNameSuffix :: Name -> Name -> Maybe String
    stripNameSuffix suf = stripSuffix (nameBase suf) . nameBase


stripSuffix :: String -> String -> Maybe String
stripSuffix suf = fmap unpack . Text.stripSuffix (pack suf) . pack
