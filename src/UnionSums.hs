{-# LANGUAGE TemplateHaskell #-}

module UnionSums (unionSumTypes, toMainTypeConversion) where

import Control.Error.Util
import Control.Monad (join)
import Data.Char (toLower)
import Data.Maybe (fromJust)
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
        (TyConI dec) ->
          either fail return $ mapM (modConstructor typeName) $ getConstructors dec
        _ -> fail $ nameBase typeName ++ " must be a sum type"

    modConstructor :: Name -> Con -> Either String Con
    modConstructor typeName (NormalC conName args) =
        (\n -> NormalC (mkName n) args) <$>
        note "Constructor name missing type name suffix"
          (changeSuffix (nameBase typeName) newNameStr (nameBase conName))
    modConstructor _ _ = fail "Unrecognised constructor pattern"

toMainTypeConversion :: Name -> Name -> Q [Dec]
toMainTypeConversion subName mainName = do
    ft <- [t| $(conT subName) -> $(conT mainName) |]
    subDec <- unTy <$> reify subName
    f <- fun $ getConstructors subDec
    return [sig ft, f]
  where
    sig t = SigD name t
    unTy (TyConI d) = d
    tOfDec (DataD _ _ _ k _ _) = fromJust k
    fun subCons = clauses subCons >>= return . (FunD name)
    name = mkName $ lowerFirst $ (nameBase subName) ++ "To" ++ (nameBase mainName)
    clauses subCons = mapM toClause subCons
    swapSuffix n = mkName $ fromJust $ changeSuffix (nameBase subName) (nameBase mainName) $ nameBase n
    toClause (NormalC conName args) = do
        argNames <- mapM (const $ newName "a") args
        return $ Clause (conPattern conName argNames) (recon (swapSuffix conName) argNames) []
    conPattern conName argNames = [ConP conName $ map VarP argNames]
    recon mainConName argNames = NormalB $ foldl AppE (ConE mainConName) $ map VarE argNames

getConstructors :: Dec -> [Con]
getConstructors (DataD _ _ _ _ cs _) = cs
getConstructors _ = error "Could not get constructors"

changeSuffix :: String -> String -> String -> Maybe String
changeSuffix startSuffix endSuffix var = (\n -> n ++ endSuffix) <$> (stripSuffix startSuffix var)

stripSuffix :: String -> String -> Maybe String
stripSuffix suf = fmap unpack . Text.stripSuffix (pack suf) . pack

lowerFirst :: String -> String
lowerFirst (c:s) = toLower c : s
