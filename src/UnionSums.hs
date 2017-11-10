{-# LANGUAGE TemplateHaskell #-}

module UnionSums (unionSumTypes, mkConverter, mkDecompose) where

import Control.Error.Util
import Control.Monad (join, zipWithM)
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
      cons <- getConstructors typeName
      either fail return $ mapM (modConstructor typeName) cons

    modConstructor :: Name -> Con -> Either String Con
    modConstructor typeName (NormalC conName args) =
        (flip NormalC args) <$>
        note "Constructor name missing type name suffix"
          (changeSuffix typeName (mkName newNameStr) conName)
    modConstructor _ _ = fail "Unrecognised constructor pattern"

mkConverter :: Name -> Name -> Q [Dec]
mkConverter subName unionName = do
    ft <- [t| $(conT subName) -> $(conT unionName) |]
    cs <- getConstructors subName >>= mapM toClause
    return [SigD name ft, FunD name cs]
  where
    name = mkName $ lowerFirst $ (nameBase subName) ++ "To" ++ (nameBase unionName)
    swapSuffix = fromJust . changeSuffix subName unionName
    toClause (NormalC conName args) = do
        argNames <- mapM (const $ newName "a") args
        return $ Clause (conPattern conName argNames) (recon (swapSuffix conName) argNames) []
    conPattern conName argNames = [ConP conName $ map VarP argNames]
    recon mainConName argNames = NormalB $ foldl AppE (ConE mainConName) $ map VarE argNames

getConstructors :: Name -> Q [Con]
getConstructors n = do
    info <- reify n
    case info of
        (TyConI dec) -> case dec of
            (DataD _ _ _ _ cs _) -> return cs
            _ -> fail $ (nameBase n) ++ " does not only contain data definitions"
        _ -> fail $ (nameBase n) ++ " must be a sum type"

changeSuffix :: Name -> Name -> Name -> Maybe Name
changeSuffix oldSuffix newSuffix var = mkName . (++ (nameBase newSuffix)) <$>
    stripSuffix (nameBase oldSuffix) (nameBase var)

stripSuffix :: String -> String -> Maybe String
stripSuffix suf = fmap unpack . Text.stripSuffix (pack suf) . pack

lowerFirst :: String -> String
lowerFirst (c:s) = toLower c : s

replace :: Int -> a -> [a] -> [a]
replace i a as = go 0 as
  where
    go _ [] = []
    go currI (x:xs) =
      let val = if currI == i then a else x in
      val : go (currI + 1) xs

-- FIXME: this should probably only be called by our code, because otherwise
-- users could scuff up which sub-types they include for the main type.
mkDecompose :: Name -> [Name] -> Q [Dec]
mkDecompose unionName subNames = do
    unionCons <- getConstructors unionName
    subConss <- mapM getConstructors subNames
    let subConIdxs = mconcat $ zipWith replicate (length <$> subConss) [0..]
    clauses <- zipWithM mkClause unionCons $ zip subConIdxs (mconcat subConss)
    return [SigD name ft, FunD name clauses]
  where
    name = mkName $ "decompose" ++ nameBase unionName
    mkArrow t1 t2 = AppT (AppT ArrowT t1) t2
    a = VarT $ mkName "a"
    arrows = flip mkArrow a <$> ConT <$> subNames
    ft = foldr mkArrow (mkArrow (ConT unionName) a) arrows
    mkClause :: Con -> (Int, Con) -> Q Clause
    mkClause (NormalC conNameU argsU) (i, (NormalC conNameS argsS)) = do
        -- We assume argsU and argsS have been lined up correctly!
        argNames <- mapM (const $ newName "a") argsU
        _Pats <- mapM (const $ return . VarP $ mkName "_") subNames
        let fPats = replace i (VarP $ mkName "f") _Pats
        return $ Clause
          (fPats ++ [ConP conNameU $ fmap VarP argNames])
          (NormalB $ AppE (VarE $ mkName "f") $ foldl AppE (ConE conNameS) $ fmap VarE argNames)
          []
