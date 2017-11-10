{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module UnionSumsSpec where

import Test.Hspec

import UnionSums


data SubTypeA
  = FooSubTypeA String
  | BarSubTypeA
  deriving (Eq, Show)

data SubTypeB = BazSubTypeB deriving (Eq, Show)

unionSumTypes "BigType" [''SubTypeA, ''SubTypeB]

deriving instance Eq BigType
deriving instance Show BigType

mkConverter ''SubTypeA ''BigType
mkConverter ''SubTypeB ''BigType

mkDecompose ''BigType [''SubTypeA, ''SubTypeB]


spec :: Spec
spec = do
  describe "type conversion" $ do
    it "should convert small value to big value" $ do
      subTypeAToBigType BarSubTypeA `shouldBe` BarBigType
      subTypeAToBigType (FooSubTypeA "hello") `shouldBe` FooBigType "hello"
      subTypeBToBigType BazSubTypeB `shouldBe` BazBigType

    it "should allow conversion of big value via the smaller" $ do
      decomposeBigType f g (FooBigType "world") `shouldBe` Just "world"
      decomposeBigType f g BarBigType `shouldBe` Nothing
      decomposeBigType f g BazBigType `shouldBe` Nothing
      where
        f (FooSubTypeA s) = Just s
        f BarSubTypeA = Nothing
        g BazSubTypeB = Nothing
