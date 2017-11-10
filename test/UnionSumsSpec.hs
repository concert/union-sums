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

data SubTypeC = QuxSubTypeC deriving (Eq, Show)

unionSumTypes "BigType" [''SubTypeA, ''SubTypeB, ''SubTypeC]

deriving instance Eq BigType
deriving instance Show BigType

mkConverter ''SubTypeA ''BigType
mkConverter ''SubTypeB ''BigType
mkConverter ''SubTypeC ''BigType

mkDecompose ''BigType [''SubTypeA, ''SubTypeB, ''SubTypeC]


spec :: Spec
spec = do
  describe "type conversion" $ do
    it "should convert small value to big value" $ do
      subTypeAToBigType BarSubTypeA `shouldBe` BarBigType
      subTypeAToBigType (FooSubTypeA "hello") `shouldBe` FooBigType "hello"
      subTypeBToBigType BazSubTypeB `shouldBe` BazBigType
      subTypeCToBigType QuxSubTypeC `shouldBe` QuxBigType

    it "should allow conversion of big value via the smaller" $ do
      decomposeBigType f g h (FooBigType "world") `shouldBe` Just "world"
      decomposeBigType f g h BarBigType `shouldBe` Nothing
      decomposeBigType f g h BazBigType `shouldBe` Nothing
      decomposeBigType f g h QuxBigType `shouldBe` Nothing
      where
        f (FooSubTypeA s) = Just s
        f BarSubTypeA = Nothing
        g BazSubTypeB = Nothing
        h QuxSubTypeC = Nothing
