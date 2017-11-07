{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
import UnionSums

data FooType = Foo1FooType String | Foo2FooType deriving (Eq, Show)
data BarType = BarBarType deriving (Eq, Show)
unionSumTypes "FooBarType" [''FooType, ''BarType]
deriving instance Show FooBarType
deriving instance Eq FooBarType

mkConverter ''FooType ''FooBarType
mkConverter ''BarType ''FooBarType

foo1 :: FooBarType
foo1 = fooTypeToFooBarType $ Foo1FooType "bill"

main :: IO ()
main = putStrLn $ if foo1 /= BarFooBarType then "pass" else "fail"
