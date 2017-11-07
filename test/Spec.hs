{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
import UnionSums

data FooType = Foo1FooType String | Foo2FooType deriving (Eq, Show)
data BarType = BarBarType deriving (Eq, Show)
unionSumTypes "FooBarType" [''FooType, ''BarType]
deriving instance Show FooBarType
deriving instance Eq FooBarType

main :: IO ()
main = putStrLn $ if Foo1FooBarType "bill" /= BarFooBarType then "pass" else "fail"
