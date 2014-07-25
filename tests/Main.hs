{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Data.MessagePack
import Data.Serialize
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.QuickCheck
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Text as T

main :: IO ()
main = $(defaultMainGenerator)

instance Arbitrary Object where
    arbitrary = resize 6 $ oneof [ return ObjectNil
                                 , ObjectInt    <$> arbitrary
                                 , ObjectBool   <$> arbitrary
                                 , ObjectFloat  <$> arbitrary
                                 , ObjectDouble <$> arbitrary
                                 , ObjectString <$> arbitrary
                                 , ObjectBinary <$> arbitrary
                                 , ObjectArray  <$> arbitrary
                                 , ObjectMap    <$> arbitrary ]

    shrink (ObjectString s) = map ObjectString $ shrink s
    shrink (ObjectBinary b) = map ObjectBinary $ shrink b
    shrink (ObjectArray a)  = (map ObjectArray $ shrink a) ++ a
    shrink (ObjectMap m)    = (map ObjectMap $ shrink m) ++ M.keys m ++ M.elems m
    shrink _                = []

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (M.Map k v) where

    arbitrary = M.fromList <$> arbitrary

    shrink  = map M.fromList . shrink . M.toList

instance Arbitrary BS.ByteString where

    arbitrary = BS.pack <$> arbitrary

    shrink = map BS.pack . shrink . BS.unpack

instance Arbitrary T.Text where

    arbitrary = T.pack <$> arbitrary

    shrink = map T.pack . shrink . T.unpack

prop_encodeDecodeIsIdentity :: Object -> Bool
prop_encodeDecodeIsIdentity o = either error (== o) $ decode $ encode o

