{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import Data.MessagePack
import qualified Data.Text as T
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.QuickCheck

main :: IO ()
main = $(defaultMainGenerator)

instance Arbitrary Object where
    arbitrary = sized $ \n -> oneof [ return ObjectNil
                                    , ObjectInt    <$> arbitrary
                                    , ObjectBool   <$> arbitrary
                                    , ObjectFloat  <$> arbitrary
                                    , ObjectDouble <$> arbitrary
                                    , ObjectString <$> arbitrary
                                    , ObjectBinary <$> arbitrary
                                    , ObjectArray  <$> resize (3 * n `quot` 4) arbitrary
                                    , ObjectMap    <$> resize (3 * n `quot` 4) arbitrary
                                    , ObjectExt    <$> arbitrary <*> arbitrary ]

    shrink (ObjectString s) = map ObjectString $ shrink s
    shrink (ObjectBinary b) = map ObjectBinary $ shrink b
    shrink (ObjectArray a)  = (map ObjectArray $ shrink a) ++ a
    shrink (ObjectMap m)    = (map ObjectMap $ shrink m) ++ M.keys m ++ M.elems m
    shrink (ObjectExt t s)  = map (ObjectExt t) $ shrink s
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
prop_encodeDecodeIsIdentity o = (== o) $ decodeObj $ encodeObj o
  where
    encodeObj :: Object -> LB.ByteString
    encodeObj = encode

    decodeObj :: LB.ByteString -> Object
    decodeObj = decode
