# MessagePack for Haskell

This implementation defines an messagepack `Object` type, which is an instance of
`Serialize` (from [cereal](http://hackage.haskell.org/package/cereal) ):

```haskell
data Object = ObjectInt    Int
            | ObjectNil
            | ObjectBool   Bool
            | ObjectFloat  Float
            | ObjectDouble Double
            | ObjectString Text
            | ObjectBinary ByteString
            | ObjectArray  [Object]
            | ObjectMap    (Map Object Object )
    deriving (Eq, Ord, Show)
    
instance Serialize Object where
    -- ...
```

Thus, you can use cereal's `encode` and `decode` to pack and unpack objects.
