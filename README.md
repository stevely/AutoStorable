# AutoStorable

This is a small Haskell library for simplifying the process of writing
`Storable` instances for data types which have trivial `Storable` instance
implementations. These are data types that only contain fields that are
themselves instances of `Storable`.

## Using AutoStorable

The recommended way to use AutoStorable is to define an isomorphism between
your data type and one of AutoStorable's data types using `withIso`, and then
using that combinator with `autoSizeOf`, `autoAlignment`, `autoPeek`, and
`autoPoke` to define your `Storable` instance. Here is a full example:

    data MyData = MyData Int Float Bool

    myDataIso :: ((MyData -> T3 Int Float Bool)
                  -> (T3 Int Float Bool -> MyData) -> r)
                 -> r
    myDataIso = withIso to from
      where
        to :: MyData -> T3 Int Float Bool
        to (MyData i f b) = T3 i f b
        from :: T3 Int Float Bool -> MyData
        from (T3 i f b) = MyData i f b

    instance Storable MyData where
        sizeOf = myDataIso autoSizeOf
        alignment = myDataIso autoAlignment
        peek = myDataIso autoPeek
        poke = myDataIso autoPoke
