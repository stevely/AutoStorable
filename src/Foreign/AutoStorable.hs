{-
 - AutoStorable.hs
 - By Steven Smith
 -}

-- | AutoStorable enables users to generate 'Storable' instances automatically.
--
-- In order to use AutoStorable to generate a 'Storable' instance for a data
-- type, that data type must only contain fields that are instances of
-- `Storable`.
--
-- Basically, if your data type is isomorphic to some N-tuple where each
-- component is an instance of 'Storable', or your data type is a sum type
-- where each data constructor is isomorphic to some N-tuple where each
-- component is an instance of 'Storable', then you can use this module to
-- generate 'Storable' instances.
--
-- To use this module, you need to provide two functions: one that converts
-- your data type into one of the tuple types 'T1' through 'T8' or one of the
-- sum types 'S2' through 'S8' and one that converts from that type back to
-- your data type. For example:
--
-- @
-- data MyData = MyData Int Float Bool
--
-- myDataToTuple :: MyData -> T3 Int Float Bool
-- myDataToTuple (MyData i f b) = T3 i f b
--
-- tupleToMyData :: T3 Int Float Bool -> MyData
-- tupleToMyData (T3 i f b) = MyData i f b
-- @
--
-- With these two functions defined you can now define your 'Storable' instance
-- using 'autoSizeOf', 'autoAlignment', 'autoPeek', and 'autoPoke':
--
-- @
-- instance Storable MyData where
--     sizeOf = autoSizeOf myDataToTuple tupleToMyData
--     alignment = autoAlignment myDataToTuple tupleToMyData
--     peek = autoPeek myDataToTuple tupleToMyData
--     poke = autoPoke myDataToTuple tupleToMyData
-- @
--
-- AutoStorable also provides a combinator 'defineIso' to simplify
-- implementations. Here's a complete example to show how to use it:
--
-- @
-- data MyData = MyData Int Float Bool
--
-- myDataIso = defineIso to from
--   where
--     to :: MyData -> T3 Int Float Bool
--     to (MyData i f b) = T3 i f b
--     from :: T3 Int Float Bool -> MyData
--     from (T3 i f b) = MyData i f b
--
-- instance Storable MyData where
--     sizeOf = myDataIso autoSizeOf
--     alignment = myDataIso autoAlignment
--     peek = myDataIso autoPeek
--     poke = myDataIso autoPoke
-- @
--
-- Here's an example showing how to work with sum types (type signatures
-- omitted for brevity):
--
-- @
-- data MyData = MyData1 Int Float Bool
--             | MyData2 Float Int
--             | MyData3 Bool Bool Bool Bool
--
-- myDataIso = defineIso to from
--   where
--     to (MyData1 i f b)      = S3a (T3 i f b)
--     to (MyData2 f i)        = S3b (T2 f i)
--     to (MyData3 b1 b2 b3 b4 = S3c (T4 b1 b2 b3 b4)
--     from (S3a (T3 i f b))       = MyData1 i f b
--     from (S3b (T2 f i))         = MyData2 f i
--     from (S3c (T4 b1 b2 b3 b4)) = MyData3 b1 b2 b3 b4
--
-- instance Storable MyData where
--     sizeOf = myDataIso autoSizeOf
--     alignment = myDataIso autoAlignment
--     peek = myDataIso autoPeek
--     poke = myDataIso autoPoke
-- @
module Foreign.AutoStorable
( defineIso
, autoSizeOf
, autoAlignment
, autoPeek
, autoPoke
) where

import Foreign.Ptr
import Foreign.Storable

import Foreign.AutoStorable.Utils

-- | A continuation passing combinator. Think of this as a way to hold onto
-- the isomorphism in one place to feed into the @auto...@ functions:
--
-- @
-- iso = defineIso fromMyData toMyData
--
-- instance Storable MyData where
--     sizeOf = iso autoSizeOf
--     alignment = iso autoAlignment
--     peek = iso autoPeek
--     poke = iso autoPoke
-- @
defineIso :: (a -> b) -> (b -> a) -> ((a -> b) -> (b -> a) -> r) -> r
defineIso f g h = h f g

-- | Given an isomorphism between two types @a@ and @b@, with @b@ being an
-- instance of 'Storable', produces an implementation for 'sizeOf' for the type
-- @a@.
autoSizeOf :: Storable b => (a -> b) -> (b -> a) -> a -> Int
autoSizeOf to _ = sizeOf . to

-- | Given an isomorphism between two types @a@ and @b@, with @b@ being an
-- instance of 'Storable', produces an implementation for 'alignment' for the
-- type @a@.
autoAlignment :: Storable b => (a -> b) -> (b -> a) -> a -> Int
autoAlignment to _ = alignment . to

-- | Given an isomorphism between two types @a@ and @b@, with @b@ being an
-- instance of 'Storable', produces an implementation for 'peek' for the type
-- @a@.
autoPeek :: Storable b => (a -> b) -> (b -> a) -> Ptr a -> IO a
autoPeek to from ptr =
    fmap from (peek (castPtr ptr `asTypeOf` wrap (to (unwrap ptr))))

-- | Given an isomorphism between two types @a@ and @b@, with @b@ being an
-- instance of 'Storable', produces an implementation for 'poke' for the type
-- @a@.
autoPoke :: Storable b => (a -> b) -> (b -> a) -> Ptr a -> a -> IO ()
autoPoke to _ ptr a = poke (castPtr ptr `asTypeOf` wrap (to a)) (to a)
