{-
 - AutoStorable.hs
 - By Steven Smith
 -}

-- | AutoStorable enables users to generate 'Storable' instances automatically.
--
-- In order to use AutoStorable to generate a 'Storable' instance for a data
-- type, that data type must adhere to the following requirements:
--
-- * It only has one data constructor (no sum types)
-- * Every component of the data type is itself an instance of 'Storable'
--
-- Basically, if your data type is isomorphic to some N-tuple where each
-- component is an instance of 'Storable', then you can use this module to
-- generate 'Storable' instances.
--
-- To use this module, you need to provide two functions: one that converts
-- your data type into one of the tuple types 'T1' through 'T8' and one that
-- converts from that tuple type back to your data type. For example:
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
-- AutoStorable also provides a combinator 'withIso' to simplify
-- implementations. Here's a complete example to show how to use it:
--
-- @
-- data MyData = MyData Int Float Bool
--
-- myDataIso = withIso to from
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
module Foreign.AutoStorable (
withIso,
autoSizeOf,
autoAlignment,
autoPeek,
autoPoke,
T1 (..),
T2 (..),
T3 (..),
T4 (..),
T5 (..),
T6 (..),
T7 (..),
T8 (..)
) where

import Control.Applicative
import Foreign.Storable
import Foreign.Ptr
import Control.Monad.State.Strict

-- | A continuation passing combinator. Think of this as a way to hold onto
-- the isomorphism in one place to feed into the @auto...@ functions:
--
-- @
-- iso = withIso fromMyData toMyData
--
-- instance Storable MyData where
--     sizeOf = iso autoSizeOf
--     alignment = iso autoAlignment
--     peek = iso autoPeek
--     poke = iso autoPoke
-- @
withIso :: (a -> b) -> (b -> a) -> ((a -> b) -> (b -> a) -> r) -> r
withIso f g h = h f g

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
autoPoke to form ptr a = poke (castPtr ptr `asTypeOf` wrap (to a)) (to a)

unwrap :: Ptr a -> a
unwrap = undefined

wrap :: a -> Ptr a
wrap = undefined

peekShift :: Storable b => b -> StateT (Ptr a) IO b
peekShift b = do
    ptr <- get
    b' <- lift $ peek (castPtr ptr `asTypeOf` wrap b)
    put (ptr `plusPtr` sizeOf b)
    return b'

pokeShift :: Storable b => b -> StateT (Ptr a) IO ()
pokeShift b = do
    ptr <- get
    lift $ poke (castPtr ptr `asTypeOf` wrap b) b
    put (ptr `plusPtr` sizeOf b)

runPeek :: Monad m => Ptr a -> StateT (Ptr a) m a -> m a
runPeek ptr st = evalStateT st ptr

runPoke :: Monad m => Ptr a -> StateT (Ptr a) m () -> m ()
runPoke ptr st = evalStateT st ptr

data T1 a = T1 a
data T2 a b = T2 a b
data T3 a b c = T3 a b c
data T4 a b c d = T4 a b c d
data T5 a b c d e = T5 a b c d e
data T6 a b c d e f = T6 a b c d e f
data T7 a b c d e f g = T7 a b c d e f g
data T8 a b c d e f g h = T8 a b c d e f g h

instance Storable a => Storable (T1 a) where
    sizeOf ~(T1 a) = sizeOf a
    alignment ~(T1 a) = alignment a
    peek ptr = runPeek ptr $ T1
        <$> peekShift a
      where
        ~(T1 a) = unwrap ptr
    poke ptr (T1 a) = runPoke ptr $ do
        pokeShift a

instance (Storable a, Storable b) => Storable (T2 a b) where
    sizeOf ~(T2 a b) = sizeOf a + sizeOf b
    alignment ~(T2 a _) = alignment a
    peek ptr = runPeek ptr $ T2
        <$> peekShift a
        <*> peekShift b
      where
        ~(T2 a b) = unwrap ptr
    poke ptr (T2 a b) = runPoke ptr $ do
        pokeShift a
        pokeShift b

instance (Storable a, Storable b, Storable c) => Storable (T3 a b c) where
    sizeOf ~(T3 a b c) = sizeOf a + sizeOf b + sizeOf c
    alignment ~(T3 a _ _) = alignment a
    peek ptr = runPeek ptr $ T3
        <$> peekShift a
        <*> peekShift b
        <*> peekShift c
      where
        ~(T3 a b c) = unwrap ptr
    poke ptr (T3 a b c) = runPoke ptr $ do
        pokeShift a
        pokeShift b
        pokeShift c

instance (Storable a, Storable b, Storable c, Storable d)
    => Storable (T4 a b c d) where
    sizeOf ~(T4 a b c d) = sizeOf a + sizeOf b + sizeOf c + sizeOf d
    alignment ~(T4 a _ _ _) = alignment a
    peek ptr = runPeek ptr $ T4
        <$> peekShift a
        <*> peekShift b
        <*> peekShift c
        <*> peekShift d
      where
        ~(T4 a b c d) = unwrap ptr
    poke ptr (T4 a b c d) = runPoke ptr $ do
        pokeShift a
        pokeShift b
        pokeShift c
        pokeShift d

instance (Storable a, Storable b, Storable c, Storable d, Storable e)
    => Storable (T5 a b c d e) where
    sizeOf ~(T5 a b c d e) =
        sizeOf a + sizeOf b + sizeOf c + sizeOf d + sizeOf e
    alignment ~(T5 a _ _ _ _) = alignment a
    peek ptr = runPeek ptr $ T5
        <$> peekShift a
        <*> peekShift b
        <*> peekShift c
        <*> peekShift d
        <*> peekShift e
      where
        ~(T5 a b c d e) = unwrap ptr
    poke ptr (T5 a b c d e) = runPoke ptr $ do
        pokeShift a
        pokeShift b
        pokeShift c
        pokeShift d
        pokeShift e

instance
    (Storable a, Storable b, Storable c, Storable d, Storable e, Storable f)
    => Storable (T6 a b c d e f) where
    sizeOf ~(T6 a b c d e f) =
        sizeOf a + sizeOf b + sizeOf c + sizeOf d + sizeOf e + sizeOf f
    alignment ~(T6 a _ _ _ _ _) = alignment a
    peek ptr = runPeek ptr $ T6
        <$> peekShift a
        <*> peekShift b
        <*> peekShift c
        <*> peekShift d
        <*> peekShift e
        <*> peekShift f
      where
        ~(T6 a b c d e f) = unwrap ptr
    poke ptr (T6 a b c d e f) = runPoke ptr $ do
        pokeShift a
        pokeShift b
        pokeShift c
        pokeShift d
        pokeShift e
        pokeShift f

instance
    (Storable a, Storable b, Storable c, Storable d, Storable e, Storable f,
     Storable g)
    => Storable (T7 a b c d e f g) where
    sizeOf ~(T7 a b c d e f g) =
        sizeOf a + sizeOf b + sizeOf c + sizeOf d + sizeOf e + sizeOf f +
        sizeOf g
    alignment ~(T7 a _ _ _ _ _ _) = alignment a
    peek ptr = runPeek ptr $ T7
        <$> peekShift a
        <*> peekShift b
        <*> peekShift c
        <*> peekShift d
        <*> peekShift e
        <*> peekShift f
        <*> peekShift g
      where
        ~(T7 a b c d e f g) = unwrap ptr
    poke ptr (T7 a b c d e f g) = runPoke ptr $ do
        pokeShift a
        pokeShift b
        pokeShift c
        pokeShift d
        pokeShift e
        pokeShift f
        pokeShift g

instance
    (Storable a, Storable b, Storable c, Storable d, Storable e, Storable f,
     Storable g, Storable h)
    => Storable (T8 a b c d e f g h) where
    sizeOf ~(T8 a b c d e f g h) =
        sizeOf a + sizeOf b + sizeOf c + sizeOf d + sizeOf e + sizeOf f +
        sizeOf g + sizeOf h
    alignment ~(T8 a _ _ _ _ _ _ _) = alignment a
    peek ptr = runPeek ptr $ T8
        <$> peekShift a
        <*> peekShift b
        <*> peekShift c
        <*> peekShift d
        <*> peekShift e
        <*> peekShift f
        <*> peekShift g
        <*> peekShift h
      where
        ~(T8 a b c d e f g h) = unwrap ptr
    poke ptr (T8 a b c d e f g h) = runPoke ptr $ do
        pokeShift a
        pokeShift b
        pokeShift c
        pokeShift d
        pokeShift e
        pokeShift f
        pokeShift g
        pokeShift h
