{-
 - Utils.hs
 - By Steven Smith
 -}

{-# LANGUAGE RankNTypes #-}

-- | Generally useful functions for writing 'Storable' instances. It's not
-- recommended that you use any of these functions and instead rely on the
-- AutoStorable types and mechanisms from the other modules to generate
-- 'Storable' instances, but these functions can be generally useful to those
-- that want to write 'Storable' instances by hand.
module Foreign.AutoStorable.Utils
( unwrap
, wrap
, sizePadded
, fixPadding
, readBytes
, sizeTagged
, peekShift
, peekTag
, getTag
, pokeShift
, runPeek
, runPoke
, runPokeTagged
, sapp2
, sapp3
, sapp4
, sapp5
, sapp6
, sapp7
, sapp8
, sapp9
) where

import Control.Monad.State.Strict
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types

-- | Helper function for guiding type inference. Extracts the parameterized type
-- from a 'Ptr' type.
--
-- WARNING: This function is 'undefined', do not force the value this function
-- pretends to produce!
unwrap :: Ptr a -> a
unwrap = undefined

-- | Helper function for guiding type inference. Extracts the parameterized type
-- from a 'Ptr' type.
--
-- WARNING: This function is 'undefined', do not force the value this function
-- pretends to produce!
wrap :: a -> Ptr a
wrap = undefined

-- | Given a 'Storable' value and an offset in bytes, produces the updated
-- offset after marshalling the given value. This function includes padding
-- needed to be added to satisfy the given value's alignment constraint.
sizePadded :: Storable a => a -> Int -> Int
sizePadded a i = size + padding
  where
    align = alignment a
    size = i + sizeOf a
    padding = (align - (size `mod` align)) `mod` align

-- | Given a 'Storable' value and the base size of that value in bytes, produces
-- the size of that value after adding padding to fulfill the value's alignment
-- constraint.
fixPadding :: Storable a => a -> Int -> Int
fixPadding a i = i + padding
  where
    align = alignment a
    padding = (align - (i `mod` align)) `mod` align

-- | Given a 'Ptr' to a 'Storable' value, produces the byte sequence obtained
-- from reading through that pointer.
--
-- Note: For little endian architectures individual fields will appear with
-- their bytes in reverse order.
readBytes :: Storable a => Ptr a -> IO [Int]
readBytes ptr = evalStateT
    (replicateM (sizeOf (unwrap ptr)) (fmap fromIntegral (peekShift cuchar)))
    (alignPtr ptr (alignment (unwrap ptr)))

-- Just to give us a proxy to guide type inference
cchar :: CChar
cchar = undefined

-- Just to give us a proxy to guide type inference
cuchar :: CUChar
cuchar = undefined

-- | Given a 'Storable' value, produces its size when succeeded by a 1-byte tag.
-- This size will properly account for alignment constraints assuming the given
-- value's 'Storable' instance also properly accounts for it.
sizeTagged :: Storable a => a -> Int
sizeTagged a = sizeOf a + (sizeOf cchar `max` alignment cchar)

-- | Given a 'Storable' value, produces a 'StateT' action that performs a 'peek'
-- through the 'Ptr' being held in the 'StateT' and then shifting the 'Ptr' by
-- the size of the given value. This action will properly account for alignment
-- constraints for the given value.
--
-- Note: The given value is not forced, and thus can be bottom. It is only used
-- to direct the types.
peekShift :: Storable b => b -> StateT (Ptr a) IO b
peekShift b = do
    ptr <- fmap (\ptr0 -> alignPtr ptr0 (alignment b)) get
    b' <- lift $ peek (castPtr ptr `asTypeOf` wrap b)
    put (ptr `plusPtr` sizeOf b)
    return b'

-- | Given an offset, produces a 'StateT' action that performs a 'peek' through
-- the 'Ptr' being held in the 'StateT' at the given offset as a 'CChar', which
-- is the type used for tagging sum types.
peekTag :: Int -> StateT (Ptr a) IO CChar
peekTag i = do
    ptr <- fmap (\ptr0 -> alignPtr (ptr0 `plusPtr` i) (alignment cchar)) get
    lift (peek ptr)

-- | Given an offset and a 'Ptr', attempts to read the tag at the given offset
-- for the given pointer. The tag is a 'CChar' used to discriminate constructors
-- in a sum type.
getTag :: Int -> Ptr a -> IO CChar
getTag i ptr = runPeek (castPtr ptr) (peekTag i)

-- | Given a 'Storable' value, produces a 'StateT' action that performs a 'poke'
-- through the 'Ptr' being held in the 'StateT' with the given value. It will
-- then shift the 'Ptr' by the size of the given value. This action will
-- properly account for alignment constraints for the given value.
pokeShift :: Storable b => b -> StateT (Ptr a) IO ()
pokeShift b = do
    ptr <- fmap (\ptr0 -> alignPtr ptr0 (alignment b)) get
    lift $ poke (castPtr ptr `asTypeOf` wrap b) b
    put (ptr `plusPtr` sizeOf b)

-- | Runs a 'StateT' action given a 'Ptr' to some 'Storable' type, unmarshalling
-- a value of that type.
runPeek :: (Storable a, Monad m) => Ptr a -> StateT (Ptr a) m a -> m a
runPeek ptr st = evalStateT st (alignPtr ptr (alignment (unwrap ptr)))

-- | Runs a 'StateT' action given a 'Ptr to some 'Storable' type, marshalling a
-- value of that type.
runPoke :: (Storable a, Monad m) => Ptr a -> StateT (Ptr a) m () -> m ()
runPoke ptr st = evalStateT st (alignPtr ptr (alignment (unwrap ptr)))

-- | Given a 'Ptr', a numeric tag, a tag offset, a 'Storable' value, marshal
-- the given value with the tag succeeding it.
runPokeTagged :: (Storable a, Storable b) => Ptr a -> CChar -> Int -> b -> IO ()
runPokeTagged ptr tag offset b = runPoke ptr $ do
    ptr' <- fmap (\ptr0 -> alignPtr ptr0 (alignment b)) get
    lift $ poke (castPtr ptr' `asTypeOf` wrap b) b
    put (ptr' `plusPtr` offset)
    pokeShift tag

sapp2 :: (Storable a, Storable b)
      => (forall x. Storable x => x -> r) -> (r -> r -> r)
      -> a -> b -> r
sapp2 x y a b = x a `y` x b

sapp3 :: (Storable a, Storable b, Storable c)
      => (forall x. Storable x => x -> r) -> (r -> r -> r)
      -> a -> b -> c -> r
sapp3 x y a b c = x a `y` x b `y` x c

sapp4 :: (Storable a, Storable b, Storable c, Storable d)
      => (forall x. Storable x => x -> r) -> (r -> r -> r)
      -> a -> b -> c -> d -> r
sapp4 x y a b c d = x a `y` x b `y` x c `y` x d

sapp5 :: (Storable a, Storable b, Storable c, Storable d, Storable e)
      => (forall x. Storable x => x -> r) -> (r -> r -> r)
      -> a -> b -> c -> d -> e -> r
sapp5 x y a b c d e = x a `y` x b `y` x c `y` x d `y` x e

sapp6 :: (Storable a, Storable b, Storable c, Storable d, Storable e,
          Storable f)
      => (forall x. Storable x => x -> r) -> (r -> r -> r)
      -> a -> b -> c -> d -> e -> f -> r
sapp6 x y a b c d e f = x a `y` x b `y` x c `y` x d `y` x e `y` x f

sapp7 :: (Storable a, Storable b, Storable c, Storable d, Storable e,
          Storable f, Storable g)
      => (forall x. Storable x => x -> r) -> (r -> r -> r)
      -> a -> b -> c -> d -> e -> f -> g -> r
sapp7 x y a b c d e f g = x a `y` x b `y` x c `y` x d `y` x e `y` x f `y` x g

sapp8 :: (Storable a, Storable b, Storable c, Storable d, Storable e,
          Storable f, Storable g, Storable h)
      => (forall x. Storable x => x -> r) -> (r -> r -> r)
      -> a -> b -> c -> d -> e -> f -> g -> h -> r
sapp8 x y a b c d e f g h =
    x a `y` x b `y` x c `y` x d `y` x e `y` x f `y` x g `y` x h

sapp9 :: (Storable a, Storable b, Storable c, Storable d, Storable e,
          Storable f, Storable g, Storable h, Storable i)
      => (forall x. Storable x => x -> r) -> (r -> r -> r)
      -> a -> b -> c -> d -> e -> f -> g -> h -> i -> r
sapp9 x y a b c d e f g h i =
    x a `y` x b `y` x c `y` x d `y` x e `y` x f `y` x g `y` x h `y` x i
