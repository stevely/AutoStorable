{-
 - Types.hs
 - By Steven Smith
 -}

-- | This module contains the AutoStorable types. They are essentially
-- special-purpose product and sum types. If you can make your data type into
-- some combination of these types and each component is an instance of
-- 'Storable', then you can use the AutoStorable functions to generate a
-- 'Storable' instance for your data type.
module Foreign.AutoStorable.Types (
T1 (..),
T2 (..),
T3 (..),
T4 (..),
T5 (..),
T6 (..),
T7 (..),
T8 (..),
S2 (..),
S3 (..),
S4 (..),
S5 (..),
S6 (..),
S7 (..),
S8 (..)
) where

import Control.Applicative
import Control.Arrow ((>>>))
import Foreign.Storable

import Foreign.AutoStorable.Utils

-- | A 1-element tuple. I don't know of an actual use for this, but it is
-- included for completeness.
data T1 a = T1 a
    deriving (Show, Eq)

-- | A 2-element tuple
data T2 a b = T2 a b
    deriving (Show, Eq)

-- | A 3-element tuple
data T3 a b c = T3 a b c
    deriving (Show, Eq)

-- | A 4-element tuple
data T4 a b c d = T4 a b c d
    deriving (Show, Eq)

-- | A 5-element tuple
data T5 a b c d e = T5 a b c d e
    deriving (Show, Eq)

-- | A 6-element tuple
data T6 a b c d e f = T6 a b c d e f
    deriving (Show, Eq)

-- | A 7-element tuple
data T7 a b c d e f g = T7 a b c d e f g
    deriving (Show, Eq)

-- | An 8-element tuple
data T8 a b c d e f g h = T8 a b c d e f g h
    deriving (Show, Eq)

-- No S1 because that would just be T1
-- | A 2-element sum type
data S2 a b = S2a a | S2b b
    deriving (Show, Eq)

-- | A 3-element sum type
data S3 a b c = S3a a | S3b b | S3c c
    deriving (Show, Eq)

-- | A 4-element sum type
data S4 a b c d = S4a a | S4b b | S4c c | S4d d
    deriving (Show, Eq)

-- | A 5-element sum type
data S5 a b c d e = S5a a | S5b b | S5c c | S5d d | S5e e
    deriving (Show, Eq)

-- | A 6-element sum type
data S6 a b c d e f = S6a a | S6b b | S6c c | S6d d | S6e e | S6f f
    deriving (Show, Eq)

-- | A 7-element sum type
data S7 a b c d e f g = S7a a | S7b b | S7c c | S7d d | S7e e | S7f f | S7g g
    deriving (Show, Eq)

-- | An 8-element sum type
data S8 a b c d e f g h =
    S8a a | S8b b | S8c c | S8d d | S8e e | S8f f | S8g g | S8h h
    deriving (Show, Eq)

split2 :: S2 a b -> T2 a b
split2 = undefined

split3 :: S3 a b c -> T3 a b c
split3 = undefined

split4 :: S4 a b c d -> T4 a b c d
split4 = undefined

split5 :: S5 a b c d e -> T5 a b c d e
split5 = undefined

split6 :: S6 a b c d e f -> T6 a b c d e f
split6 = undefined

split7 :: S7 a b c d e f g -> T7 a b c d e f g
split7 = undefined

split8 :: S8 a b c d e f g h -> T8 a b c d e f g h
split8 = undefined

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
    sizeOf ~t@(T2 a b) = fixPadding t . sapp2 sizePadded (>>>) a b $ 0
    alignment ~(T2 a b) = sapp2 alignment max a b
    peek ptr = runPeek ptr $ T2
        <$> peekShift a
        <*> peekShift b
      where
        ~(T2 a b) = unwrap ptr
    poke ptr (T2 a b) = runPoke ptr $ do
        pokeShift a
        pokeShift b

instance (Storable a, Storable b, Storable c) => Storable (T3 a b c) where
    sizeOf ~t@(T3 a b c) = fixPadding t . sapp3 sizePadded (>>>) a b c $ 0
    alignment ~(T3 a b c) = sapp3 alignment max a b c
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
    sizeOf ~t@(T4 a b c d) = fixPadding t . sapp4 sizePadded (>>>) a b c d $ 0
    alignment ~(T4 a b c d) = sapp4 alignment max a b c d
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
    sizeOf ~t@(T5 a b c d e) =
        fixPadding t . sapp5 sizePadded (>>>) a b c d e $ 0
    alignment ~(T5 a b c d e) = sapp5 alignment max a b c d e
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
    sizeOf ~t@(T6 a b c d e f) =
        fixPadding t . sapp6 sizePadded (>>>) a b c d e f $ 0
    alignment ~(T6 a b c d e f) = sapp6 alignment max a b c d e f
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
    sizeOf ~t@(T7 a b c d e f g) =
        fixPadding t . sapp7 sizePadded (>>>) a b c d e f g $ 0
    alignment ~(T7 a b c d e f g) = sapp7 alignment max a b c d e f g
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
    sizeOf ~t@(T8 a b c d e f g h) =
        fixPadding t . sapp8 sizePadded (>>>) a b c d e f g h $ 0
    alignment ~(T8 a b c d e f g h) = sapp8 alignment max a b c d e f g h
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

instance (Storable a, Storable b) => Storable (S2 a b) where
    sizeOf s = fixPadding s $ sapp2 sizeTagged max a b
      where
        ~(T2 a b) = split2 s
    alignment s = sapp2 alignment max a b
      where
        ~(T2 a b) = split2 s
    peek ptr = runPeek ptr $ do
        tag <- peekTag tagOffset
        case tag of
            0 -> S2a <$> peekShift a
            _ -> S2b <$> peekShift b
      where
        ~(T2 a b) = split2 (unwrap ptr)
        tagOffset = sapp2 sizeOf max a b
    poke ptr s = case s of
        S2a a -> runPokeTagged ptr 0 tagOffset a
        S2b b -> runPokeTagged ptr 1 tagOffset b
      where
        ~(T2 a' b') = split2 (unwrap ptr)
        tagOffset = sapp2 sizeOf max a' b'

instance (Storable a, Storable b, Storable c) => Storable (S3 a b c) where
    sizeOf s = fixPadding s $ sapp3 sizeTagged max a b c
      where
        ~(T3 a b c) = split3 s
    alignment s = sapp3 alignment max a b c
      where
        ~(T3 a b c) = split3 s
    peek ptr = runPeek ptr $ do
        tag <- peekTag tagOffset
        case tag of
            0 -> S3a <$> peekShift a
            1 -> S3b <$> peekShift b
            _ -> S3c <$> peekShift c
      where
        ~(T3 a b c) = split3 (unwrap ptr)
        tagOffset = sapp3 sizeOf max a b c
    poke ptr s = case s of
        S3a a -> runPokeTagged ptr 0 tagOffset a
        S3b b -> runPokeTagged ptr 1 tagOffset b
        S3c c -> runPokeTagged ptr 2 tagOffset c
      where
        ~(T3 a' b' c') = split3 (unwrap ptr)
        tagOffset = sapp3 sizeOf max a' b' c'

instance (Storable a, Storable b, Storable c, Storable d)
    => Storable (S4 a b c d) where
    sizeOf s = fixPadding s $ sapp4 sizeTagged max a b c d
      where
        ~(T4 a b c d) = split4 s
    alignment s = sapp4 alignment max a b c d
      where
        ~(T4 a b c d) = split4 s
    peek ptr = runPeek ptr $ do
        tag <- peekTag tagOffset
        case tag of
            0 -> S4a <$> peekShift a
            1 -> S4b <$> peekShift b
            2 -> S4c <$> peekShift c
            _ -> S4d <$> peekShift d
      where
        ~(T4 a b c d) = split4 (unwrap ptr)
        tagOffset = sapp4 sizeOf max a b c d
    poke ptr s = case s of
        S4a a -> runPokeTagged ptr 0 tagOffset a
        S4b b -> runPokeTagged ptr 1 tagOffset b
        S4c c -> runPokeTagged ptr 2 tagOffset c
        S4d d -> runPokeTagged ptr 3 tagOffset d
      where
        ~(T4 a' b' c' d') = split4 (unwrap ptr)
        tagOffset = sapp4 sizeOf max a' b' c' d'

instance (Storable a, Storable b, Storable c, Storable d, Storable e)
    => Storable (S5 a b c d e) where
    sizeOf s = fixPadding s $ sapp5 sizeTagged max a b c d e
      where
        ~(T5 a b c d e) = split5 s
    alignment s = sapp5 alignment max a b c d e
      where
        ~(T5 a b c d e) = split5 s
    peek ptr = runPeek ptr $ do
        tag <- peekTag tagOffset
        case tag of
            0 -> S5a <$> peekShift a
            1 -> S5b <$> peekShift b
            2 -> S5c <$> peekShift c
            3 -> S5d <$> peekShift d
            _ -> S5e <$> peekShift e
      where
        ~(T5 a b c d e) = split5 (unwrap ptr)
        tagOffset = sapp5 sizeOf max a b c d e
    poke ptr s = case s of
        S5a a -> runPokeTagged ptr 0 tagOffset a
        S5b b -> runPokeTagged ptr 1 tagOffset b
        S5c c -> runPokeTagged ptr 2 tagOffset c
        S5d d -> runPokeTagged ptr 3 tagOffset d
        S5e e -> runPokeTagged ptr 4 tagOffset e
      where
        ~(T5 a' b' c' d' e') = split5 (unwrap ptr)
        tagOffset = sapp5 sizeOf max a' b' c' d' e'

instance
    (Storable a, Storable b, Storable c, Storable d, Storable e, Storable f)
    => Storable (S6 a b c d e f) where
    sizeOf s = fixPadding s $ sapp6 sizeTagged max a b c d e f
      where
        ~(T6 a b c d e f) = split6 s
    alignment s = sapp6 alignment max a b c d e f
      where
        ~(T6 a b c d e f) = split6 s
    peek ptr = runPeek ptr $ do
        tag <- peekTag tagOffset
        case tag of
            0 -> S6a <$> peekShift a
            1 -> S6b <$> peekShift b
            2 -> S6c <$> peekShift c
            3 -> S6d <$> peekShift d
            4 -> S6e <$> peekShift e
            _ -> S6f <$> peekShift f
      where
        ~(T6 a b c d e f) = split6 (unwrap ptr)
        tagOffset = sapp6 sizeOf max a b c d e f
    poke ptr s = case s of
        S6a a -> runPokeTagged ptr 0 tagOffset a
        S6b b -> runPokeTagged ptr 1 tagOffset b
        S6c c -> runPokeTagged ptr 2 tagOffset c
        S6d d -> runPokeTagged ptr 3 tagOffset d
        S6e e -> runPokeTagged ptr 4 tagOffset e
        S6f f -> runPokeTagged ptr 5 tagOffset f
      where
        ~(T6 a' b' c' d' e' f') = split6 (unwrap ptr)
        tagOffset = sapp6 sizeOf max a' b' c' d' e' f'

instance
    (Storable a, Storable b, Storable c, Storable d, Storable e, Storable f,
     Storable g)
    => Storable (S7 a b c d e f g) where
    sizeOf s = fixPadding s $ sapp7 sizeTagged max a b c d e f g
      where
        ~(T7 a b c d e f g) = split7 s
    alignment s = sapp7 alignment max a b c d e f g
      where
        ~(T7 a b c d e f g) = split7 s
    peek ptr = runPeek ptr $ do
        tag <- peekTag tagOffset
        case tag of
            0 -> S7a <$> peekShift a
            1 -> S7b <$> peekShift b
            2 -> S7c <$> peekShift c
            3 -> S7d <$> peekShift d
            4 -> S7e <$> peekShift e
            5 -> S7f <$> peekShift f
            _ -> S7g <$> peekShift g
      where
        ~(T7 a b c d e f g) = split7 (unwrap ptr)
        tagOffset = sapp7 sizeOf max a b c d e f g
    poke ptr s = case s of
        S7a a -> runPokeTagged ptr 0 tagOffset a
        S7b b -> runPokeTagged ptr 1 tagOffset b
        S7c c -> runPokeTagged ptr 2 tagOffset c
        S7d d -> runPokeTagged ptr 3 tagOffset d
        S7e e -> runPokeTagged ptr 4 tagOffset e
        S7f f -> runPokeTagged ptr 5 tagOffset f
        S7g g -> runPokeTagged ptr 6 tagOffset g
      where
        ~(T7 a' b' c' d' e' f' g') = split7 (unwrap ptr)
        tagOffset = sapp7 sizeOf max a' b' c' d' e' f' g'

instance
    (Storable a, Storable b, Storable c, Storable d, Storable e, Storable f,
     Storable g, Storable h)
    => Storable (S8 a b c d e f g h) where
    sizeOf s = fixPadding s $ sapp8 sizeTagged max a b c d e f g h
      where
        ~(T8 a b c d e f g h) = split8 s
    alignment s = sapp8 alignment max a b c d e f g h
      where
        ~(T8 a b c d e f g h) = split8 s
    peek ptr = runPeek ptr $ do
        tag <- peekTag tagOffset
        case tag of
            0 -> S8a <$> peekShift a
            1 -> S8b <$> peekShift b
            2 -> S8c <$> peekShift c
            3 -> S8d <$> peekShift d
            4 -> S8e <$> peekShift e
            5 -> S8f <$> peekShift f
            6 -> S8g <$> peekShift g
            _ -> S8h <$> peekShift h
      where
        ~(T8 a b c d e f g h) = split8 (unwrap ptr)
        tagOffset = sapp8 sizeOf max a b c d e f g h
    poke ptr s = case s of
        S8a a -> runPokeTagged ptr 0 tagOffset a
        S8b b -> runPokeTagged ptr 1 tagOffset b
        S8c c -> runPokeTagged ptr 2 tagOffset c
        S8d d -> runPokeTagged ptr 3 tagOffset d
        S8e e -> runPokeTagged ptr 4 tagOffset e
        S8f f -> runPokeTagged ptr 5 tagOffset f
        S8g g -> runPokeTagged ptr 6 tagOffset g
        S8h h -> runPokeTagged ptr 7 tagOffset h
      where
        ~(T8 a' b' c' d' e' f' g' h') = split8 (unwrap ptr)
        tagOffset = sapp8 sizeOf max a' b' c' d' e' f' g' h'
