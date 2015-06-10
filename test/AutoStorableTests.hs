{-
 - AutoStorableTests.hs
 - By Steven Smith
 -}

import Test.Tasty
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.List
import Foreign.Storable
import Foreign.Marshal.Utils (with)

import qualified Test.Tasty.QuickCheck as QC

import Foreign.AutoStorable
import Foreign.AutoStorable.Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [tTests, sTests]

tTests :: TestTree
tTests = testGroup "T (Tuples)" [tMarshallingTests]

tMarshallingTests :: TestTree
tMarshallingTests = testGroup "Marshalling"
    [ QC.testProperty "TestT1a" $ testT1aMarshalTest
    , QC.testProperty "TestT2a" $ testT2aMarshalTest
    , QC.testProperty "TestT3a" $ testT3aMarshalTest
    , QC.testProperty "TestT4a" $ testT4aMarshalTest
    , QC.testProperty "TestT5a" $ testT5aMarshalTest
    , QC.testProperty "TestT6a" $ testT6aMarshalTest
    , QC.testProperty "TestT7a" $ testT7aMarshalTest
    , QC.testProperty "TestT8a" $ testT8aMarshalTest
    ]

sTests :: TestTree
sTests = testGroup "S (Sum)" [sMarshallingTests]

sMarshallingTests :: TestTree
sMarshallingTests = testGroup "Marshalling"
    [ testS2aMarshalTests
    , testS3aMarshalTests
    , testS4aMarshalTests
    , testS5aMarshalTests
    , testS6aMarshalTests
    , testS7aMarshalTests
    , testS8aMarshalTests
    ]

testS2aMarshalTests :: TestTree
testS2aMarshalTests = testGroup "TestS2a"
    [ QC.testProperty "a" $ testS2aaMarshalTest
    , QC.testProperty "b" $ testS2abMarshalTest
    ]

testS3aMarshalTests :: TestTree
testS3aMarshalTests = testGroup "TestS3a"
    [ QC.testProperty "a" $ testS3aaMarshalTest
    , QC.testProperty "b" $ testS3abMarshalTest
    , QC.testProperty "c" $ testS3acMarshalTest
    ]

testS4aMarshalTests :: TestTree
testS4aMarshalTests = testGroup "TestS4a"
    [ QC.testProperty "a" $ testS4aaMarshalTest
    , QC.testProperty "b" $ testS4abMarshalTest
    , QC.testProperty "c" $ testS4acMarshalTest
    , QC.testProperty "d" $ testS4adMarshalTest
    ]

testS5aMarshalTests :: TestTree
testS5aMarshalTests = testGroup "TestS5a"
    [ QC.testProperty "a" $ testS5aaMarshalTest
    , QC.testProperty "b" $ testS5abMarshalTest
    , QC.testProperty "c" $ testS5acMarshalTest
    , QC.testProperty "d" $ testS5adMarshalTest
    , QC.testProperty "e" $ testS5aeMarshalTest
    ]

testS6aMarshalTests :: TestTree
testS6aMarshalTests = testGroup "TestS6a"
    [ QC.testProperty "a" $ testS6aaMarshalTest
    , QC.testProperty "b" $ testS6abMarshalTest
    , QC.testProperty "c" $ testS6acMarshalTest
    , QC.testProperty "d" $ testS6adMarshalTest
    , QC.testProperty "e" $ testS6aeMarshalTest
    , QC.testProperty "f" $ testS6afMarshalTest
    ]

testS7aMarshalTests :: TestTree
testS7aMarshalTests = testGroup "TestS7a"
    [ QC.testProperty "a" $ testS7aaMarshalTest
    , QC.testProperty "b" $ testS7abMarshalTest
    , QC.testProperty "c" $ testS7acMarshalTest
    , QC.testProperty "d" $ testS7adMarshalTest
    , QC.testProperty "e" $ testS7aeMarshalTest
    , QC.testProperty "f" $ testS7afMarshalTest
    , QC.testProperty "g" $ testS7agMarshalTest
    ]

testS8aMarshalTests :: TestTree
testS8aMarshalTests = testGroup "TestS8a"
    [ QC.testProperty "a" $ testS8aaMarshalTest
    , QC.testProperty "b" $ testS8abMarshalTest
    , QC.testProperty "c" $ testS8acMarshalTest
    , QC.testProperty "d" $ testS8adMarshalTest
    , QC.testProperty "e" $ testS8aeMarshalTest
    , QC.testProperty "f" $ testS8afMarshalTest
    , QC.testProperty "g" $ testS8agMarshalTest
    , QC.testProperty "h" $ testS8ahMarshalTest
    ]

-- T1 tests

data TestT1a = TestT1a Char
    deriving (Eq)

testT1aIso = defineIso to from
  where
    to (TestT1a c) = T1 c
    from (T1 c) = TestT1a c

instance Storable TestT1a where
    sizeOf = testT1aIso autoSizeOf
    alignment = testT1aIso autoAlignment
    peek = testT1aIso autoPeek
    poke = testT1aIso autoPoke

testT1aMarshalTest :: Char -> Property
testT1aMarshalTest c = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestT1a c

-- T2 tests

data TestT2a = TestT2a Char Int
    deriving (Eq)

testT2aIso = defineIso to from
  where
    to (TestT2a c i) = T2 c i
    from (T2 c i) = TestT2a c i

instance Storable TestT2a where
    sizeOf = testT2aIso autoSizeOf
    alignment = testT2aIso autoAlignment
    peek = testT2aIso autoPeek
    poke = testT2aIso autoPoke

testT2aMarshalTest :: Char -> Int -> Property
testT2aMarshalTest c i = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestT2a c i

-- T3 tests

data TestT3a = TestT3a Char Int Bool
    deriving (Eq)

testT3aIso = defineIso to from
  where
    to (TestT3a c i b) = T3 c i b
    from (T3 c i b) = TestT3a c i b

instance Storable TestT3a where
    sizeOf = testT3aIso autoSizeOf
    alignment = testT3aIso autoAlignment
    peek = testT3aIso autoPeek
    poke = testT3aIso autoPoke

testT3aMarshalTest :: Char -> Int -> Bool -> Property
testT3aMarshalTest c i b = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestT3a c i b

-- T4 tests

data TestT4a = TestT4a Char Int Bool Float
    deriving (Eq)

testT4aIso = defineIso to from
  where
    to (TestT4a c i b f) = T4 c i b f
    from (T4 c i b f) = TestT4a c i b f

instance Storable TestT4a where
    sizeOf = testT4aIso autoSizeOf
    alignment = testT4aIso autoAlignment
    peek = testT4aIso autoPeek
    poke = testT4aIso autoPoke

testT4aMarshalTest :: Char -> Int -> Bool -> Float -> Property
testT4aMarshalTest c i b f = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestT4a c i b f

-- T5 tests

data TestT5a = TestT5a Char Int Bool Float Char
    deriving (Eq)

testT5aIso = defineIso to from
  where
    to (TestT5a c1 i b f c2) = T5 c1 i b f c2
    from (T5 c1 i b f c2) = TestT5a c1 i b f c2

instance Storable TestT5a where
    sizeOf = testT5aIso autoSizeOf
    alignment = testT5aIso autoAlignment
    peek = testT5aIso autoPeek
    poke = testT5aIso autoPoke

testT5aMarshalTest :: Char -> Int -> Bool -> Float -> Char -> Property
testT5aMarshalTest c1 i b f c2 = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestT5a c1 i b f c2

-- T6 tests

data TestT6a = TestT6a Char Int Bool Float Char Int
    deriving (Eq)

testT6aIso = defineIso to from
  where
    to (TestT6a c1 i1 b f c2 i2) = T6 c1 i1 b f c2 i2
    from (T6 c1 i1 b f c2 i2) = TestT6a c1 i1 b f c2 i2

instance Storable TestT6a where
    sizeOf = testT6aIso autoSizeOf
    alignment = testT6aIso autoAlignment
    peek = testT6aIso autoPeek
    poke = testT6aIso autoPoke

testT6aMarshalTest :: Char -> Int -> Bool -> Float -> Char -> Int -> Property
testT6aMarshalTest c1 i1 b f c2 i2 = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestT6a c1 i1 b f c2 i2

-- T7 tests

data TestT7a = TestT7a Char Int Bool Float Char Int Bool
    deriving (Eq)

testT7aIso = defineIso to from
  where
    to (TestT7a c1 i1 b1 f c2 i2 b2) = T7 c1 i1 b1 f c2 i2 b2
    from (T7 c1 i1 b1 f c2 i2 b2) = TestT7a c1 i1 b1 f c2 i2 b2

instance Storable TestT7a where
    sizeOf = testT7aIso autoSizeOf
    alignment = testT7aIso autoAlignment
    peek = testT7aIso autoPeek
    poke = testT7aIso autoPoke

testT7aMarshalTest :: Char -> Int -> Bool -> Float -> Char -> Int -> Bool
                   -> Property
testT7aMarshalTest c1 i1 b1 f c2 i2 b2 = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestT7a c1 i1 b1 f c2 i2 b2

-- T8 tests

data TestT8a = TestT8a Char Int Bool Float Char Int Bool Float
    deriving (Eq)

testT8aIso = defineIso to from
  where
    to (TestT8a c1 i1 b1 f1 c2 i2 b2 f2) = T8 c1 i1 b1 f1 c2 i2 b2 f2
    from (T8 c1 i1 b1 f1 c2 i2 b2 f2) = TestT8a c1 i1 b1 f1 c2 i2 b2 f2

instance Storable TestT8a where
    sizeOf = testT8aIso autoSizeOf
    alignment = testT8aIso autoAlignment
    peek = testT8aIso autoPeek
    poke = testT8aIso autoPoke

testT8aMarshalTest :: Char -> Int -> Bool -> Float -> Char -> Int -> Bool
                   -> Float -> Property
testT8aMarshalTest c1 i1 b1 f1 c2 i2 b2 f2 = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestT8a c1 i1 b1 f1 c2 i2 b2 f2

-- S2 tests

data TestS2a = TestS2aa Char
             | TestS2ab Int
    deriving (Eq)

testS2aIso = defineIso to from
  where
    to (TestS2aa c) = S2a c
    to (TestS2ab i) = S2b i
    from (S2a c) = TestS2aa c
    from (S2b i) = TestS2ab i

instance Storable TestS2a where
    sizeOf = testS2aIso autoSizeOf
    alignment = testS2aIso autoAlignment
    peek = testS2aIso autoPeek
    poke = testS2aIso autoPoke

testS2aaMarshalTest :: Char -> Property
testS2aaMarshalTest c = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS2aa c

testS2abMarshalTest :: Int -> Property
testS2abMarshalTest i = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS2ab i

-- S3 tests

data TestS3a = TestS3aa Char
             | TestS3ab Int
             | TestS3ac Bool
    deriving (Eq)

testS3aIso = defineIso to from
  where
    to (TestS3aa c) = S3a c
    to (TestS3ab i) = S3b i
    to (TestS3ac b) = S3c b
    from (S3a c) = TestS3aa c
    from (S3b i) = TestS3ab i
    from (S3c b) = TestS3ac b

instance Storable TestS3a where
    sizeOf = testS3aIso autoSizeOf
    alignment = testS3aIso autoAlignment
    peek = testS3aIso autoPeek
    poke = testS3aIso autoPoke

testS3aaMarshalTest :: Char -> Property
testS3aaMarshalTest c = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS3aa c

testS3abMarshalTest :: Int -> Property
testS3abMarshalTest i = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS3ab i

testS3acMarshalTest :: Bool -> Property
testS3acMarshalTest b = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS3ac b

-- S4 tests

data TestS4a = TestS4aa Char
             | TestS4ab Int
             | TestS4ac Bool
             | TestS4ad Float
    deriving (Eq)

testS4aIso = defineIso to from
  where
    to (TestS4aa c) = S4a c
    to (TestS4ab i) = S4b i
    to (TestS4ac b) = S4c b
    to (TestS4ad f) = S4d f
    from (S4a c) = TestS4aa c
    from (S4b i) = TestS4ab i
    from (S4c b) = TestS4ac b
    from (S4d f) = TestS4ad f

instance Storable TestS4a where
    sizeOf = testS4aIso autoSizeOf
    alignment = testS4aIso autoAlignment
    peek = testS4aIso autoPeek
    poke = testS4aIso autoPoke

testS4aaMarshalTest :: Char -> Property
testS4aaMarshalTest c = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS4aa c

testS4abMarshalTest :: Int -> Property
testS4abMarshalTest i = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS4ab i

testS4acMarshalTest :: Bool -> Property
testS4acMarshalTest b = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS4ac b

testS4adMarshalTest :: Float -> Property
testS4adMarshalTest f = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS4ad f

-- S5 tests

data TestS5a = TestS5aa Char
             | TestS5ab Int
             | TestS5ac Bool
             | TestS5ad Float
             | TestS5ae Char
    deriving (Eq)

testS5aIso = defineIso to from
  where
    to (TestS5aa c) = S5a c
    to (TestS5ab i) = S5b i
    to (TestS5ac b) = S5c b
    to (TestS5ad f) = S5d f
    to (TestS5ae c) = S5e c
    from (S5a c) = TestS5aa c
    from (S5b i) = TestS5ab i
    from (S5c b) = TestS5ac b
    from (S5d f) = TestS5ad f
    from (S5e c) = TestS5ae c

instance Storable TestS5a where
    sizeOf = testS5aIso autoSizeOf
    alignment = testS5aIso autoAlignment
    peek = testS5aIso autoPeek
    poke = testS5aIso autoPoke

testS5aaMarshalTest :: Char -> Property
testS5aaMarshalTest c = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS5aa c

testS5abMarshalTest :: Int -> Property
testS5abMarshalTest i = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS5ab i

testS5acMarshalTest :: Bool -> Property
testS5acMarshalTest b = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS5ac b

testS5adMarshalTest :: Float -> Property
testS5adMarshalTest f = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS5ad f

testS5aeMarshalTest :: Char -> Property
testS5aeMarshalTest c = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS5ae c

-- S6 tests

data TestS6a = TestS6aa Char
             | TestS6ab Int
             | TestS6ac Bool
             | TestS6ad Float
             | TestS6ae Char
             | TestS6af Int
    deriving (Eq)

testS6aIso = defineIso to from
  where
    to (TestS6aa c) = S6a c
    to (TestS6ab i) = S6b i
    to (TestS6ac b) = S6c b
    to (TestS6ad f) = S6d f
    to (TestS6ae c) = S6e c
    to (TestS6af i) = S6f i
    from (S6a c) = TestS6aa c
    from (S6b i) = TestS6ab i
    from (S6c b) = TestS6ac b
    from (S6d f) = TestS6ad f
    from (S6e c) = TestS6ae c
    from (S6f i) = TestS6af i

instance Storable TestS6a where
    sizeOf = testS6aIso autoSizeOf
    alignment = testS6aIso autoAlignment
    peek = testS6aIso autoPeek
    poke = testS6aIso autoPoke

testS6aaMarshalTest :: Char -> Property
testS6aaMarshalTest c = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS6aa c

testS6abMarshalTest :: Int -> Property
testS6abMarshalTest i = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS6ab i

testS6acMarshalTest :: Bool -> Property
testS6acMarshalTest b = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS6ac b

testS6adMarshalTest :: Float -> Property
testS6adMarshalTest f = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS6ad f

testS6aeMarshalTest :: Char -> Property
testS6aeMarshalTest c = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS6ae c

testS6afMarshalTest :: Int -> Property
testS6afMarshalTest i = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS6af i

-- S7 tests

data TestS7a = TestS7aa Char
             | TestS7ab Int
             | TestS7ac Bool
             | TestS7ad Float
             | TestS7ae Char
             | TestS7af Int
             | TestS7ag Bool
    deriving (Eq)

testS7aIso = defineIso to from
  where
    to (TestS7aa c) = S7a c
    to (TestS7ab i) = S7b i
    to (TestS7ac b) = S7c b
    to (TestS7ad f) = S7d f
    to (TestS7ae c) = S7e c
    to (TestS7af i) = S7f i
    to (TestS7ag b) = S7g b
    from (S7a c) = TestS7aa c
    from (S7b i) = TestS7ab i
    from (S7c b) = TestS7ac b
    from (S7d f) = TestS7ad f
    from (S7e c) = TestS7ae c
    from (S7f i) = TestS7af i
    from (S7g b) = TestS7ag b

instance Storable TestS7a where
    sizeOf = testS7aIso autoSizeOf
    alignment = testS7aIso autoAlignment
    peek = testS7aIso autoPeek
    poke = testS7aIso autoPoke

testS7aaMarshalTest :: Char -> Property
testS7aaMarshalTest c = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS7aa c

testS7abMarshalTest :: Int -> Property
testS7abMarshalTest i = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS7ab i

testS7acMarshalTest :: Bool -> Property
testS7acMarshalTest b = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS7ac b

testS7adMarshalTest :: Float -> Property
testS7adMarshalTest f = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS7ad f

testS7aeMarshalTest :: Char -> Property
testS7aeMarshalTest c = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS7ae c

testS7afMarshalTest :: Int -> Property
testS7afMarshalTest i = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS7af i

testS7agMarshalTest :: Bool -> Property
testS7agMarshalTest b = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS7ag b

-- S8 tests

data TestS8a = TestS8aa Char
             | TestS8ab Int
             | TestS8ac Bool
             | TestS8ad Float
             | TestS8ae Char
             | TestS8af Int
             | TestS8ag Bool
             | TestS8ah Float
    deriving (Eq)

testS8aIso = defineIso to from
  where
    to (TestS8aa c) = S8a c
    to (TestS8ab i) = S8b i
    to (TestS8ac b) = S8c b
    to (TestS8ad f) = S8d f
    to (TestS8ae c) = S8e c
    to (TestS8af i) = S8f i
    to (TestS8ag b) = S8g b
    to (TestS8ah f) = S8h f
    from (S8a c) = TestS8aa c
    from (S8b i) = TestS8ab i
    from (S8c b) = TestS8ac b
    from (S8d f) = TestS8ad f
    from (S8e c) = TestS8ae c
    from (S8f i) = TestS8af i
    from (S8g b) = TestS8ag b
    from (S8h f) = TestS8ah f

instance Storable TestS8a where
    sizeOf = testS8aIso autoSizeOf
    alignment = testS8aIso autoAlignment
    peek = testS8aIso autoPeek
    poke = testS8aIso autoPoke

testS8aaMarshalTest :: Char -> Property
testS8aaMarshalTest c = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS8aa c

testS8abMarshalTest :: Int -> Property
testS8abMarshalTest i = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS8ab i

testS8acMarshalTest :: Bool -> Property
testS8acMarshalTest b = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS8ac b

testS8adMarshalTest :: Float -> Property
testS8adMarshalTest f = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS8ad f

testS8aeMarshalTest :: Char -> Property
testS8aeMarshalTest c = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS8ae c

testS8afMarshalTest :: Int -> Property
testS8afMarshalTest i = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS8af i

testS8agMarshalTest :: Bool -> Property
testS8agMarshalTest b = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS8ag b

testS8ahMarshalTest :: Float -> Property
testS8ahMarshalTest f = monadicIO $ do
    testData' <- run (with testData peek)
    assert (testData == testData')
  where
    testData = TestS8ah f
