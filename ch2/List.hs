module List (suffixes, SuffixTest, suffixTests) 
  where 

import Data.List
import Test.QuickCheck

suffixes :: [a] -> [[a]]
suffixes []          = [[]]
suffixes list@(_:xs) = list : suffixes xs 

type SuffixTest a = [[a]] -> Bool

suffixTests :: IO ()
suffixTests = mapM_ quickCheck tests
  where
    tests :: [SuffixTest Char]
    tests = [prop_finite, prop_len, prop_suffix]

prop_len :: SuffixTest a
prop_len xs = length xs + 1 == length (suffixes xs)

prop_finite :: SuffixTest a
prop_finite = (all (\_ -> True)) . suffixes

prop_suffix :: Eq a => SuffixTest a
prop_suffix xs = all (\(x,y) -> isSuffixOf x y) (zip (tail suff) suff)
  where
    suff = suffixes xs