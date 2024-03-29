-- Some tests for module Combinatorial
--
-- To run all tests automatically by the currycheck tool, use the command:
-- "curry-check TestCombinatorial"

import Data.List ( nub, sort )
import Test.Prop

import Combinatorial

------------------------------------------------------------------

testPermuteEmpty = (permute (tail [1])) <~> []

testPermuteSingle = (permute [1]) <~> [1]

testPermuteTriple =
  (permute [1,2,3])
  <~>  ([1,2,3] ? [2,1,3] ? [2,3,1] ? [1,3,2] ? [3,1,2] ? [3,2,1])


------------------------------------------------------------------

testSubset = 
  (subset [0,1,2,3]) <~>
  ([] ? [3] ? [2] ? [2,3] ? [1] ? [1,3] ? [1,2] ? [1,2,3] ? 
   [0] ? [0,3] ? [0,2] ? [0,2,3] ? [0,1] ? [0,1,3] ? [0,1,2] ? [0,1,2,3])

------------------------------------------------------------------

testSizedSubset = (sizedSubset size base) # (binomial (length base) size)
 where
  base = [0,1,2,3,4,5,6,7]
  size = 4

------------------------------------------------------------------

-- check some results of splitSet
testSplitSet1 =
  splitSet [1..4] ~> (([2,3],[1,4]) ? ([1,2,3],[4]) ? ([],[1,2,3,4]))

-- check number of results of splitSet
testSplitSet2 = splitSet input # 2 ^ (length input)
 where
  input = [1,2,3,4] 

------------------------------------------------------------------

testPartitionNotEmpty = partition [1,2,3,4] `isAlways` all (not . null)

testPartitionMemberLength :: [Int] -> Prop
testPartitionMemberLength xs =
  partition xs `isAlways` (\ys -> length (concat ys) == length xs)

testPartitionContent :: [Int] -> Prop
testPartitionContent xs =
  sort (concat (partition xs)) <~> sort xs

------------------------------------------------------------------

--- The value of `factorial n` is the factorial of `n`.
--- Fails if `n &lt; 0`.
factorial :: Int -> Int
factorial n | n >= 0 = if n == 0 then 1 else n * factorial (n-1)

--- The value of `binomial n m` is `n*(n-1)*...*(n-m+1)/m*(m-1)*...1`.
--- Fails if `m &lt;= 0` or `n &lt; m`.
binomial :: Int -> Int -> Int
binomial n m | m > 0 && n >= m = aux m n `div` factorial m
  where aux x y = if x == 0 then 1 else y * aux (x-1) (y-1)
