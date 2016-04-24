-- Some tests for module Sort
--
-- To run all tests automatically by the currycheck tool, use the command:
-- "currycheck testSort"

import Sort
import Test.EasyCheck

testMergeSort1 = [1,3,3,4,5,6,7,7,8,23,32,35] -=-
                 (mergeSortBy (<=) [3,1,5,3,7,4,32,8,35,7,23,6])

testMergeSort2 = let n = 10000 in [1..n] -=- (mergeSortBy (<=) [1..n])

testMergeSort3 = let n = 10000 in [1..n] -=- (mergeSortBy (<=) [n,n-1..1])


testQuickSort =
  (quickSortBy leqString ["zx","zz","ad","cd","hf","he","hff"])
  -=- ["ad","cd","he","hf","hff","zx","zz"]
