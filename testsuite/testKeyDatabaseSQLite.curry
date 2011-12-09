------------------------------------------------------------------------------
--- Some tests for library KeyDatabaseSQLite
---
--- To run all tests automatically by the currytest tool, use the command:
--- "currytest testKeyDatabaseSQLite"
--- 
--- @author Michael Hanus and Sebastian Fischer
--- @version December 2011
------------------------------------------------------------------------------

import Assertion -- for testing
import System
import IO

import KeyDatabaseSQLite

import List ( sortBy )

sort = sortBy (<=)

testPred :: Int -> (String,Int) -> Dynamic
testPred = persistentSQLite "test.db" "test" ["rowid","oid"]

testNotExists =
  assertIO "test notExists" (runQ $ existsDBKey testPred 0) False

testAllKeysEmpty =
  assertIO "test allKeysEmpty" (runQ $ allDBKeys testPred) []

testAllInfosEmpty =
  assertIO "test allInfosEmpty" (runQ $ allDBInfos testPred) []

testAllKeyInfosEmpty =
  assertIO "test allKeyInfosEmpty" (runQ $ allDBKeyInfos testPred) []

testInfoEmpty =
  assertIO "test infoEmpty" 
    (runQ $ getDBInfo testPred 0) Nothing

testInfosEmpty =
  assertIO "test infosEmpty" 
    (runQ $ getDBInfos testPred [0,1,2]) Nothing

testDeleteKeyEmpty =
  assertIO "test deleteKeyEmpty" (runT $ deleteDBEntry testPred 0) (Left ())

testDeleteKeysEmpty =
  assertIO "test deleteKeysEmpty"
    (runT $ deleteDBEntries testPred [0,1,2]) (Left ())

testUpdateEmpty =
  assertIO "test updateEmpty" 
    (runTWithError $ updateDBEntry testPred 0 ("",1)) (Right KeyNotExistsError)

testCreatedExists =
  assertIO "test createdExists" 
    (runT $ (newDBEntry testPred ("new",42) |>>= getDB . existsDBKey testPred))
    (Left True)

testCreatedGoneAfterClean =
  assertIO "test createdGoneAfterClean" 
    (runTWithError
      (newDBEntry testPred ("new",42) |>>= \key ->
       cleanDB testPred |>> getDB (getDBInfo testPred key)))
    (Left Nothing)

testGetAllCreatedKeys =
  assertIO "test getAllCreatedKeys" 
   (runT (mapT (newDBEntry testPred) [("a",10),("b",20),("c",30)] |>>= \keys1 ->
          getDB (allDBKeys testPred) |>>= \keys2 ->
          returnT (sameBag keys1 keys2)))
   (Left True)

testGetAllCreatedInfos =
  assertIO "test getAllCreatedInfos"
    (runT (cleanDB testPred |>>
           mapT (newDBEntry testPred) infos1 |>>
           getDB (allDBInfos testPred) |>>= \infos2 ->
           returnT (sort infos2)))
    (Left infos1)
 where infos1 = [("a",10),("b",20),("c",30)]

testGetAllCreatedKeyInfos =
  assertIO "test getAllCreatedKeyInfos" 
    (runT (cleanDB testPred |>>
           mapT (newDBEntry testPred) infos |>>= \keys ->
           let keyinfos1 = zip keys infos
            in getDB (allDBKeyInfos testPred) |>>= \keyinfos2 ->
               returnT (sameBag keyinfos1 keyinfos2)))
    (Left True)
 where infos = [("a",10),("b",20),("c",30)]

testGetCreatedInfo =
  assertIO "test getCreatedInfo" 
    (runT (cleanDB testPred |>>
           newDBEntry testPred ("new",42) |>>= getDB . getDBInfo testPred))
    (Left (Just ("new",42)))

testGetCreatedInfos =
  assertIO "test getCreatedInfos" 
    (runT (cleanDB testPred |>>
           mapT (newDBEntry testPred) infos |>>= \keys ->
           getDB (getDBInfos testPred keys)))
    (Left (Just infos))
 where infos = [("a",10),("b",20),("c",30)]

testDeleteOneCreated =
  assertIO "test deleteOneCreated" 
   (runT (cleanDB testPred |>>
          mapT (newDBEntry testPred) [("a",10),("b",20),("c",30)] |>>= \keys ->
          deleteDBEntry testPred (keys!!1) |>>
          getDB (allDBInfos testPred) |>>= \infos ->
          returnT (sort infos)))
   (Left [("a",10),("c",30)])

testDeleteAllCreated =
  assertIO "test deleteAllCreated" 
   (runT (cleanDB testPred |>>
          mapT (newDBEntry testPred) [("a",10),("b",20),("c",30)] |>>= \keys ->
          deleteDBEntries testPred keys |>>
          getDB (allDBKeys testPred)))
   (Left [])

testUpdateCreated =
  assertIO "test updateCreated" 
   (runT (cleanDB testPred |>>
          newDBEntry testPred ("old",41) |>>= \key ->
          updateDBEntry testPred key ("new",42) |>>
          getDB (getDBInfo testPred key)))
   (Left (Just ("new",42)))

testQueryDeleted =
  assertIO "test queryDeleted" 
   (runT (cleanDB testPred |>>
          newDBEntry testPred ("new",42) |>>= \key ->
          deleteDBEntry testPred key |>>
          getDB (getDBInfo testPred key)))
   (Left Nothing)

testQueryListWithOneDeleted =
  assertIO "test queryListWithOneDeleted" 
   (runT (cleanDB testPred |>>
          mapT (newDBEntry testPred) [("a",10),("b",20),("c",30)] |>>= \keys ->
          deleteDBEntry testPred (keys!!1) |>>
          getDB (getDBInfos testPred keys)))
   (Left Nothing)

testRollbackOnError =
  assertIO "test rollbackOnError" 
   (runT (cleanDB testPred) >>
    runTWithError (cleanDB testPred |>>
                   newDBEntry testPred ("new",42) |>> transError))
   (Right ExecutionError)

testEmptyAfterRollback =
  assertIO "test emptyAfterRollback" (runQ $ allDBKeys testPred) []

transError :: Transaction ()
transError = error "transaction error"

-- finalize:
testFinal = assertIO "clean up" (system "rm -f test.db") 0

-- Auxiliaries:

runTWithError trans =
  runT trans >>= return . either Left (\ (TError tkind _) -> Right tkind)

sameBag :: [a] -> [a] -> Bool
sameBag xs ys = sort xs == sort ys

