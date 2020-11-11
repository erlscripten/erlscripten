module Test.Main where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect.Aff (launchAff_, Aff)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Data.Tuple as T
import Data.Array as A
import Partial.Unsafe
import Erlang.Type
import Lists

exec :: ErlangFun -> Array ErlangTerm -> Aff ErlangTerm
exec fun args = liftEffect (unsafePartial (fun args))

test_reverse a = do
    let input = arrayToErlangList $ map ErlangNum a
    let output = arrayToErlangList $ map ErlangNum (A.reverse a)
    res <- exec erlps__reverse__1 [input]
    output `shouldEqual` res

test_sort a = do
    let input = arrayToErlangList $ map ErlangNum a
    let output = arrayToErlangList $ map ErlangNum (A.sort a)
    res <- exec erlps__sort__1 [input]
    output `shouldEqual` res

test_map a ef pf = do
    let input = arrayToErlangList $ map ErlangNum a
    let output = arrayToErlangList $ map ErlangNum (map pf a)
    res <- exec erlps__map__2 [ef, input]
    output `shouldEqual` res

test_zip a b = do
    let input_a = arrayToErlangList $ map ErlangNum a
    let input_b = arrayToErlangList $ map ErlangNum b
    let output = arrayToErlangList $ map (\(T.Tuple x y) -> ErlangTuple [x,y]) (A.zip (map ErlangNum a) (map ErlangNum b))
    res <- exec erlps__zip__2 [input_a, input_b]
    output `shouldEqual` res

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
    describe "Sanity check" do
        it "one should equal one" do
            1 `shouldEqual` 1
        it "two should equal two" do
            2 `shouldEqual` 2
    describe "STDLIB Lists" do
        it "reverse/1" do
            test_reverse [1,2,3,4,5,6,7,8,9,10]
            test_reverse [1]
            test_reverse []
            test_reverse [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
        it "sort/1" do
            test_sort [1,2,3,4,5,6,7,8,9,10]
            test_sort [10,9,8,7,6,5,4,3,2,1]
            test_sort [5,3,34,6,2,5,7565,4,3,7,8,5,3]
        it "map/1" do
            test_map [1,2,3,4,5] (ErlangFun 1 (\[ErlangNum a] -> pure $ ErlangNum (a*20))) (\x -> x*20)
            test_map [1,2,3,4,5] (ErlangFun 1 (\[ErlangNum a] -> pure $ ErlangNum (-a))) (\x -> -x)
        it "zip/2" do
            test_zip [1,2,3,4] [4,3,2,1]
            test_zip [1,2,7,4] [1,3,2,1]
