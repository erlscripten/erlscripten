module Test.Main where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Unsafe
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect.Aff
import Effect.Exception(catchException)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual, expectError)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Data.Either
import Data.Tuple as T
import Data.Array as A
import Partial.Unsafe
import Erlang.Type
import Erlang.Helpers (unsafePerformEffectGuard2)
import Lists
import Lambdas

exec :: ErlangFun -> Array ErlangTerm -> Aff ErlangTerm
exec fun args = liftEffect (unsafePartial (fun args)) 

--expectError
--  :: forall m t
--   . MonadError Error m
--  => m t
--  -> m Unit
--expectError a = do
--  e <- try a
--  case e of
--    Left _ -> pure unit
--    Right _ -> throwError $ error "Expected error"

exec_may_throw :: ErlangFun -> Array ErlangTerm -> ErlangTerm
exec_may_throw fun args =
     unsafePartial $ ((unsafePerformEffectGuard2 fun) args)

isRight (Right a) e | a==e = true
isRight _ _ = false

isLeft (Left _) = true
isLeft _ = false

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

    describe "Lambdas" do
        --it "can be called" do
        --    r <- exec_may_throw erlps__test_can_be_called__0 []
        --    true `shouldEqual` (isRight r (ErlangAtom "ok"))
        --it "fun can be treated as lambda" do
        --    r <- exec_may_throw erlps__test_can_pass_fun__0 []
        --    true `shouldEqual` (isRight r (ErlangAtom "ok"))
        it "Lambda clauses overwrite vars in scope" do
            let r = exec_may_throw erlps__test_match_semantics_1__0 []
            ErlangAtom "ok" `shouldEqual` r
        --it "Lambdas have access to vars from the enclosing scope" do
        --    r <- exec_may_throw erlps__test_match_semantics_2__0 []
        --    true `shouldEqual` (isRight r (ErlangAtom "ok"))
        --it "Does not leak scope 1" do
        --    r <- exec_may_throw erlps__test_scope_does_not_leak_1__0 []
        --    true `shouldEqual` (isRight r (ErlangAtom "ok"))
        --it "Does not leak scope 2" do
        --    r <- exec_may_throw erlps__test_scope_does_not_leak_2__0 []
        --    true `shouldEqual` (isRight r (ErlangAtom "ok"))

