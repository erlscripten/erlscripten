module Test.Main where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Unsafe
import Effect.Ref as Ref
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect.Aff hiding (error)
import Effect.Exception(catchException)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual, expectError)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Data.Lazy
import Data.Either
import Data.Tuple as T
import Data.Array as A
import Partial.Unsafe
import Erlang.Type
import Erlang.Exception
import Erlang.Builtins as BIF
import Erlang.Helpers (unsafePerformEffectGuard)
import Lists
import Lambdas
import Records
import Exceptions
import Test.Array
import Array.SUITE

-- BEWARE - HERE BE DRAGONS - I've lost too many hours debugging alternative helpers
-- If you think you can make a better wrapper which does not crash the testing infrastructure then please make a PR
-- If you can replace this helper with something better then please feel free to do so :)
exec_may_throw_aff :: ErlangFun -> Array ErlangTerm -> Aff ErlangTerm
exec_may_throw_aff fun args =
    let
        t = defer $ (\_ -> unsafePerformEffect $ unsafePartial $ fun args)
        f = defer $ (\_ -> ErlangAtom "error")
    in do
        v <- liftEffect (catchException (\_ -> pure f) (pure t))
        pure $ force v

exec_may_throw :: ErlangFun -> Array ErlangTerm -> Aff ErlangTerm
exec_may_throw fun args = do
    res <- attempt $ exec_may_throw_aff fun args
    -- liftEffect $ log $ show res -- Uncomment for logs :)
    case res of
        Left _ -> pure make_err
        Right r -> pure $ make_ok r

make_ok term = ErlangTuple [ErlangAtom "ok", term]
make_err = ErlangAtom "error"

test_reverse a = do
    let input = arrayToErlangList $ map ErlangNum a
    let output = make_ok $ arrayToErlangList $ map ErlangNum (A.reverse a)
    res <- exec_may_throw erlps__reverse__1 [input]
    output `shouldEqual` res

test_sort a = do
    let input = arrayToErlangList $ map ErlangNum a
    let output = make_ok $ arrayToErlangList $ map ErlangNum (A.sort a)
    res <- exec_may_throw erlps__sort__1 [input]
    output `shouldEqual` res

test_map a ef pf = do
    let input = arrayToErlangList $ map ErlangNum a
    let output = make_ok $ arrayToErlangList $ map ErlangNum (map pf a)
    res <- exec_may_throw erlps__map__2 [ef, input]
    output `shouldEqual` res

test_zip_ok a b = do
    let input_a = arrayToErlangList $ map ErlangNum a
    let input_b = arrayToErlangList $ map ErlangNum b
    let output = make_ok $ arrayToErlangList $ map (\(T.Tuple x y) -> ErlangTuple [x,y]) (A.zip (map ErlangNum a) (map ErlangNum b))
    res <- exec_may_throw erlps__zip__2 [input_a, input_b]
    output `shouldEqual` res

test_zip_fail a b = do
    let input_a = arrayToErlangList $ map ErlangNum a
    let input_b = arrayToErlangList $ map ErlangNum b
    res <- exec_may_throw erlps__zip__2 [input_a, input_b]
    make_err `shouldEqual` res

test_seq from to expected = do
    calc <- exec_may_throw erlps__seq__2 [ErlangNum from, ErlangNum to]
    let out = make_ok $ arrayToErlangList $ map ErlangNum expected
    out `shouldEqual` calc

shouldEqualOk a b = make_ok a `shouldEqual` b

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
            test_map [1,2,3,4,5] (ErlangFun 1 (\ [ErlangNum a] -> pure $ ErlangNum (a*20))) (\x -> x*20)
            test_map [1,2,3,4,5] (ErlangFun 1 (\ [ErlangNum a] -> pure $ ErlangNum (-a))) (\x -> -x)
        it "zip/2" do
            test_zip_ok [1,2,3,4] [4,3,2,1]
            test_zip_ok [1,2,7,4] [1,3,2,1]
            test_zip_fail [1] [1,2]
            test_zip_fail [1,2] [1]
        it "seq/2" do
            test_seq 0 0 [0]
            test_seq 1 0 []
            test_seq 1 10 [1,2,3,4,5,6,7,8,9,10]

    describe "STDLIB Array" do
        it "can create zero size array" do
            r <- exec_may_throw erlps__test_create_0__0 []
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "Create a fixed-size array with entries 0-9 set to 'undefined'" do
            r <- exec_may_throw erlps__test_create_1__0 []
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "is functional :)" do
            r <- exec_may_throw erlps__test_functionality__0 []
            make_ok (ErlangAtom "ok") `shouldEqual` r

    describe "Real Array tests taken from OTP - array_SUITE.erl" do
        it "new_test" do
            r <- exec_may_throw erlps__new_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "fix_test" do
            r <- exec_may_throw erlps__fix_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "relax_test" do
            r <- exec_may_throw erlps__relax_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "resize_test" do
            r <- exec_may_throw erlps__resize_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "set_get_test" do
            r <- exec_may_throw erlps__set_get_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "to_list_test" do
            r <- exec_may_throw erlps__to_list_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "sparse_to_list_test" do
            r <- exec_may_throw erlps__sparse_to_list_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "from_list_test" do
            r <- exec_may_throw erlps__from_list_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "to_orddict_test" do
            r <- exec_may_throw erlps__to_orddict_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "sparse_to_orddict_test" do
            r <- exec_may_throw erlps__sparse_to_orddict_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "from_orddict_test" do
            r <- exec_may_throw erlps__from_orddict_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "map_test" do
            r <- exec_may_throw erlps__map_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "sparse_map_test" do
            r <- exec_may_throw erlps__sparse_map_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "foldl_test" do
            r <- exec_may_throw erlps__foldl_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "sparse_foldl_test" do
            r <- exec_may_throw erlps__sparse_foldl_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "foldr_test" do
            r <- exec_may_throw erlps__foldr_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "sparse_foldr_test" do
            r <- exec_may_throw erlps__sparse_foldr_test__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

    describe "Lambdas" do
        it "can be called" do
            r <- exec_may_throw erlps__test_can_be_called__0 []
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "fun can be treated as lambda" do
            r <- exec_may_throw erlps__test_can_pass_fun__0 []
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "Lambda clauses overwrite vars in scope" do
            r <- exec_may_throw erlps__test_match_semantics_1__0 []
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "Lambdas have access to vars from the enclosing scope" do
            r <- exec_may_throw erlps__test_match_semantics_2__0 []
            make_err `shouldEqual` r
        it "Guards in lambdas have access to vars from the enclosing scope" do
            r <- exec_may_throw erlps__test_match_semantics_3__0 []
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "Does not leak scope 1" do
            r <- exec_may_throw erlps__test_scope_does_not_leak_1__0 []
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "Does not leak scope 2" do
            r <- exec_may_throw erlps__test_scope_does_not_leak_2__0 []
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "Can pass lambda do lists stdlib ;)" do
            r <- exec_may_throw erlps__test_can_use_stdlib__0 []
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "Can calculate not trivial factorial" do
            r <- exec_may_throw erlps__test_factorial_abuse_1__0 []
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "Y combinator factorial xD" do
            r <- exec_may_throw erlps__test_factorial_abuse_2__0 []
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "The factorial of death" do
            r <- exec_may_throw erlps__test_factorial_abuse_3__0 []
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "Compare two factorial implementations using list comprehensions" do
            r <- exec_may_throw erlps__test_factorial_comp__0 []
            make_ok (ErlangAtom "ok") `shouldEqual` r

    let atomTup ats = ErlangTuple (map ErlangAtom ats)
    describe "Records" do
      it "Build empty" do
        r <- exec_may_throw erlps__test_build_1__0 []
        atomTup ["empty"] `shouldEqualOk` r
      it "Build product" do
        r <- exec_may_throw erlps__test_build_2__0 []
        atomTup ["product", "l", "r"] `shouldEqualOk` r
      it "Build typed with defaults" do
        r <- exec_may_throw erlps__test_build_3__0 []
        atomTup ["typed", "undefined", "undefined"] `shouldEqualOk` r
      it "Build partially defaulted" do
        r <- exec_may_throw erlps__test_build_4__0 []
        atomTup ["defaulted", "n", "yyy", "undefined", ";)"] `shouldEqualOk` r

      it "Query product left" do
        r <- exec_may_throw erlps__test_query_1__0 []
        ErlangAtom "l" `shouldEqualOk` r
      it "Query product right" do
        r <- exec_may_throw erlps__test_query_2__0 []
        ErlangAtom "r" `shouldEqualOk` r
      it "Query typed with default" do
        r <- exec_may_throw erlps__test_query_3__0 []
        ErlangAtom "undefined" `shouldEqualOk` r
      it "Query defaulted updated undefined" do
        r <- exec_may_throw erlps__test_query_4__0 []
        ErlangAtom "n" `shouldEqualOk` r
      it "Query defaulted updated defaulted" do
        r <- exec_may_throw erlps__test_query_5__0 []
        ErlangAtom "yyy" `shouldEqualOk` r
      it "Query defaulted undefined" do
        r <- exec_may_throw erlps__test_query_6__0 []
        ErlangAtom "undefined" `shouldEqualOk` r
      it "Query defaulted defaulted" do
        r <- exec_may_throw erlps__test_query_7__0 []
        ErlangAtom ";)" `shouldEqualOk` r

      it "Update single field" do
        r <- exec_may_throw erlps__test_update_1__0 []
        atomTup ["product", "l", "updated"] `shouldEqualOk` r
      it "Update all fields" do
        r <- exec_may_throw erlps__test_update_2__0 []
        atomTup ["product", "updated_as_well", "reupdated"] `shouldEqualOk` r
      it "Update defaulted" do
        r <- exec_may_throw erlps__test_update_3__0 []
        atomTup ["defaulted", "n", "yepyep", "undefined", ";)"] `shouldEqualOk` r
      it "Update nothing at all" do
        r <- exec_may_throw erlps__test_update_4__0 []
        atomTup ["defaulted", "n", "yyy", "undefined", ";)"] `shouldEqualOk` r
      it "Update some random stuff" do
        r <- exec_may_throw erlps__test_update_5__0 []
        atomTup ["defaulted", "n", "lol", "nn", ";)"] `shouldEqualOk` r

      it "Match empty" do
        r <- exec_may_throw erlps__test_match_1__0 []
        ErlangAtom "empty" `shouldEqualOk` r
      it "Match single field" do
        r <- exec_may_throw erlps__test_match_2__0 []
        ErlangAtom "l" `shouldEqualOk` r
      it "Match all fields (undefined)" do
        r <- exec_may_throw erlps__test_match_3__0 []
        atomTup ["undefined", "undefined"] `shouldEqualOk` r
      it "Match all some fields with default" do
        r <- exec_may_throw erlps__test_match_4__0 []
        atomTup ["yyy", ";)"] `shouldEqualOk` r
      it "Match defaults in expr" do
        r <- exec_may_throw erlps__test_match_5__0 []
        atomTup ["y", "undefined"] `shouldEqualOk` r

    let dropStack (ErlangTuple [t, p, _]) = pure (ErlangTuple [t, p])
        dropStack _ = pure (ErlangAtom "bad_exception")
    describe "Exception library" do
      it "no exception" do
        r <- liftEffect $ tryCatchFinally
          (\_ -> pure (ErlangAtom "hey"))
          (\_ -> pure (ErlangAtom "bad"))
          (\_ -> pure (ErlangAtom "ok"))
        ErlangAtom "hey" `shouldEqual` r
      it "throw" do
        r <- liftEffect $ tryCatchFinally
          (\_ -> throw (ErlangAtom "boom"))
          dropStack
          (\_ -> pure (ErlangAtom "ok"))
        atomTup ["throw", "boom"] `shouldEqual` r
      it "exit" do
        r <- liftEffect $ tryCatchFinally
          (\_ -> exit (ErlangAtom "boom"))
          dropStack
          (\_ -> pure (ErlangAtom "ok"))
        atomTup ["exit", "boom"] `shouldEqual` r
      it "error" do
        r <- liftEffect $ tryOfCatchFinally
          (\_ -> error (ErlangAtom "boom"))
          (\x -> pure x)
          dropStack
          (\_ -> pure (ErlangAtom "ok"))
        atomTup ["error", "boom"] `shouldEqual` r
      it "finally from catch" do
        ref <- liftEffect $ Ref.new false
        r <- liftEffect $ tryCatchFinally
          (\_ -> error (ErlangAtom "boom"))
          (\err -> pure (ErlangAtom "ok"))
          (\_ -> Ref.write true ref)
        executed <- liftEffect $ Ref.read ref
        executed `shouldEqual` true
        ErlangAtom "ok" `shouldEqual` r
      it "finally from rethrow" do
        ref <- liftEffect $ Ref.new false
        r <- liftEffect $ tryCatch
             (\_ -> tryCatchFinally
                    (\_ -> error (ErlangAtom "boom"))
                    (\err -> error (ErlangAtom "boom"))
                    (\_ -> Ref.write true ref)
             )
             (\_ -> pure (ErlangAtom "ok_e"))
        executed <- liftEffect $ Ref.read ref
        executed `shouldEqual` true
        ErlangAtom "ok_e" `shouldEqual` r
      it "finally from `of`" do
        ref <- liftEffect $ Ref.new false
        r <- liftEffect $ tryCatch
             (\_ -> tryOfCatchFinally
                    (\_ -> pure (ErlangAtom "ok"))
                    (\_ -> error (ErlangAtom "boom"))
                    (\err -> pure (ErlangAtom "bad"))
                    (\_ -> Ref.write true ref))
             (\_ -> pure (ErlangAtom "ok_e"))
        executed <- liftEffect $ Ref.read ref
        executed `shouldEqual` true
        ErlangAtom "ok_e" `shouldEqual` r

    let ok = ErlangAtom "ok"
    describe "Exception transpilation" do
      it "try/catch" do
        r <- exec_may_throw erlps__test_try_catch__0 []
        ok `shouldEqualOk` r
      it "try/catch on typed" do
        r <- exec_may_throw erlps__test_try_catch_type__0 []
        atomTup ["throw", "ok"] `shouldEqualOk` r
      it "try/catch select throw" do
        r <- exec_may_throw erlps__test_try_catch_select_throw__0 []
        ok `shouldEqualOk` r
      it "try/catch select error" do
        r <- exec_may_throw erlps__test_try_catch_select_error__0 []
        ok `shouldEqualOk` r
      it "try of" do
        r <- exec_may_throw erlps__test_try_of__0 []
        ok `shouldEqualOk` r
      it "try of catch" do
        r <- exec_may_throw erlps__test_try_of_catch__0 []
        ok `shouldEqualOk` r
      it "Rethrow" do
        r <- exec_may_throw erlps__test_rethrow__0 []
        ok `shouldEqualOk` r
      it "throw from of" do
        r <- exec_may_throw erlps__test_throw_of__0 []
        ok `shouldEqualOk` r
      it "Unmatched catch" do
        r <- exec_may_throw erlps__test_unmatched_catch__0 []
        ok `shouldEqualOk` r
      it "throw in `after` after return" do
        r <- exec_may_throw erlps__test_after_throw_return__0 []
        ok `shouldEqualOk` r
      it "throw in `after` after catch" do
        r <- exec_may_throw erlps__test_after_throw_catch__0 []
        ok `shouldEqualOk` r
      it "throw in `after` after rethrow" do
        r <- exec_may_throw erlps__test_after_throw_rethrow__0 []
        ok `shouldEqualOk` r
      it "throw in `after` after catch after throw in `of`" do
        r <- exec_may_throw erlps__test_after_throw_of__0 []
        ok `shouldEqualOk` r
      it "Some nasty nesting" do
        r <- exec_may_throw erlps__test_nasty_nest__0 []
        ok `shouldEqualOk` r
      it "Factorial on exceptions" do
        r1 <- exec_may_throw erlps__test_sick_factorial__1 [ErlangNum 1]
        ErlangNum 1 `shouldEqualOk` r1
        r2 <- exec_may_throw erlps__test_sick_factorial__1 [ErlangNum 4]
        ErlangNum 24 `shouldEqualOk` r2
        r3 <- exec_may_throw erlps__test_sick_factorial__1 [ErlangNum 6]
        ErlangNum 720 `shouldEqualOk` r3
      it "Continuational fold left on exceptions" do
        r1 <- exec_may_throw erlps__test_completely_casual_foldl__3
             [ ErlangFun 2 BIF.erlang__op_plus
             , ErlangNum 0
             , arrayToErlangList (map ErlangNum [1,2,3,4])
             ]
        ErlangNum 10 `shouldEqualOk` r1
        r2 <- exec_may_throw erlps__test_completely_casual_foldl__3
             [ ErlangFun 2 BIF.erlang__op_div
             , ErlangNum 210
             , arrayToErlangList (map ErlangNum [2,3,5,7])
             ]
        ErlangNum 1 `shouldEqualOk` r2
