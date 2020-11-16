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
import Erlang.Helpers (unsafePerformEffectGuard)
import Lists
import Lambdas
import Records

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
      it "Build typed" do
        r <- exec_may_throw erlps__test_build_3__0 []
        atomTup ["typed", "undefined", "undefined"] `shouldEqualOk` r
      it "Build defaulted" do
        r <- exec_may_throw erlps__test_build_4__0 []
        atomTup ["defaulted", "n", "yyy", "nn", ";)"] `shouldEqualOk` r

      it "Query product `left`" do
        r <- exec_may_throw erlps__test_query_1__0 []
        ErlangAtom "l" `shouldEqualOk` r
      it "Query product `right`" do
        r <- exec_may_throw erlps__test_query_2__0 []
        ErlangAtom "r" `shouldEqualOk` r
      it "Query typed" do
        r <- exec_may_throw erlps__test_query_3__0 []
        ErlangAtom "undefined" `shouldEqualOk` r
      it "Query defaulted `no`" do
        r <- exec_may_throw erlps__test_query_4__0 []
        ErlangAtom "n" `shouldEqualOk` r
      it "Query defaulted `yep`" do
        r <- exec_may_throw erlps__test_query_5__0 []
        ErlangAtom "yyy" `shouldEqualOk` r
      it "Query defaulted `noooo`" do
        r <- exec_may_throw erlps__test_query_6__0 []
        ErlangAtom "nn" `shouldEqualOk` r
      it "Query defaulted `yeeeep`" do
        r <- exec_may_throw erlps__test_query_7__0 []
        ErlangAtom ";)" `shouldEqualOk` r

      it "Update 1" do
        r <- exec_may_throw erlps__test_update_1__0 []
        atomTup ["product", "l", "updated"] `shouldEqualOk` r
      it "Update 2" do
        r <- exec_may_throw erlps__test_update_2__0 []
        atomTup ["product", "updated_as_well", "reupdated"] `shouldEqualOk` r
      it "Update 3" do
        r <- exec_may_throw erlps__test_update_3__0 []
        atomTup ["defaulted", "n", "yepyep", "nn", ";)"] `shouldEqualOk` r
      it "Update 4" do
        r <- exec_may_throw erlps__test_update_4__0 []
        atomTup ["defaulted", "n", "yyy", "nn", ";)"] `shouldEqualOk` r
      it "Update 5" do
        r <- exec_may_throw erlps__test_update_5__0 []
        atomTup ["defaulted", "n", "lol", "nn", ";)"] `shouldEqualOk` r

      it "Match 1" do
        r <- exec_may_throw erlps__test_match_1__0 []
        ErlangAtom "empty" `shouldEqualOk` r
      it "Match 2" do
        r <- exec_may_throw erlps__test_match_2__0 []
        ErlangAtom "l" `shouldEqualOk` r
      it "Match 3" do
        r <- exec_may_throw erlps__test_match_3__0 []
        atomTup ["undefined", "undefined"] `shouldEqualOk` r
      it "Match 4" do
        r <- exec_may_throw erlps__test_match_4__0 []
        atomTup ["yyy", ";)"] `shouldEqualOk` r
      it "Match 5" do
        r <- exec_may_throw erlps__test_match_5__0 []
        atomTup ["y", "undefined"] `shouldEqualOk` r

    describe "Exception library" do
      it "throw" do
        r <- liftEffect $ tryCatchFinally
          (\_ -> throw (ErlangAtom "boom"))
          (\err -> pure (ErlangTuple [err.exceptionType, err.exceptionPayload]))
          (\_ -> pure (ErlangAtom "ok"))
        atomTup ["throw", "boom"] `shouldEqual` r
      it "exit" do
        r <- liftEffect $ tryCatchFinally
          (\_ -> exit (ErlangAtom "boom"))
          (\err -> pure (ErlangTuple [err.exceptionType, err.exceptionPayload]))
          (\_ -> pure (ErlangAtom "ok"))
        atomTup ["exit", "boom"] `shouldEqual` r
      it "error" do
        r <- liftEffect $ tryOfCatchFinally
          (\_ -> error (ErlangAtom "boom"))
          (\x -> pure x)
          (\err -> pure (ErlangTuple [err.exceptionType, err.exceptionPayload]))
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
