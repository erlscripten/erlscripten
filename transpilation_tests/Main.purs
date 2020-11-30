module Test.Main where

import Prelude

import Effect.Aff.AVar as AVar
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
import Data.String.CodePoints as StrCP
import Data.String as Str
import Unsafe.Coerce

import Data.Time.Duration
import Data.Lazy
import Data.Either
import Data.Tuple as T
import Data.Array as A
import Data.Maybe as M
import Partial.Unsafe
import Erlang.Type
import Erlang.Exception
import Erlang.Builtins as BIF
import Erlang.Invoke
import Node.Buffer as Buf

import Processes
import Lists
import Lambdas
import Records
import Exceptions
import Scoping
import Ordering
import Binaries
import Test.Array
import Array.SUITE
import Base64.SUITE as B64S
import Cancer
import Circular.One as C1
import Circular.Two as C2
import Test.Onload as C3

-- BEWARE - HERE BE DRAGONS - I've lost too many hours debugging alternative helpers
-- If you think you can make a better wrapper which does not crash the testing infrastructure then please make a PR
-- If you can replace this helper with something better then please feel free to do so :)
exec_may_throw_aff :: ErlangFun -> Array ErlangTerm -> Aff ErlangTerm
exec_may_throw_aff fun args =
    let
      t = defer $ (\_ -> run_erlang fun args)
      f = defer $ (\_ -> ErlangAtom "error")
    in do
      v <- liftEffect (catchException (\_ -> pure f
                                      ) (pure t))
      pure $ force v

wololo_term :: Error -> ErlangTerm
wololo_term res = unsafeCoerce res

wololo_codepoint :: Partial => ErlangTerm -> StrCP.CodePoint
wololo_codepoint (ErlangInt res) = unsafeCoerce res

print_err (Right r) = show r
print_err (Left e) =
    case show e of
        "[object Object]" ->
            case (wololo_term e) of
                ErlangTuple [a,b,stack] ->
                    let
                        m1 = show a
                        m2 = show b
                        m3 = unsafePartial $ Str.fromCodePointArray $ map wololo_codepoint $ A.fromFoldable $ M.fromJust $ erlangListToList stack
                    in
                        "[" <> m1 <> ", " <> m2 <> ", " <> m3 <> "]"
                r ->
                    show r
        r ->
            r

exec_may_throw :: ErlangFun -> Array ErlangTerm -> Aff ErlangTerm
exec_may_throw fun args = do
    res <- attempt $ exec_may_throw_aff fun args
    liftEffect $ log $ print_err res -- Uncomment for logs :)
    case res of
        Left _ -> pure make_err
        Right r -> pure $ make_ok r

lift_aff_to_erlang_process :: forall a. (Unit -> Aff a) -> Aff (T.Tuple ErlangTerm a)
lift_aff_to_erlang_process calc = do
        -- ONLY TOUCH THIS IF YOU KNOW WHAT YOU ARE DOING!!!!!
        -- THIS IS A DIRTY HACK TO "lift" an calculation in the Aff monad to an ErlangProcess from the GLOBAL scope
        res_channel <- AVar.empty
        pid_channel <- AVar.empty
        _ <- forkAff do
            packed_pid <- exec_may_throw BIF.erlang__spawn__1 [(
                ErlangFun 0 (\ _ -> let -- TODO: Fixme - the calculation should yield to the scheduler and only then we may launch the avar. We need a jump to FFI here :(
                    a = unsafePerformEffect $ launchAff_ (
                        do
                            res <- calc unit
                            AVar.put res res_channel
                        )
                    in
                       ErlangInt 1))]
            -- At this point we never yielded so the process MUST be alive
            pid <- unpack_ok packed_pid
            AVar.put pid pid_channel

        pid <- AVar.take pid_channel
        packed_is_alive <- exec_may_throw BIF.erlang__is_process_alive__1 [pid]
        (ErlangAtom "true") `shouldEqualOk` packed_is_alive

        res <- AVar.take res_channel

        delay (Milliseconds 1.0) -- force a context switch to cleanup the process :P
        packed_is_alive <- exec_may_throw BIF.erlang__is_process_alive__1 [pid]
        (ErlangAtom "false") `shouldEqualOk` packed_is_alive
        pure $ T.Tuple pid res

make_ok term = ErlangTuple [ErlangAtom "ok", term]
make_err = ErlangAtom "error"

unpack_ok :: ErlangTerm -> Aff ErlangTerm
unpack_ok (ErlangTuple [ErlangAtom "ok", term]) = pure term
unpack_ok _ = do
    1 `shouldEqual` 0
    pure ErlangEmptyList

test_reverse a = do
    let input = arrayToErlangList $ map ErlangInt a
    let output = make_ok $ arrayToErlangList $ map ErlangInt (A.reverse a)
    res <- exec_may_throw erlps__reverse__1 [input]
    output `shouldEqual` res

test_sort a = do
    let input = arrayToErlangList $ map ErlangInt a
    let output = make_ok $ arrayToErlangList $ map ErlangInt (A.sort a)
    res <- exec_may_throw erlps__sort__1 [input]
    output `shouldEqual` res

test_map a ef pf = do
    let input = arrayToErlangList $ map ErlangInt a
    let output = make_ok $ arrayToErlangList $ map ErlangInt (map pf a)
    res <- exec_may_throw erlps__map__2 [ef, input]
    output `shouldEqual` res

test_zip_ok a b = do
    let input_a = arrayToErlangList $ map ErlangInt a
    let input_b = arrayToErlangList $ map ErlangInt b
    let output = make_ok $ arrayToErlangList $ map (\(T.Tuple x y) -> ErlangTuple [x,y]) (A.zip (map ErlangInt a) (map ErlangInt b))
    res <- exec_may_throw erlps__zip__2 [input_a, input_b]
    output `shouldEqual` res

test_zip_fail a b = do
    let input_a = arrayToErlangList $ map ErlangInt a
    let input_b = arrayToErlangList $ map ErlangInt b
    res <- exec_may_throw erlps__zip__2 [input_a, input_b]
    make_err `shouldEqual` res

test_seq from to expected = do
    calc <- exec_may_throw erlps__seq__2 [ErlangInt from, ErlangInt to]
    let out = make_ok $ arrayToErlangList $ map ErlangInt expected
    out `shouldEqual` calc

shouldEqualOk a b = make_ok a `shouldEqual` b

main :: Effect Unit
main =
    launchAff_ $ runSpec [consoleReporter] do

    let whitelist = case unit of
          _ -> M.Nothing  -- comment for whitelist :)
          _ -> M.Just ["Binaries"]
    let describe_ s = case whitelist of
          M.Nothing -> describe s
          M.Just l ->
            if A.elemIndex s l == M.Nothing then \_ -> pure unit else describe s
    describe_ "Sanity check" do
        it "one should equal one" do
            1 `shouldEqual` 1
        it "two should equal two" do
            2 `shouldEqual` 2

    describe_ "STDLIB Lists" do
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
            test_map [1,2,3,4,5]
              (ErlangFun 1 (unsafePartial $ \ [ErlangInt a] -> ErlangInt (a*20))) (\x -> x*20)
            test_map [1,2,3,4,5]
              (ErlangFun 1 (unsafePartial $ \ [ErlangInt a] -> ErlangInt (-a))) (\x -> -x)
        it "zip/2" do
            test_zip_ok [1,2,3,4] [4,3,2,1]
            test_zip_ok [1,2,7,4] [1,3,2,1]
            test_zip_fail [1] [1,2]
            test_zip_fail [1,2] [1]
        it "seq/2" do
            test_seq 0 0 [0]
            test_seq 1 0 []
            test_seq 1 10 [1,2,3,4,5,6,7,8,9,10]

    describe_ "STDLIB Array" do
        it "can create zero size array" do
            r <- exec_may_throw erlps__test_create_0__0 []
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "Create a fixed-size array with entries 0-9 set to 'undefined'" do
            r <- exec_may_throw erlps__test_create_1__0 []
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "is functional :)" do
            r <- exec_may_throw erlps__test_functionality__0 []
            make_ok (ErlangAtom "ok") `shouldEqual` r

    describe_ "Real Array tests taken from OTP - array_SUITE.erl" do
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

    describe_ "Lambdas" do
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
        it "apply/3 apply/2 make_fun/3 work" do
            r <- exec_may_throw erlps__test_apply_and_make_fun__0 []
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "apply/3 apply/2 make_fun/3 throw appropriate exceptions" do
            r <- exec_may_throw erlps__test_apply_exceptions__0 []
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "Local recursion (simple)" do
            r <- exec_may_throw erlps__test_local_rec_1__0 []
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "Local recursion (self shadow in arg)" do
            r <- exec_may_throw erlps__test_local_rec_2__0 []
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "Local recursion (self shadow call)" do
            r <- exec_may_throw erlps__test_local_rec_3__0 []
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "Local recursion (simple tail rec)" do
            r <- exec_may_throw erlps__test_local_tailrec_1__0 []
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "Local recursion (tail rec big)" do
            r <- exec_may_throw erlps__test_local_tailrec_2__0 []
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "Local recursion (nested shadow scopetest 1)" do
            r <- exec_may_throw erlps__test_local_rec_scoping_1__0 []
            make_ok (ErlangAtom "ok") `shouldEqual` r
        it "Local recursion (nested shadow scopetest 2)" do
            r <- exec_may_throw erlps__test_local_rec_scoping_2__0 []
            make_ok (ErlangAtom "ok") `shouldEqual` r

    let atomTup ats = ErlangTuple (map ErlangAtom ats)
    describe_ "Records" do
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

      it "Test index 1" do
        r <- exec_may_throw erlps__test_index_1__0 []
        ErlangInt 3 `shouldEqualOk` r
      it "Test index 2" do
        r <- exec_may_throw erlps__test_index_2__0 []
        ErlangInt 2 `shouldEqualOk` r
      it "Test index 3" do
        r <- exec_may_throw erlps__test_index_3__0 []
        ErlangInt 3 `shouldEqualOk` r

    let dropStack (ErlangTuple [t, p, _]) = ErlangTuple [t, p]
        dropStack _ = ErlangAtom "bad_exception"
    describe_ "Exception library" do
      it "no exception" do
        let r = tryCatchFinally
                (\_ -> ErlangAtom "hey")
                (\_ -> ErlangAtom "bad")
                (\_ -> ErlangAtom "ok")
        ErlangAtom "hey" `shouldEqual` r
      it "throw" do
        let r = tryCatchFinally
                (\_ -> throw (ErlangAtom "boom"))
                dropStack
                (\_ -> ErlangAtom "ok")
        atomTup ["throw", "boom"] `shouldEqual` r
      it "exit" do
        let r = tryCatchFinally
                (\_ -> exit (ErlangAtom "boom"))
                dropStack
                (\_ -> ErlangAtom "ok")
        atomTup ["exit", "boom"] `shouldEqual` r
      it "error" do
        let r = tryOfCatchFinally
                (\_ -> error (ErlangAtom "boom"))
                (\x -> x)
                dropStack
                (\_ -> ErlangAtom "ok")
        atomTup ["error", "boom"] `shouldEqual` r
      it "finally from catch" do
        ref <- liftEffect $ Ref.new false
        let r = tryCatchFinally
                (\_ -> error (ErlangAtom "boom"))
                (\err -> ErlangAtom "ok")
                (\_ -> unsafePerformEffect $ Ref.write true ref)
        executed <- liftEffect $ Ref.read ref
        executed `shouldEqual` true
        ErlangAtom "ok" `shouldEqual` r
      it "finally from rethrow" do
        ref <- liftEffect $ Ref.new false
        let r = tryCatch
             (\_ -> tryCatchFinally
                    (\_ -> error (ErlangAtom "boom"))
                    (\err -> error (ErlangAtom "boom"))
                    (\_ -> unsafePerformEffect $ Ref.write true ref)
             )
             (\_ -> ErlangAtom "ok_e")
        executed <- liftEffect $ Ref.read ref
        executed `shouldEqual` true
        ErlangAtom "ok_e" `shouldEqual` r
      it "finally from `of`" do
        ref <- liftEffect $ Ref.new false
        let r = tryCatch
             (\_ -> tryOfCatchFinally
                    (\_ -> ErlangAtom "ok")
                    (\_ -> error (ErlangAtom "boom"))
                    (\err -> ErlangAtom "bad")
                    (\_ -> unsafePerformEffect $ Ref.write true ref))
             (\_ -> ErlangAtom "ok_e")
        executed <- liftEffect $ Ref.read ref
        executed `shouldEqual` true
        ErlangAtom "ok_e" `shouldEqual` r

    let ok = ErlangAtom "ok"
    describe_ "Exception transpilation" do
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
        r1 <- exec_may_throw erlps__test_sick_factorial__1 [ErlangInt 1]
        ErlangInt 1 `shouldEqualOk` r1
        r2 <- exec_may_throw erlps__test_sick_factorial__1 [ErlangInt 4]
        ErlangInt 24 `shouldEqualOk` r2
        r3 <- exec_may_throw erlps__test_sick_factorial__1 [ErlangInt 6]
        ErlangInt 720 `shouldEqualOk` r3
      it "Continuational fold left on exceptions" do
        r1 <- exec_may_throw erlps__test_completely_casual_foldl__3
             [ ErlangFun 2 BIF.erlang__op_plus
             , ErlangInt 0
             , arrayToErlangList (map ErlangInt [1,2,3,4])
             ]
        ErlangInt 10 `shouldEqualOk` r1
        r2 <- exec_may_throw erlps__test_completely_casual_foldl__3
             [ ErlangFun 2 BIF.erlang__op_div
             , ErlangInt 210
             , arrayToErlangList (map ErlangInt [2,3,5,7])
             ]
        ErlangInt 1 `shouldEqualOk` r2
      it "Deprecated catch / throw" do
        r <- exec_may_throw erlps__test_deprecated_catch_throw__0 []
        ok `shouldEqualOk` r
      it "Deprecated catch / error" do
        r <- exec_may_throw erlps__test_deprecated_catch_error__0 []
        atomTup ["EXIT", "ok"] `shouldEqualOk` r
      it "Deprecated catch / exit" do
        r <- exec_may_throw erlps__test_deprecated_catch_exit__0 []
        atomTup ["EXIT", "ok"] `shouldEqualOk` r
      it "Properly orders operations 1" do
        r <- exec_may_throw erlps__test_ordering_1__0 []
        ok `shouldEqualOk` r
      it "Properly orders operations 2" do
        r <- exec_may_throw erlps__test_ordering_2__0 []
        ok `shouldEqualOk` r
      it "Properly orders operations 3" do
        r <- exec_may_throw erlps__test_ordering_3__0 []
        ok `shouldEqualOk` r
      it "Doesn't fuck up List Comprehensions" do
        r <- exec_may_throw erlps__test_lc__0 []
        ok `shouldEqualOk` r
      it "test_scope_1" do
        r <- exec_may_throw erlps__test_scope_1__0 []
        ok `shouldEqualOk` r
      it "test_scope_2" do
        r <- exec_may_throw erlps__test_scope_2__0 []
        ok `shouldEqualOk` r

    describe_ "Cancer..." do
      it "IO works :O" do
        r <- exec_may_throw erlps__test_wtf__0 []
        ok `shouldEqualOk` r

    describe "Real b64 tests from OTP" do
      it "base64_encode" do
            r <- exec_may_throw B64S.erlps__base64_encode__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r
      it "base64_decode" do
            r <- exec_may_throw B64S.erlps__base64_decode__1 [ErlangEmptyList]
            make_ok (ErlangAtom "ok") `shouldEqual` r

    describe_ "Processes OwO" do
      it "can spawn" do
        r <- exec_may_throw erlps__test_spawn__0 []
        ok `shouldEqualOk` r
      it "can move aff monad to process" do
          (T.Tuple _ r) <- lift_aff_to_erlang_process (\_ -> pure "OK")
          "OK" `shouldEqual` r
      it "can request it's own pid" do
          (T.Tuple pid1 packed_pid) <- lift_aff_to_erlang_process (\_ -> exec_may_throw erlps__test_get_self__0 [])
          pid1 `shouldEqualOk` packed_pid
      it "receive primop - simple case" do
          (T.Tuple _ r) <- lift_aff_to_erlang_process (\_ -> exec_may_throw erlps__test_simple_receive_primop__0 [])
          ok `shouldEqualOk` r

    describe_ "Binaries" do
      let bin b = ErlangBinary (unsafePerformEffect $ Buf.fromArray b)
      it "Build empty" do
        r <- exec_may_throw erlps__test_build_empty__0 []
        bin [] `shouldEqualOk` r
      it "Build single" do
        r <- exec_may_throw erlps__test_build_single__0 []
        bin [1] `shouldEqualOk` r
      it "Build multi" do
        r <- exec_may_throw erlps__test_build_multi__0 []
        bin [1,2,3] `shouldEqualOk` r
      it "Build string" do
        r <- exec_may_throw erlps__test_build_string__0 []
        bin [88,68] `shouldEqualOk` r
      it "Build subbinary" do
        r <- exec_may_throw erlps__test_build_bin__0 []
        bin [1,2,3] `shouldEqualOk` r
      it "Build float" do
        r <- exec_may_throw erlps__test_build_float__0 []
        bin [63,240,0,0,0,0,0,0] `shouldEqualOk` r
      it "Build mixed" do
        r <- exec_may_throw erlps__test_build_mixed__0 []
        bin [88,68,1,2,3,4,64,20,0,0,0,0,0,0] `shouldEqualOk` r
      it "Build single" do
        r <- exec_may_throw erlps__test_build_single__0 []
        bin [1] `shouldEqualOk` r

      it "Build int reg. size" do
        r <- exec_may_throw erlps__test_build_int_size__0 []
        bin [0,0,0,1] `shouldEqualOk` r
      it "Build int reg. size&unit" do
        r <- exec_may_throw erlps__test_build_int_size_unit__0 []
        bin [0,0,0,1] `shouldEqualOk` r
      it "Build int reg. little end" do
        r <- exec_may_throw erlps__test_build_int_endian_little__0 []
        bin [1,0,0,0] `shouldEqualOk` r
      it "Build int reg. underflow" do
        r <- exec_may_throw erlps__test_build_int_underflow__0 []
        bin [255] `shouldEqualOk` r
      it "Build int reg. overflow" do
        r <- exec_may_throw erlps__test_build_int_overflow__0 []
        bin [0] `shouldEqualOk` r

      it "Build string reg. size&unit" do
        r <- exec_may_throw erlps__test_build_string_size_unit__0 []
        bin [0,88,0,68] `shouldEqualOk` r

      it "Build float reg. size" do
        r <- exec_may_throw erlps__test_build_float_32__0 []
        bin [63,128,0,0] `shouldEqualOk` r
      it "Build float reg. size&unit" do
        r <- exec_may_throw erlps__test_build_float_32_unit__0 []
        bin [63,128,0,0] `shouldEqualOk` r
      it "Build float reg. little end" do
        r <- exec_may_throw erlps__test_build_float_little__0 []
        bin [0,0,0,0,0,0,240,63] `shouldEqualOk` r

      it "Build comprehension" do
        r <- exec_may_throw erlps__test_build_comprehension__0 []
        bin [1,0,4,2,0,4,3,0,4] `shouldEqualOk` r

      it "Match empty" do
        r <- exec_may_throw erlps__test_match_empty__0 []
        ok `shouldEqualOk` r
      it "Match single" do
        r <- exec_may_throw erlps__test_match_single__0 []
        ok `shouldEqualOk` r
      it "Match single var" do
        r <- exec_may_throw erlps__test_match_single_var__0 []
        ok `shouldEqualOk` r
      it "Match many" do
        r <- exec_may_throw erlps__test_match_many__0 []
        ok `shouldEqualOk` r
      it "Match int size" do
        r <- exec_may_throw erlps__test_match_int_size__0 []
        ok `shouldEqualOk` r
      it "Match int size&unit" do
        r <- exec_may_throw erlps__test_match_int_size_unit__0 []
        ok `shouldEqualOk` r
      it "Match int little" do
        r <- exec_may_throw erlps__test_match_int_little__0 []
        ok `shouldEqualOk` r
      it "Match int signed" do
        r <- exec_may_throw erlps__test_match_int_signed__0 []
        ok `shouldEqualOk` r
      it "Match int mix" do
        r <- exec_may_throw erlps__test_match_int_mix__0 []
        ok `shouldEqualOk` r
      it "Match string" do
        r <- exec_may_throw erlps__test_match_string__0 []
        ok `shouldEqualOk` r
      it "Match bin" do
        r <- exec_may_throw erlps__test_match_bin__0 []
        ok `shouldEqualOk` r
      it "Match bin at the end" do
        r <- exec_may_throw erlps__test_match_bin_end__0 []
        ok `shouldEqualOk` r
      it "Match float64" do
        r <- exec_may_throw erlps__test_match_float_64__0 []
        ok `shouldEqualOk` r
      it "Match float32" do
        r <- exec_may_throw erlps__test_match_float_32__0 []
        ok `shouldEqualOk` r

    describe_ "Code server" do
      it "Circular dependencies - One -> One local call" do
        r <- exec_may_throw C1.erlps__one__0 []
        (ErlangAtom "hello_from_module_one") `shouldEqualOk` r
      it "Circular dependencies - One -> Two remote call" do
        r <- exec_may_throw C1.erlps__two__0 []
        (ErlangAtom "hello_from_module_two") `shouldEqualOk` r
      it "Circular dependencies - Two -> One local call" do
        r <- exec_may_throw C2.erlps__one__0 []
        (ErlangAtom "hello_from_module_one") `shouldEqualOk` r
      it "Circular dependencies - Two -> Two remote call" do
        r <- exec_may_throw C2.erlps__two__0 []
        (ErlangAtom "hello_from_module_two") `shouldEqualOk` r
      it "Onload" do
        r <- exec_may_throw C3.erlps__test_onload__0 []
        (ErlangAtom "hehe") `shouldEqualOk` r
