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
import Data.BigInt as DBI
import Partial.Unsafe
import Erlang.Type
import Erlang.Exception
import Erlang.Builtins as BIF
import Erlang.Invoke
import Erlang.TestUtil
import Node.Buffer as Buf

import Comparator
import Processes
import Lists
import Lambdas
import Records
import Exceptions
import Scoping
import Ordering
import Binaries
import Playground as Play
import Test.Array
import Array.SUITE
--import Base64.SUITE as B64S
import Cancer
import Basics
import Circular.One as C1
import Circular.Two as C2
import Test.Onload as C3
import Autoimport as Auto
import String.Escape

test_reverse :: forall e. ToErlang e => Array e -> Aff Unit
test_reverse a = do
    let input = toErl a
    let output = makeOk $ toErl (A.reverse a)
    res <- exec erlps__reverse__1 [input]
    output `shouldEqual` res

test_sort :: forall e. Ord e => ToErlang e => Array e -> Aff Unit
test_sort a = do
    let input = toErl a
    let output = makeOk $ toErl (A.sort a)
    res <- exec erlps__sort__1 [input]
    output `shouldEqual` res

test_map :: forall e. ToErlang e => Array e -> ErlangTerm -> (e -> e) -> Aff Unit
test_map a ef pf = do
    let input = toErl a
    let output = makeOk $ toErl (map pf a)
    res <- exec erlps__map__2 [ef, input]
    output `shouldEqual` res

test_zip_ok :: forall e. ToErlang e => Array e -> Array e -> Aff Unit
test_zip_ok a b = do
    let input_a = toErl a
    let input_b = toErl b
    let output = makeOk $ toErl $ map (\(T.Tuple x y) -> ErlangTuple [x,y]) (A.zip (map toErl a) (map toErl b))
    res <- exec erlps__zip__2 [input_a, input_b]
    output `shouldEqual` res

test_zip_fail :: forall e. ToErlang e => Array e -> Array e -> Aff Unit
test_zip_fail a b = do
    let input_a = toErl a
    let input_b = toErl b
    res <- exec erlps__zip__2 [input_a, input_b]
    err `shouldEqual` res

test_seq :: forall e. ToErlang e => e -> e -> Array e -> Aff Unit
test_seq from to expected = do
    calc <- exec erlps__seq__2 [toErl from, toErl to]
    let out = makeOk $ toErl expected
    out `shouldEqual` calc

shouldEqualOk a b = makeOk a `shouldEqual` b

main :: Effect Unit
main =
    launchAff_ $ runSpec [consoleReporter] do

    let ok = ErlangAtom "ok"
    let whitelist = case unit of
          _ -> M.Nothing  -- comment for whitelist :)
          _ -> M.Just ["PLAYGROUND"]
    let describe_ s = case whitelist of
          M.Nothing -> describe s
          M.Just l ->
            if A.elemIndex s l == M.Nothing then \_ -> pure unit else describe s
    describe_ "Sanity check" do
        it "one should equal one" do
            1 `shouldEqual` 1
        it "two should equal two" do
            2 `shouldEqual` 2

    describe_ "PLAYGROUND" do
      it "TEST 1" do
          testExecOk ok Play.erlps__test__0 []

    describe_ "Basics" do
        it "2 + 2" do
            testExecOk (toErl 4) erlps__test_two_plus_two__0 []
        it "Factorial" do
            testExecOk (toErl 720) erlps__test_factorial__1 [toErl 6]
        it "Match float64" do
            testExecOk ok erlps__test_too_big_int__0 []
        it "Comparator 1" do
            testExecOk ok erlps__test_comp1__0 []

    describe_ "STDLIB Lists" do
        it "reverse/1" do
            test_reverse [1,2,3,4,5,6,7,8,9,10]
            test_reverse [1]
            test_reverse ([] :: Array Int)
            test_reverse [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
        it "sort/1" do
            test_sort [1,2,3,4,5,6,7,8,9,10]
            test_sort [10,9,8,7,6,5,4,3,2,1]
            test_sort [5,3,34,6,2,5,7565,4,3,7,8,5,3]
        it "map/1" do
            test_map [1,2,3,4,5]
              (ErlangFun 1 (unsafePartial $ \ [ErlangInt a] -> ErlangInt (a * DBI.fromInt 20)))
              (\x -> x * 20)
            test_map [1,2,3,4,5]
              (ErlangFun 1 (unsafePartial $ \ [ErlangInt a] -> ErlangInt (-a))) (\x -> -x)
        it "zip/2" do
            test_zip_ok [1,2,3,4] [4,3,2,1]
            test_zip_ok [1,2,7,4] [1,3,2,1]
            test_zip_fail [1] [1,2]
            test_zip_fail [1,2] [1]
        it "seq/2" do
            test_seq 0 0 [0]
            test_seq 1 0 ([] :: Array Int)
            test_seq 1 10 [1,2,3,4,5,6,7,8,9,10]

    describe_ "STDLIB Array" do
        it "can create zero size array" do
            testExecOk ok erlps__test_create_0__0 []
        it "Create a fixed-size array with entries 0-9 set to 'undefined'" do
            testExecOk ok erlps__test_create_1__0 []
        it "is functional :)" do
            testExecOk ok erlps__test_functionality__0 []

    describe_ "Real Array tests taken from OTP - array_SUITE.erl" do
        it "new_test" do
            testExecOk ok erlps__new_test__1 [ErlangEmptyList]
        it "fix_test" do
            testExecOk ok erlps__fix_test__1 [ErlangEmptyList]
        it "relax_test" do
            testExecOk ok erlps__relax_test__1 [ErlangEmptyList]
        it "resize_test" do
            testExecOk ok erlps__resize_test__1 [ErlangEmptyList]
        it "set_get_test" do
            testExecOk ok erlps__set_get_test__1 [ErlangEmptyList]
        it "to_list_test" do
            testExecOk ok erlps__to_list_test__1 [ErlangEmptyList]
        it "sparse_to_list_test" do
            testExecOk ok erlps__sparse_to_list_test__1 [ErlangEmptyList]
        it "from_list_test" do
            testExecOk ok erlps__from_list_test__1 [ErlangEmptyList]
        it "to_orddict_test" do
            testExecOk ok erlps__to_orddict_test__1 [ErlangEmptyList]
        it "sparse_to_orddict_test" do
            testExecOk ok erlps__sparse_to_orddict_test__1 [ErlangEmptyList]
        it "from_orddict_test" do
            testExecOk ok erlps__from_orddict_test__1 [ErlangEmptyList]
        it "map_test" do
            testExecOk ok erlps__map_test__1 [ErlangEmptyList]
        it "sparse_map_test" do
            testExecOk ok erlps__sparse_map_test__1 [ErlangEmptyList]
        it "foldl_test" do
            testExecOk ok erlps__foldl_test__1 [ErlangEmptyList]
        it "sparse_foldl_test" do
            testExecOk ok erlps__sparse_foldl_test__1 [ErlangEmptyList]
        it "foldr_test" do
            testExecOk ok erlps__foldr_test__1 [ErlangEmptyList]
        it "sparse_foldr_test" do
            testExecOk ok erlps__sparse_foldr_test__1 [ErlangEmptyList]

    describe_ "Lambdas" do
        it "can be called" do
            testExecOk ok erlps__test_can_be_called__0 []
        it "fun can be treated as lambda" do
            testExecOk ok erlps__test_can_pass_fun__0 []
        it "Lambda clauses overwrite vars in scope" do
            testExecOk ok erlps__test_match_semantics_1__0 []
        it "Lambdas have access to vars from the enclosing scope" do
            testExecErr erlps__test_match_semantics_2__0 []
        it "Guards in lambdas have access to vars from the enclosing scope" do
            testExecOk ok erlps__test_match_semantics_3__0 []
        it "Does not leak scope 1" do
            testExecOk ok erlps__test_scope_does_not_leak_1__0 []
        it "Does not leak scope 2" do
            testExecOk ok erlps__test_scope_does_not_leak_2__0 []
        it "Can pass lambda do lists stdlib ;)" do
            testExecOk ok erlps__test_can_use_stdlib__0 []
        it "Can calculate not trivial factorial" do
            testExecOk ok erlps__test_factorial_abuse_1__0 []
        it "Y combinator factorial xD" do
            testExecOk ok erlps__test_factorial_abuse_2__0 []
        it "The factorial of death" do
            testExecOk ok erlps__test_factorial_abuse_3__0 []
        it "Compare two factorial implementations using list comprehensions" do
            testExecOk ok erlps__test_factorial_comp__0 []
        it "apply/3 apply/2 make_fun/3 work" do
            testExecOk ok erlps__test_apply_and_make_fun__0 []
        it "apply/3 apply/2 make_fun/3 throw appropriate exceptions" do
            testExecOk ok erlps__test_apply_exceptions__0 []
        it "Local recursion (simple)" do
            testExecOk ok erlps__test_local_rec_1__0 []
        it "Local recursion (self shadow in arg)" do
            testExecOk ok erlps__test_local_rec_2__0 []
        it "Local recursion (self shadow call)" do
            testExecOk ok erlps__test_local_rec_3__0 []
        it "Local recursion (simple tail rec)" do
            testExecOk ok erlps__test_local_tailrec_1__0 []
        it "Local recursion (tail rec big)" do
            testExecOk ok erlps__test_local_tailrec_2__0 []
        it "Local recursion (nested shadow scopetest 1)" do
            testExecOk ok erlps__test_local_rec_scoping_1__0 []
        it "Local recursion (nested shadow scopetest 2)" do
            testExecOk ok erlps__test_local_rec_scoping_2__0 []

    let atomTup ats = ErlangTuple (map ErlangAtom ats)
    describe_ "Records" do
      it "Build empty" do
        testExecOk (atomTup ["empty"]) erlps__test_build_1__0 []
      it "Build product" do
        testExecOk (atomTup ["product", "l", "r"]) erlps__test_build_2__0 []
      it "Build typed with defaults" do
        testExecOk (atomTup ["typed", "undefined", "undefined"]) erlps__test_build_3__0 []
      it "Build partially defaulted" do
        testExecOk (atomTup ["defaulted", "n", "yyy", "undefined", ";)"]) erlps__test_build_4__0 []

      it "Query product left" do
        testExecOk (ErlangAtom "l") erlps__test_query_1__0 []
      it "Query product right" do
        testExecOk (ErlangAtom "r") erlps__test_query_2__0 []
      it "Query typed with default" do
        testExecOk (ErlangAtom "undefined") erlps__test_query_3__0 []
      it "Query defaulted updated undefined" do
        testExecOk (ErlangAtom "n") erlps__test_query_4__0 []
      it "Query defaulted updated defaulted" do
        testExecOk (ErlangAtom "yyy") erlps__test_query_5__0 []
      it "Query defaulted undefined" do
        testExecOk (ErlangAtom "undefined") erlps__test_query_6__0 []
      it "Query defaulted defaulted" do
        testExecOk (ErlangAtom ";)") erlps__test_query_7__0 []

      it "Update single field" do
        testExecOk (atomTup ["product", "l", "updated"]) erlps__test_update_1__0 []
      it "Update all fields" do
        testExecOk (atomTup ["product", "updated_as_well", "reupdated"]) erlps__test_update_2__0 []
      it "Update defaulted" do
        testExecOk (atomTup ["defaulted", "n", "yepyep", "undefined", ";)"]) erlps__test_update_3__0 []
      it "Update nothing at all" do
        testExecOk (atomTup ["defaulted", "n", "yyy", "undefined", ";)"]) erlps__test_update_4__0 []
      it "Update some random stuff" do
        testExecOk (atomTup ["defaulted", "n", "lol", "nn", ";)"]) erlps__test_update_5__0 []

      it "Match empty" do
        testExecOk (ErlangAtom "empty") erlps__test_match_1__0 []
      it "Match single field" do
        testExecOk (ErlangAtom "l") erlps__test_match_2__0 []
      it "Match all fields (undefined)" do
        testExecOk (atomTup ["undefined", "undefined"]) erlps__test_match_3__0 []
      it "Match all some fields with default" do
        testExecOk (atomTup ["yyy", ";)"]) erlps__test_match_4__0 []
      it "Match defaults in expr" do
        testExecOk (atomTup ["y", "undefined"]) erlps__test_match_5__0 []

      it "Test index 1" do
        testExecOk (toErl 3) erlps__test_index_1__0 []
      it "Test index 2" do
        testExecOk (toErl 2) erlps__test_index_2__0 []
      it "Test index 3" do
        testExecOk (toErl 3) erlps__test_index_3__0 []

      it "is_record tests" do
        testExecOk ok erlps__test_is_record__0 []
      it "record_info" do
        testExecOk ok erlps__test_record_info__0 []

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

    describe_ "Exception transpilation" do
      it "try/catch" do
        testExecOk ok erlps__test_try_catch__0 []
      it "try/catch on typed" do
        testExecOk (atomTup ["throw", "ok"]) erlps__test_try_catch_type__0 []
      it "try/catch select throw" do
        testExecOk ok erlps__test_try_catch_select_throw__0 []
      it "try/catch select error" do
        testExecOk ok erlps__test_try_catch_select_error__0 []
      it "try of" do
        testExecOk ok erlps__test_try_of__0 []
      it "try of catch" do
        testExecOk ok erlps__test_try_of_catch__0 []
      it "Rethrow" do
        testExecOk ok erlps__test_rethrow__0 []
      it "throw from of" do
        testExecOk ok erlps__test_throw_of__0 []
      it "Unmatched catch" do
        testExecOk ok erlps__test_unmatched_catch__0 []
      it "throw in `after` after return" do
        testExecOk ok erlps__test_after_throw_return__0 []
      it "throw in `after` after catch" do
        testExecOk ok erlps__test_after_throw_catch__0 []
      it "throw in `after` after rethrow" do
        testExecOk ok erlps__test_after_throw_rethrow__0 []
      it "throw in `after` after catch after throw in `of`" do
        testExecOk ok erlps__test_after_throw_of__0 []
      it "Some nasty nesting" do
        testExecOk ok erlps__test_nasty_nest__0 []
      it "Factorial on exceptions" do
        r1 <- exec erlps__test_sick_factorial__1 [toErl 1]
        toErl 1 `shouldEqualOk` r1
        r2 <- exec erlps__test_sick_factorial__1 [toErl 4]
        toErl 24 `shouldEqualOk` r2
        r3 <- exec erlps__test_sick_factorial__1 [toErl 6]
        toErl 720 `shouldEqualOk` r3
      it "Continuational fold left on exceptions" do
        r1 <- exec erlps__test_completely_casual_foldl__3
             [ ErlangFun 2 BIF.erlang__op_plus
             , toErl 0
             , toErl (map toErl [1,2,3,4])
             ]
        toErl 10 `shouldEqualOk` r1
        r2 <- exec erlps__test_completely_casual_foldl__3
             [ ErlangFun 2 BIF.erlang__op_div
             , toErl 210
             , toErl (map toErl [2,3,5,7])
             ]
        toErl 1 `shouldEqualOk` r2
      it "Deprecated catch / throw" do
        testExecOk ok erlps__test_deprecated_catch_throw__0 []
      it "Deprecated catch / error" do
        testExecOk (atomTup ["EXIT", "ok"]) erlps__test_deprecated_catch_error__0 []
      it "Deprecated catch / exit" do
        testExecOk (atomTup ["EXIT", "ok"]) erlps__test_deprecated_catch_exit__0 []
      it "Properly orders operations 1" do
        testExecOk ok erlps__test_ordering_1__0 []
      it "Properly orders operations 2" do
        testExecOk ok erlps__test_ordering_2__0 []
      it "Properly orders operations 3" do
        testExecOk ok erlps__test_ordering_3__0 []
      it "Doesn't fuck up List Comprehensions" do
        testExecOk ok erlps__test_lc__0 []
      it "test_scope_1" do
        testExecOk ok erlps__test_scope_1__0 []
      it "test_scope_2" do
        testExecOk ok erlps__test_scope_2__0 []
      it "test_scope_3" do
        testExecOk ok erlps__test_scope_3__0 []

    describe_ "Cancer..." do
      it "IO works :O" do
        testExecOk ok erlps__test_wtf__0 []

    --describe "Real b64 tests from OTP" do
    --  it "base64_encode" do
    --        r <- exec B64S.erlps__base64_encode__1 [ErlangEmptyList]
    --        makeOk (ErlangAtom "ok") `shouldEqual` r
    --  it "base64_decode" do
    --        r <- exec B64S.erlps__base64_decode__1 [ErlangEmptyList]
    --        makeOk (ErlangAtom "ok") `shouldEqual` r

    describe_ "Processes OwO" do
      it "can spawn" do
        testExecOk ok erlps__test_spawn__0 []
      it "can move aff monad to process" do
          (T.Tuple _ r) <- liftAffToErlangProcess (\_ -> pure "OK")
          "OK" `shouldEqual` r
      it "can request it's own pid" do
          (T.Tuple pid1 packed_pid) <- liftAffToErlangProcess (\_ -> exec erlps__test_get_self__0 [])
          pid1 `shouldEqualOk` packed_pid
      --it "receive primop - simple case" do
      --    (T.Tuple _ r) <- liftAffToErlangProcess (\_ -> exec erlps__test_simple_receive_primop__0 [])
      --    ok `shouldEqualOk` r

    describe_ "Binaries" do
      let bin b = ErlangBinary (unsafePerformEffect $ Buf.fromArray b)
      it "Build empty" do
        testExecOk (bin []) erlps__test_build_empty__0 []
      it "Build single" do
        testExecOk (bin [1]) erlps__test_build_single__0 []
      it "Build multi" do
        testExecOk (bin [1,2,3]) erlps__test_build_multi__0 []
      it "Build string" do
        testExecOk (bin [88,68]) erlps__test_build_string__0 []
      it "Build subbinary" do
        testExecOk (bin [1,2,3]) erlps__test_build_bin__0 []
      it "Build float" do
        testExecOk (bin [63,240,0,0,0,0,0,0]) erlps__test_build_float__0 []
      it "Build mixed" do
        testExecOk (bin [88,68,1,2,3,4,64,20,0,0,0,0,0,0]) erlps__test_build_mixed__0 []
      it "Build single" do
        testExecOk (bin [1]) erlps__test_build_single__0 []

      it "Build int reg. size" do
        testExecOk (bin [0,0,0,1]) erlps__test_build_int_size__0 []
      it "Build int reg. size&unit" do
        testExecOk (bin [0,0,0,1]) erlps__test_build_int_size_unit__0 []
      it "Build int reg. little end" do
        testExecOk (bin [1,0,0,0]) erlps__test_build_int_endian_little__0 []
      it "Build int reg. underflow" do
        testExecOk (bin [255]) erlps__test_build_int_underflow__0 []
      it "Build int reg. overflow" do
        testExecOk (bin [0]) erlps__test_build_int_overflow__0 []

      it "Build string reg. size&unit" do
        testExecOk (bin [0,88,0,68]) erlps__test_build_string_size_unit__0 []

      it "Build float reg. size" do
        testExecOk (bin [63,128,0,0]) erlps__test_build_float_32__0 []
      it "Build float reg. size&unit" do
        testExecOk (bin [63,128,0,0]) erlps__test_build_float_32_unit__0 []
      it "Build float reg. little end" do
        testExecOk (bin [0,0,0,0,0,0,240,63]) erlps__test_build_float_little__0 []

      it "Build comprehension" do
        testExecOk (bin [1,0,4,2,0,4,3,0,4]) erlps__test_build_comprehension__0 []

      it "Match empty" do
        testExecOk ok erlps__test_match_empty__0 []
      it "Match single" do
        testExecOk ok erlps__test_match_single__0 []
      it "Match single var" do
        testExecOk ok erlps__test_match_single_var__0 []
      it "Match many" do
        testExecOk ok erlps__test_match_many__0 []
      it "Match int size" do
        testExecOk ok erlps__test_match_int_size__0 []
      it "Match int size&unit" do
        testExecOk ok erlps__test_match_int_size_unit__0 []
      it "Match int little" do
        testExecOk ok erlps__test_match_int_little__0 []
      it "Match int signed" do
        testExecOk ok erlps__test_match_int_signed__0 []
      it "Match int mix" do
        testExecOk ok erlps__test_match_int_mix__0 []
      it "Match string" do
        testExecOk ok erlps__test_match_string__0 []
      it "Match bin" do
        testExecOk ok erlps__test_match_bin__0 []
      it "Match bin at the end" do
        testExecOk ok erlps__test_match_bin_end__0 []
      it "Match float64" do
        testExecOk ok erlps__test_match_float_64__0 []
      it "Match float32" do
        testExecOk ok erlps__test_match_float_32__0 []
      it "Match wildcard binary/bitstring" do
        testExecOk ok erlps__test_match_wildcard_binary__0 []
      it "Match padded bitstring" do
        testExecOk ok erlps__test_match_padded_bitstring__0 []


    describe_ "Code server" do
      it "Circular dependencies - One -> One local call" do
        testExecOk ((ErlangAtom "hello_from_module_one")) C1.erlps__one__0 []
      it "Circular dependencies - One -> Two remote call" do
        testExecOk ((ErlangAtom "hello_from_module_two")) C1.erlps__two__0 []
      it "Circular dependencies - Two -> One local call" do
        testExecOk ((ErlangAtom "hello_from_module_one")) C2.erlps__one__0 []
      it "Circular dependencies - Two -> Two remote call" do
        testExecOk ((ErlangAtom "hello_from_module_two")) C2.erlps__two__0 []
      it "Onload" do
        testExecOk ((ErlangAtom "hehe")) C3.erlps__test_onload__0 []

    describe_ "Autoimport" do
      it "Not all function in the erlang module are autoimported :)" do
        testExecOk ok Auto.erlps__test_autoimports__0 []
