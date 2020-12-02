module Erlang.Ioserver where

import Prelude
import Effect.Unsafe
import Effect.Console (log)

import Data.Array as A
import Data.Maybe as M
import Partial.Unsafe
import Erlang.Type
import Data.String.CodePoints as StrCP
import Data.String as Str
import Unsafe.Coerce

wololo_codepoint :: Partial => ErlangTerm -> StrCP.CodePoint
wololo_codepoint (ErlangInt res) = unsafeCoerce res

toString x = unsafePartial $ Str.fromCodePointArray $ map wololo_codepoint $ A.fromFoldable $ M.fromJust $ erlangListToList x

erlps__request__2 [io_server@(ErlangPID _), ErlangTuple [ErlangAtom "io_request", from@(ErlangPID _), replyAs, request]] =
    let
        a = unsafePerformEffect (log (show request))
    in
        ErlangTuple [ErlangAtom "io_reply", replyAs, handle_request request]
erlps__request__2 args = ErlangAtom "ok"

handle_request (ErlangTuple [ErlangAtom "put_chars", ErlangAtom "unicode", ErlangAtom "io_lib", ErlangAtom "format", ErlangCons format (ErlangCons ErlangEmptyList ErlangEmptyList)]) =
    let
        a = unsafePerformEffect (log ((toString format)))
    in
          ErlangAtom "ok"
handle_request (ErlangTuple [ErlangAtom "put_chars", ErlangAtom "unicode", ErlangAtom "io_lib", ErlangAtom "format", ErlangCons (ErlangAtom "user") (ErlangCons format ErlangEmptyList)]) =
    let
        a = unsafePerformEffect (log ((toString format)))
    in
          ErlangAtom "ok"
handle_request (ErlangTuple [ErlangAtom "put_chars", ErlangAtom "unicode", ErlangAtom "io_lib", ErlangAtom "format", ErlangCons format (ErlangCons terms ErlangEmptyList)]) =
    let
        a = unsafePerformEffect (log ((toString format) <> " " <> show terms))
    in
          ErlangAtom "ok"
handle_request _ = ErlangAtom "ok"
