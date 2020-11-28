module Erlang.Ioserver where

import Prelude
import Effect.Unsafe
import Effect.Console (log)

import Erlang.Type

erlps__request__2 [io_server@(ErlangPID _), ErlangTuple [ErlangAtom "io_request", from@(ErlangPID _), replyAs, request]] =
    let
        a = unsafePerformEffect (log (show request))
    in
        ErlangTuple [ErlangAtom "io_reply", replyAs, ErlangAtom "ok"]
erlps__request__2 args = ErlangAtom "ok"
