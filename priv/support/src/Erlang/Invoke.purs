module Erlang.Invoke where

-- Processes are realized in Erlps using ES6 generators
-- Unfortunatelly Purescript is not aware of ES6 or generators
-- This module is a FFI wraper which tries to determine whether someone passed us
-- a ES6 generator or a normal function - we assume that if we have a generator
-- then we expect it to return immediately - in tests please delegate anything blocking
-- to a separate process which might block

import Erlang.Type

foreign import do_run_erlang :: ErlangFun -> Array ErlangTerm -> ErlangTerm
run_erlang :: ErlangFun -> Array ErlangTerm -> ErlangTerm
run_erlang fun args = do_run_erlang fun args
