module Erlang.Exception
  ( tryCatchFinally
  , tryOfCatchFinally
  , tryCatch
  , tryOfCatch
  , raise
  , throw, error, exit
  , error_badarg
  ) where

import Prelude
import Erlang.Type
import Erlang.Helpers

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

buildException :: String -> ErlangTerm -> String -> ErlangTerm
buildException exType exPayload exStack = ErlangTuple
  [ ErlangAtom exType
  , exPayload
  , make_string exStack
  ]

foreign import raise :: ErlangTerm -> ErlangTerm

foreign import getStack :: Unit -> String

foreign import tryCatchFinally
  :: forall a. (Unit -> ErlangTerm)
  -> (ErlangTerm -> ErlangTerm)
  -> (Unit -> a)
  -> ErlangTerm

foreign import tryOfCatchFinally
  :: forall a. (Unit -> ErlangTerm)
  -> (ErlangTerm -> ErlangTerm)
  -> (ErlangTerm -> ErlangTerm)
  -> (Unit -> a)
  -> ErlangTerm


foreign import tryCatch
  :: (Unit -> ErlangTerm)
  -> (ErlangTerm -> ErlangTerm)
  -> ErlangTerm

foreign import tryOfCatch
  :: (Unit -> ErlangTerm)
  -> (ErlangTerm -> ErlangTerm)
  -> (ErlangTerm -> ErlangTerm)
  -> ErlangTerm

throw :: ErlangTerm -> ErlangTerm
throw term =
  raise $ buildException "throw" term (getStack unit)

error :: ErlangTerm -> ErlangTerm
error term =
  raise $ buildException "error" term (getStack unit)

exit :: ErlangTerm -> ErlangTerm
exit term =
  raise $ buildException "exit" term (getStack unit)


function_clause :: ErlangTerm -> ErlangTerm
function_clause term =
  error (ErlangTuple [ErlangAtom "function_clause", term])

case_clause :: ErlangTerm -> ErlangTerm
case_clause term =
  error (ErlangTuple [ErlangAtom "case_clause", term])

if_clause :: ErlangTerm -> ErlangTerm
if_clause term =
  error (ErlangTuple [ErlangAtom "if_clause", term])

try_clause :: ErlangTerm -> ErlangTerm
try_clause term =
  error (ErlangTuple [ErlangAtom "try_clause", term])

bad_match :: ErlangTerm -> ErlangTerm
bad_match term =
  error (ErlangTuple [ErlangAtom "badmatch", term])

error_badarg :: Unit -> ErlangTerm
error_badarg _ = error (ErlangAtom "badarg")
