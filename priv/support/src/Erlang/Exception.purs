module Erlang.Exception
  ( tryCatchFinally
  , tryOfCatchFinally
  , tryCatch
  , tryOfCatch
  , raise
  , throw, error, exit
  , function_clause, case_clause, try_clause, if_clause
  , badarity, badmatch, badarg, badrecord
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


function_clause :: Unit -> ErlangTerm
function_clause _ = error (ErlangAtom "function_clause")

case_clause :: ErlangTerm -> ErlangTerm
case_clause term = error (ErlangTuple [ErlangAtom "case_clause", term])

if_clause :: Unit -> ErlangTerm
if_clause _ = error (ErlangAtom "if_clause")

try_clause :: ErlangTerm -> ErlangTerm
try_clause term = error (ErlangTuple [ErlangAtom "try_clause", term])

badmatch :: ErlangTerm -> ErlangTerm
badmatch term = error (ErlangTuple [ErlangAtom "badmatch", term])

badarg :: Unit -> ErlangTerm
badarg _ = error (ErlangAtom "badarg")

badarity :: ErlangTerm -> Array ErlangTerm -> ErlangTerm
badarity fun args =
  error (ErlangTuple [ErlangAtom "badarity", ErlangTuple [fun, arrayToErlangList args]])

badrecord :: ErlangTerm -> ErlangTerm
badrecord _ = error (ErlangAtom "TODO: PROPER BADRECORD ERROR")
