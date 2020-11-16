module Erlang.Exception
  ( Exception
  , tryCatchFinally
  , tryOfCatchFinally
  , tryCatch
  , tryOfCatch
  , throw, error, exit
  ) where

import Prelude
import Erlang.Type
import Erlang.Helpers

import Effect (Effect)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

type Exception =
  { exceptionType :: ErlangTerm
  , exceptionPayload :: ErlangTerm
  , exceptionStack :: ErlangTerm
  }

buildException :: String -> ErlangTerm -> String -> Exception
buildException exType exPayload exStack =
  { exceptionType: ErlangAtom exType
  , exceptionPayload: exPayload
  , exceptionStack: make_string exStack
  }

foreign import throwImpl :: forall a. String -> ErlangTerm -> a

foreign import tryCatchFinallyImpl
  :: forall a. (String -> ErlangTerm -> String -> Exception)
  -> (Unit -> Effect ErlangTerm)
  -> (Exception -> Effect ErlangTerm)
  -> (Unit -> Effect a)
  -> Effect ErlangTerm

foreign import tryOfCatchFinallyImpl
  :: forall a. (String -> ErlangTerm -> String -> Exception)
  -> (Unit -> Effect ErlangTerm)
  -> (ErlangTerm -> Effect ErlangTerm)
  -> (Exception -> Effect ErlangTerm)
  -> (Unit -> Effect a)
  -> Effect ErlangTerm

throw :: forall a. ErlangTerm -> a
throw term = throwImpl "throw" term

error :: forall a. ErlangTerm -> a
error term = throwImpl "error" term

exit :: forall a. ErlangTerm -> a
exit term = throwImpl "exit" term

tryCatchFinally :: forall a. (Unit -> Effect ErlangTerm)
                -> (Exception -> Effect ErlangTerm)
                -> (Unit -> Effect a)
                -> Effect ErlangTerm
tryCatchFinally = tryCatchFinallyImpl buildException

tryCatch :: (Unit -> Effect ErlangTerm)
         -> (Exception -> Effect ErlangTerm)
         -> Effect ErlangTerm
tryCatch ex handler =
  tryCatchFinallyImpl buildException ex handler (\_ -> pure unit)

tryOfCatchFinally :: forall a. (Unit -> Effect ErlangTerm)
                  -> (ErlangTerm -> Effect ErlangTerm)
                  -> (Exception -> Effect ErlangTerm)
                  -> (Unit -> Effect a)
                  -> Effect ErlangTerm
tryOfCatchFinally = tryOfCatchFinallyImpl buildException

tryOfCatch :: (Unit -> Effect ErlangTerm)
           -> (ErlangTerm -> Effect ErlangTerm)
           -> (Exception -> Effect ErlangTerm)
           -> Effect ErlangTerm
tryOfCatch ex ofHandler handler =
  tryOfCatchFinallyImpl buildException ex ofHandler handler (\_ -> pure unit)
