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

foreign import raise :: Exception -> Effect ErlangTerm

foreign import getStack :: Effect String

foreign import tryCatchFinally
  :: forall a. (Unit -> Effect ErlangTerm)
  -> (Exception -> Effect ErlangTerm)
  -> (Unit -> Effect a)
  -> Effect ErlangTerm

foreign import tryOfCatchFinally
  :: forall a. (Unit -> Effect ErlangTerm)
  -> (ErlangTerm -> Effect ErlangTerm)
  -> (Exception -> Effect ErlangTerm)
  -> (Unit -> Effect a)
  -> Effect ErlangTerm


foreign import tryCatch
  :: (Unit -> Effect ErlangTerm)
  -> (Exception -> Effect ErlangTerm)
  -> Effect ErlangTerm

foreign import tryOfCatch
  :: (Unit -> Effect ErlangTerm)
  -> (ErlangTerm -> Effect ErlangTerm)
  -> (Exception -> Effect ErlangTerm)
  -> Effect ErlangTerm

throw :: ErlangTerm -> Effect ErlangTerm
throw term = do
  stack <- getStack
  raise $ buildException "throw" term stack

error :: ErlangTerm -> Effect ErlangTerm
error term = do
  stack <- getStack
  raise $ buildException "error" term stack

exit :: ErlangTerm -> Effect ErlangTerm
exit term = do
  stack <- getStack
  raise $ buildException "exit" term stack


