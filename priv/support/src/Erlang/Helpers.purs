module Erlang.Helpers where

import Erlang.Type
import Control.Monad
import Data.Maybe as DM
import Data.List as DL
import Data.Array as DA
import Effect
import Effect.Unsafe(unsafePerformEffect)
import Effect.Exception(throw, catchException)
import Prelude
import Control.Semigroupoid((<<<), (>>>))

erlCaseIf :: forall a. ErlangTerm -> a -> a -> a
erlCaseIf (ErlangAtom "true") t _  = t
erlCaseIf (ErlangAtom "false") _ e = e
erlCaseIf _ _ _ = error "case_clause"

error :: forall a. String -> a
error = throw >>> unsafePerformEffect

applyTerm :: Partial => ErlangTerm -> ErlangFun
applyTerm (ErlangFun arityVal fun) argsL |
  DA.length argsL == arityVal = fun argsL

unsafePerformEffectGuard :: Effect ErlangTerm -> ErlangTerm
unsafePerformEffectGuard action =
  unsafePerformEffect (catchException (\_ -> pure (ErlangAtom "false")) action)

rbind :: forall a b. (a -> Effect b) -> Effect a -> Effect b
rbind = flip bind

rbindOver :: forall a b. Effect (a -> Effect b) -> Effect a -> Effect b
rbindOver = (<<<)((>>>)(>>=))(>>=)  -- don't ask, just beta-reduce

isEL :: ErlangTerm -> Boolean
isEL ErlangEmptyList = true
isEL (ErlangCons _ _) = true
isEL _ = false
