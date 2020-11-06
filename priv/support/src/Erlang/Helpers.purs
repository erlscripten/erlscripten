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

erlIf :: forall a. ErlangTerm -> a -> a -> a
erlIf (ErlangAtom "true") t _  = t
erlIf (ErlangAtom "false") _ e = e
erlIf _ _ _ = error "if_clause"

error :: forall a. String -> a
error = throw >>> unsafePerformEffect

applyTerm :: ErlangFun
applyTerm [ErlangFun arityVal fun, args]
  | DM.Just argsL <- erlangListToList args
  , DL.length argsL == arityVal = fun (DA.fromFoldable argsL)

unsafePerformEffectGuard :: Effect ErlangTerm -> ErlangTerm
unsafePerformEffectGuard action =
  unsafePerformEffect (catchException (\_ -> pure (ErlangAtom "false") action))

rbind :: forall a b. (a -> Effect b) -> Effect a -> Effect b
rbind = flip bind

rbindOver :: forall a b. Effect (a -> Effect b) -> Effect a -> Effect b
rbindOver = (<<<)((>>>)(>>=))(>>=)  -- don't ask, just beta-reduce
