module Erlang.Helpers where

import Erlang.Type
import Control.Monad
import Data.Maybe as DM
import Data.List as DL
import Data.Array as DA
import Effect
import Effect.Unsafe(unsafePerformEffect)
import Effect.Exception(throw)
import Prelude
import Control.Semigroupoid((<<<), (>>>))

error :: forall a. String -> a
error = throw >>> unsafePerformEffect

applyTerm :: ErlangFun
applyTerm [ErlangFun arityVal fun, args]
  | DM.Just argsL <- erlangListToList args
  , DL.length argsL == arityVal = fun (DA.fromFoldable argsL)

rbind :: forall a b. (a -> Effect b) -> Effect a -> Effect b
rbind = flip bind

rbindOver :: forall a b. Effect (a -> Effect b) -> Effect a -> Effect b
rbindOver = (<<<)((>>>)(>>=))(>>=)  -- don't ask, just beta-reduce
