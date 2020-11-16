module Erlang.Helpers where

import Erlang.Type
import Control.Monad
import Data.Maybe as DM
import Data.List as DL
import Data.Array as DA
import Data.String as Str
import Data.String.CodePoints as StrCP
import Data.Foldable

import Unsafe.Coerce
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

isEL :: ErlangTerm -> Boolean
isEL ErlangEmptyList = true
isEL (ErlangCons _ _) = true
isEL _ = false

-- They removed support of it. CodePoint is just a newtype for Int.
codePointToInt :: StrCP.CodePoint -> Int
codePointToInt = unsafeCoerce

make_string :: String -> ErlangTerm
make_string str = arrayToErlangList (map (ErlangNum <<< codePointToInt) (Str.toCodePointArray str))

