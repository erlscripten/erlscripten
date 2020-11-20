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

error :: forall a. String -> a
error = throw >>> unsafePerformEffect

applyTerm :: Partial => ErlangTerm -> ErlangFun
applyTerm (ErlangFun arityVal fun) argsL |
  DA.length argsL == arityVal = fun argsL

isEL :: ErlangTerm -> Boolean
isEL ErlangEmptyList = true
isEL (ErlangCons _ _) = true
isEL _ = false

-- They removed support of it. CodePoint is just a newtype for Int.
codePointToInt :: StrCP.CodePoint -> Int
codePointToInt = unsafeCoerce

make_string :: String -> ErlangTerm
make_string str = arrayToErlangList (map (ErlangNum <<< codePointToInt) (Str.toCodePointArray str))

flmap :: ErlangFun
flmap [ErlangFun 1 f, list] = erflat (ermap list ErlangEmptyList) ErlangEmptyList where
  ermap :: ErlangTerm -> ErlangTerm -> ErlangTerm
  ermap ErlangEmptyList acc = acc
  ermap (ErlangCons h t) acc = ermap t (ErlangCons (f [h]) acc)

  erflat :: ErlangTerm -> ErlangTerm -> ErlangTerm
  erflat ErlangEmptyList acc = acc
  erflat (ErlangCons ErlangEmptyList rest) acc = erflat rest acc
  erflat (ErlangCons (ErlangCons h t) rest) acc = erflat (ErlangCons t rest) (ErlangCons h acc)
