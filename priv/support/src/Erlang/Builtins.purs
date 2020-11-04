module Erlang.Builtins where

import Erlang.Type
import Prelude
import Data.Maybe as DM
import Data.Array as DA
import Data.List as DL
import Control.Monad

erlang''op_plus :: ErlangFun
erlang''op_plus [ErlangNum x, ErlangNum y] = pure $ ErlangNum (x + y)

erlang''op_minus :: ErlangFun
erlang''op_minus [ErlangNum x, ErlangNum y] = pure $ ErlangNum (x - y)

erlang''op_mult :: ErlangFun
erlang''op_mult [ErlangNum x, ErlangNum y] = pure $ ErlangNum (x * y)

erlang''op_div :: ErlangFun
erlang''op_div [ErlangNum x, ErlangNum y] = pure $ ErlangNum (x / y)

erlang''apply''2 :: ErlangFun
erlang''apply''2 [ErlangFun arity@(ErlangNum arityVal) fun, args]
  | DM.Just argsL <- erlangListToList args
  , DL.length argsL == arityVal =
    fun args

lists''reverse''2 :: ErlangFun
lists''reverse''2 [ErlangEmptyList, acc] = pure acc
lists''reverse''2 [ErlangCons h t, acc] =
  lists''reverse''2 [t, ErlangCons h acc]

erlang''length''1 :: ErlangFun
erlang''length''1 [l] = pure $ ErlangNum (go 0 l) where
  go acc ErlangEmptyList = acc
  go acc (ErlangCons _ t) = go (acc + 1) t
