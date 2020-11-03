module Erlang.Builtins where

import Erlang.Type

erlang''op_plus [ErlangNum x, ErlangNum y] = ErlangNum (x + y)

erlang''op_minus [ErlangNum x, ErlangNum y] = ErlangNum (x - y)

erlang''op_mult [ErlangNum x, ErlangNum y] = ErlangNum (x * y)

erlang''op_div [ErlangNum x, ErlangNum y] = ErlangNum (x / y)

erlangApply [ErlangFun arity@(ErlangNum arityVal) fun, args]
  | Just argsL <- erlangListToList args
  , length argsL == arityVal =
    fun args


lists''reverse''2 [ErlangEmptyList] acc = acc
lists''reverse''2 [ErlangCons h t] acc =
  lists''reverse''2 t (ErlangCons h acc)

erlang''length''1 l = ErlangNum (go 0 l) where
  go acc [ErlangEmptyList] = acc
  go acc [ErlangCons _ t] = go (acc + 1) t
