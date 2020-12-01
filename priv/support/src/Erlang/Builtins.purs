module Erlang.Builtins where

import Erlang.Type
import Erlang.Exception as EXC
import Erlang.Helpers as H
import Prelude
import Data.Maybe as DM
import Data.Array as DA
import Data.List as DL
import Data.Int as DI
import Data.Int.Bits as DIB
import Math
import Control.Monad
import Effect.Exception (throw)
import Effect
import Effect.Unsafe

unimplemented :: String -> ErlangTerm
unimplemented name = unsafePerformEffect (throw $ "unimplemented BIF: " <> name)

purs_tco_sucks :: ErlangFun
purs_tco_sucks _ = ErlangAtom "purs_tco_sucks"

--------------------------------------------------------------------------------
--- Code server OwO
foreign import do_ffi_remote_fun_call :: String -> String -> Array ErlangTerm -> (Unit -> ErlangTerm) -> ErlangTerm
do_remote_fun_call :: String -> String -> Array ErlangTerm -> ErlangTerm
do_remote_fun_call mName fName args =
    do_ffi_remote_fun_call mName fName args (\_ -> EXC.error $ ErlangAtom "undef")

erlang__apply__2 :: ErlangFun
erlang__apply__2 [ft@(ErlangFun arity f), args0] | ErlangTuple args1 <- erlang__list_to_tuple__1 [args0] =
    case (DA.length args1) == arity of
        true ->
            f args1
        false ->
            EXC.error $ ErlangTuple [ErlangAtom "badarity", ft]
erlang__apply__2 [_,_] = EXC.badarg unit
erlang__apply__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__apply__2-}) args

foreign import do_apply_4 :: String -> String -> Array ErlangTerm -> (Unit -> ErlangTerm) -> ErlangTerm
erlang__apply__3 :: ErlangFun
erlang__apply__3 [ErlangAtom m, ErlangAtom f, args0] | ErlangTuple args1 <- erlang__list_to_tuple__1 [args0] =
    do_apply_4 m f args1 (\_ -> EXC.error $ ErlangAtom "undef")
erlang__apply__3 [_,_,_] = EXC.badarg unit
erlang__apply__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__apply__3-}) args

erlang__make_fun__3 :: ErlangFun
erlang__make_fun__3 [m@(ErlangAtom _), f@(ErlangAtom _), ErlangInt arity] =
    ErlangFun arity
        (\ args -> erlang__apply__3 [m, f, arrayToErlangList args] )
erlang__make_fun__3 [_,_,_] = EXC.badarg unit
erlang__make_fun__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__make_fun__3-}) args

--------------------------------------------------------------------------------
--- FLOAT BIFS

math__pi__0 :: ErlangFun
math__pi__0 [] = ErlangFloat pi
math__pi__0 args = EXC.badarity (ErlangFun 0 purs_tco_sucks {-math__pi__0-}) args

math_arr1 :: Array ErlangTerm -> (Number -> Number) -> ErlangTerm
math_arr1 [ErlangInt num] f = math_arr1 [ErlangFloat $ DI.toNumber num] f
math_arr1 [ErlangFloat arg] f = ErlangFloat (f arg)
math_arr1 [_] _ = EXC.badarg unit
math_arr1 args _ = EXC.badarity (ErlangFun 1 purs_tco_sucks {-math_arr1_1-}) args

math_arr2 :: Array ErlangTerm -> (Number -> Number -> Number) -> ErlangTerm
math_arr2 [ErlangInt num, arg2] f = math_arr2 [ErlangFloat $ DI.toNumber num, arg2] f
math_arr2 [arg1, ErlangInt num] f = math_arr2 [arg1, ErlangFloat $ DI.toNumber num] f
math_arr2 [ErlangFloat arg1, ErlangFloat arg2] f = ErlangFloat (f arg1 arg2)
math_arr2 [_] _ = EXC.badarg unit
math_arr2 args _ = EXC.badarity (ErlangFun 2 purs_tco_sucks {-math_arr2_1-}) args

math__sin__1 args = math_arr1 args sin
math__cos__1 args = math_arr1 args cos
math__tan__1 args = math_arr1 args tan
math__asin__1 args = math_arr1 args asin
math__acos__1 args = math_arr1 args acos
math__atan__1 args = math_arr1 args atan
math__sinh__1 args = math_arr1 args sinh
math__cosh__1 args = math_arr1 args cosh
math__tanh__1 args = math_arr1 args tanh
math__asinh__1 args = math_arr1 args asinh
math__acosh__1 args = math_arr1 args acosh
math__atanh__1 args = math_arr1 args atanh
math__exp__1 args = math_arr1 args exp
math__log__1 args = math_arr1 args log
math__log2__1 args = math_arr1 args log2
math__log10__1 args = math_arr1 args log10
math__sqrt__1 args = math_arr1 args sqrt
-- FIXME: erf(X) = 2/sqrt(pi)*integral from 0 to X of exp(-t*t) dt.
-- TODO: display middle finger when someone calls this xD
math__erf__1 args = unimplemented "math__erf__1"
-- FIXME: erfc(X) = 1 - erf(X)
math__erfc__1 args = unimplemented "math__erfc__1"
math__ceil__1 args = math_arr1 args ceil
math__floor__1 args = math_arr1 args floor

math__atan2__2 args = math_arr2 args atan2
math__pow__2 args = math_arr2 args pow
math__fmod__2 args = math_arr2 args (%)

erlang__abs__1 :: ErlangFun
erlang__abs__1 [ErlangInt a] | a >= 0 = ErlangInt a
erlang__abs__1 [ErlangInt a] = ErlangInt (-a)
erlang__abs__1 [ErlangFloat a] = ErlangFloat (abs a)
erlang__abs__1 [_] = EXC.badarg unit
erlang__abs__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__abs__1-}) args

erlang__ceil__1 :: ErlangFun
erlang__ceil__1 args = unimplemented "erlang__ceil__1"
erlang__ceil__1 [_] = EXC.badarg unit
erlang__ceil__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__ceil__1-}) args

erlang__floor__1 :: ErlangFun
erlang__floor__1 args = unimplemented "erlang__floor__1"
erlang__floor__1 [_] = EXC.badarg unit
erlang__floor__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__floor__1-}) args

erlang__float__1 :: ErlangFun
erlang__float__1 args = unimplemented "erlang__float__1"
erlang__float__1 [_] = EXC.badarg unit
erlang__float__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__float__1-}) args

--------------------------------------------------------------------------------
-- LISTS BIFS

lists__keysearch__3 :: ErlangFun
lists__keysearch__3 [_, _, ErlangEmptyList] = boolToTerm false
lists__keysearch__3 [key, idx@(ErlangInt idxNum), ErlangCons el rest] = case el of
  ErlangTuple tup | DM.Just x <- DA.index tup idxNum  ->
    case erlang__op_exactEq [x, key] of
      ErlangAtom "true" -> (ErlangTuple [ErlangAtom "value", el])
      _                 -> lists__keysearch__3 [key, idx, rest]
  _ -> lists__keysearch__3 [key, idx, rest]
lists__keysearch__3 [_,_,_] = EXC.badarg unit
lists__keysearch__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-lists__keysearch__3-}) args

lists__keymember__3 :: ErlangFun
lists__keymember__3 [_, _, ErlangEmptyList] = boolToTerm false
lists__keymember__3 [key, idx@(ErlangInt idxNum), ErlangCons el rest] = case el of
  ErlangTuple tup | DM.Just x <- DA.index tup idxNum  ->
    case erlang__op_exactEq [x, key] of
      ErlangAtom "true" -> ErlangAtom "true"
      _                 -> lists__keymember__3 [key, idx, rest]
  _ -> lists__keymember__3 [key, idx, rest]
lists__keymember__3 [_,_,_] = EXC.badarg unit
lists__keymember__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-lists__keymember__3-}) args

lists__reverse__2 :: ErlangFun
lists__reverse__2 [ErlangEmptyList, acc] = acc
lists__reverse__2 [ErlangCons h t, acc] = lists__reverse__2 [t, ErlangCons h acc]
lists__reverse__2 [_,_] = EXC.badarg unit
lists__reverse__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-lists__reverse__2-}) args

lists__member__2 :: ErlangFun
lists__member__2 [_, ErlangEmptyList] = boolToTerm false
lists__member__2 [x, ErlangCons el rest] =
  case erlang__op_exactEq [x, el] of
    ErlangAtom "true" -> ErlangAtom "true"
    _                 -> lists__member__2 [x, rest]
lists__member__2 [_,_] = EXC.badarg unit
lists__member__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-lists__member__2-}) args

lists__keyfind__3 :: ErlangFun
lists__keyfind__3 [_, _, ErlangEmptyList] = boolToTerm false
lists__keyfind__3 [key, idx@(ErlangInt idxNum), ErlangCons el rest] = case el of
  ErlangTuple tup | DM.Just x <- DA.index tup idxNum  ->
    case erlang__op_exactEq [x, key] of
      ErlangAtom "true" -> el
      _                 -> lists__keyfind__3 [key, idx, rest]
  _ -> lists__keyfind__3 [key, idx, rest]
lists__keyfind__3 [_,_,_] = EXC.badarg unit
lists__keyfind__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-lists__keyfind__3-}) args

erlang__hd__1 :: ErlangFun
erlang__hd__1 args = unimplemented "erlang__hd__1"
erlang__hd__1 [_] = EXC.badarg unit
erlang__hd__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__hd__1-}) args

erlang__tl__1 :: ErlangFun
erlang__tl__1 args = unimplemented "erlang__tl__1"
erlang__tl__1 [_] = EXC.badarg unit
erlang__tl__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__tl__1-}) args

erlang__append__2 :: ErlangFun
erlang__append__2 args = unimplemented "erlang__append__2"
erlang__append__2 [_,_] = EXC.badarg unit
erlang__append__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__append__2-}) args

erlang__length__1 :: ErlangFun
erlang__length__1 [ErlangEmptyList] = ErlangInt 0
erlang__length__1 [ErlangCons _ t] =
    case erlang__length__1 [t] of
      ErlangInt tl -> ErlangInt $ tl+1
      _ -> EXC.badarg unit
erlang__length__1 [_] = EXC.badarg unit
erlang__length__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__length__1-}) args

erlang__subtract__2 :: ErlangFun
erlang__subtract__2 args = unimplemented "erlang__subtract__2"
erlang__subtract__2 [_,_] = EXC.badarg unit
erlang__subtract__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__subtract__2-}) args

--------------------------------------------------------------------------------
--- MAP BIFS

maps__get__2 :: ErlangFun
maps__get__2 [] = unimplemented "maps__get__2"
maps__get__2 [_,_] = EXC.badarg unit
maps__get__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-maps__get__2-}) args

maps__find__2 :: ErlangFun
maps__find__2 [] = unimplemented "maps__find__2"
maps__find__2 [_,_] = EXC.badarg unit
maps__find__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-maps__find__2-}) args

maps__from_list__1 :: ErlangFun
maps__from_list__1 [] = unimplemented "maps__from_list__1"
maps__from_list__1 [_] = EXC.badarg unit
maps__from_list__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-maps__from_list__1-}) args

maps__is_key__2 :: ErlangFun
maps__is_key__2 [] = unimplemented "maps__is_key__2"
maps__is_key__2 [_,_] = EXC.badarg unit
maps__is_key__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-maps__is_key__2-}) args

maps__keys__1 :: ErlangFun
maps__keys__1 [] = unimplemented "maps__keys__1"
maps__keys__1 [_] = EXC.badarg unit
maps__keys__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-maps__keys__1-}) args

maps__merge__2 :: ErlangFun
maps__merge__2 [] = unimplemented "maps__merge__2"
maps__merge__2 [_,_] = EXC.badarg unit
maps__merge__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-maps__merge__2-}) args
maps__merge__2 [_,_] = EXC.badarg unit
maps__merge__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-maps__merge__2-}) args

maps__put__3 :: ErlangFun
maps__put__3 [] = unimplemented "maps__put__3"
maps__put__3 [_,_,_] = EXC.badarg unit
maps__put__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-maps__put__3-}) args

maps__remove__2 :: ErlangFun
maps__remove__2 [] = unimplemented "maps__remove__2"
maps__remove__2 [_,_] = EXC.badarg unit
maps__remove__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-maps__remove__2-}) args

maps__take__2 :: ErlangFun
maps__take__2 [] = unimplemented "maps__take__2"
maps__take__2 [_,_] = EXC.badarg unit
maps__take__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-maps__take__2-}) args

maps__to_list__1 :: ErlangFun
maps__to_list__1 [] = unimplemented "maps__to_list__1"
maps__to_list__1 [_] = EXC.badarg unit
maps__to_list__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-maps__to_list__1-}) args

maps__update__3 :: ErlangFun
maps__update__3 [] = unimplemented "maps__update__3"
maps__update__3 [_,_,_] = EXC.badarg unit
maps__update__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-maps__update__3-}) args

maps__values__1 :: ErlangFun
maps__values__1 [] = unimplemented "maps__values__1"
maps__values__1 [_] = EXC.badarg unit
maps__values__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-maps__values__1-}) args

erts_internal__map_next__3 :: ErlangFun
erts_internal__map_next__3 _ = unimplemented "erts_internal__map_next__3"

erlang__map_size__1 :: ErlangFun
erlang__map_size__1 args = unimplemented "erlang__map_size__1"
erlang__map_size__1 [_] = EXC.badarg unit
erlang__map_size__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__map_size__1-}) args

erlang__is_map_key__2 :: ErlangFun
erlang__is_map_key__2 args = unimplemented "erlang__is_map_key__2"
erlang__is_map_key__2 [_,_] = EXC.badarg unit
erlang__is_map_key__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__is_map_key__2-}) args

erlang__map_get__2 :: ErlangFun
erlang__map_get__2 args = unimplemented "erlang__map_get__2"
erlang__map_get__2 [_,_] = EXC.badarg unit
erlang__map_get__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__map_get__2-}) args

--------------------------------------------------------------------------------
--- BINARY AND UNARY OPERATIONS

-- =/=
erlang__op_exactNeq :: ErlangFun
erlang__op_exactNeq [a, b] = boolToTerm (a /= b)  -- FIXME (funs)
erlang__op_exactNeq [_, _] = EXC.badarg unit
erlang__op_exactNeq args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__op_exactNeq-}) args

-- =:=
erlang__op_exactEq :: ErlangFun
erlang__op_exactEq [a, b] = boolToTerm (a == b) -- FIXME (funs)
erlang__op_exactEq [_, _] = EXC.badarg unit
erlang__op_exactEq args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__op_exactEq-}) args

-- /=
erlang__op_neq :: ErlangFun
erlang__op_neq [a, b] = boolToTerm (a /= b) -- FIXME (funs, floats)
erlang__op_neq [_, _] = EXC.badarg unit
erlang__op_neq args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__op_neq-}) args
erlang__op_neq [_, _] = EXC.badarg unit
erlang__op_neq args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__op_neq-}) args

-- ==
erlang__op_eq :: ErlangFun
erlang__op_eq [a, b] = boolToTerm (a == b) -- FIXME (funs, floats)
erlang__op_eq [_, _] = EXC.badarg unit
erlang__op_eq args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__op_eq-}) args

-- and
erlang__op_and :: ErlangFun
erlang__op_and [ErlangAtom "true",  ErlangAtom "true"]  = boolToTerm true
erlang__op_and [ErlangAtom "false", ErlangAtom "true"]  = boolToTerm false
erlang__op_and [ErlangAtom "true",  ErlangAtom "false"] = boolToTerm false
erlang__op_and [ErlangAtom "false", ErlangAtom "false"] = boolToTerm false
erlang__op_and [_, _] = EXC.badarg unit
erlang__op_and args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__op_and-}) args

-- or
erlang__op_or :: ErlangFun
erlang__op_or [ErlangAtom "true",  ErlangAtom "true"]  = boolToTerm true
erlang__op_or [ErlangAtom "false", ErlangAtom "true"]  = boolToTerm true
erlang__op_or [ErlangAtom "true",  ErlangAtom "false"] = boolToTerm true
erlang__op_or [ErlangAtom "false", ErlangAtom "false"] = boolToTerm false
erlang__op_or [_, _] = EXC.badarg unit
erlang__op_or args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__op_or-}) args

-- andalso
erlang__op_andalso :: ErlangFun
erlang__op_andalso [ErlangAtom "true", other] = other
erlang__op_andalso [ErlangAtom "false", _] = boolToTerm false
erlang__op_andalso [_, _] = EXC.badarg unit
erlang__op_andalso args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__op_andalso-}) args

-- orelse
erlang__op_orelse :: ErlangFun
erlang__op_orelse [ErlangAtom "true", _] = boolToTerm true
erlang__op_orelse [ErlangAtom "false", other] = other
erlang__op_orelse [_, _] = EXC.badarg unit
erlang__op_orelse args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__op_orelse-}) args

-- /
erlang__op_div :: ErlangFun
erlang__op_div [ErlangInt a, ErlangInt b] = ErlangInt (a / b)
erlang__op_div [ErlangInt a, ErlangFloat b] = ErlangFloat ((DI.toNumber a) / b)
erlang__op_div [ErlangFloat a, ErlangInt b] = ErlangFloat (a / (DI.toNumber b))
erlang__op_div [ErlangFloat a, ErlangFloat b] = ErlangFloat (a / b)
erlang__op_div [_, _] = EXC.badarg unit
erlang__op_div args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__op_div-}) args

-- 'div'
erlang__op_div_strict :: ErlangFun
erlang__op_div_strict [ErlangInt a, ErlangInt b] = ErlangInt (a `div` b)
erlang__op_div_strict [_, _] = EXC.badarg unit
erlang__op_div_strict args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__op_div-}) args

-- %
erlang__op_rem :: ErlangFun
erlang__op_rem [ErlangInt left, ErlangInt right] = ErlangInt (mod left right)
--erlang__op_rem [ErlangInt a, ErlangFloat b] = ErlangInt ((DI.toNumber a) / b) -- FIXME
--erlang__op_rem [ErlangFloat a, ErlangInt b] = ErlangFloat (a / (DI.toNumber b)) -- FIXME
erlang__op_rem [ErlangFloat a, ErlangFloat b] = ErlangFloat (a % b)
erlang__op_rem [_,_] = EXC.badarg unit
erlang__op_rem args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__rem__2-}) args

-- 'rem'
erlang__op_rem_strict :: ErlangFun
erlang__op_rem_strict [ErlangInt left, ErlangInt right] = ErlangInt (mod left right)
erlang__op_rem_strict [_,_] = EXC.badarg unit
erlang__op_rem_strict args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__rem__2-}) args

-- *
erlang__op_mult :: ErlangFun
erlang__op_mult [ErlangInt a, ErlangInt b] = ErlangInt (a * b)
erlang__op_mult [ErlangInt a, ErlangFloat b] = ErlangFloat ((DI.toNumber a) * b)
erlang__op_mult [ErlangFloat a, ErlangInt b] = ErlangFloat (a * (DI.toNumber b))
erlang__op_mult [ErlangFloat a, ErlangFloat b] = ErlangFloat (a * b)
erlang__op_mult [_, _] = EXC.badarg unit
erlang__op_mult args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__op_mult-}) args

-- -
erlang__op_minus :: ErlangFun
erlang__op_minus [ErlangInt a, ErlangInt b] = ErlangInt (a - b)
erlang__op_minus [ErlangInt a, ErlangFloat b] = ErlangFloat ((DI.toNumber a) - b)
erlang__op_minus [ErlangFloat a, ErlangInt b] = ErlangFloat (a - (DI.toNumber b))
erlang__op_minus [ErlangFloat a, ErlangFloat b] = ErlangFloat (a - b)
erlang__op_minus [_, _] = EXC.badarg unit
erlang__op_minus args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__op_minus-}) args

-- +
erlang__op_plus :: ErlangFun
erlang__op_plus [ErlangInt a, ErlangInt b] = ErlangInt (a + b)
erlang__op_plus [ErlangInt a, ErlangFloat b] = ErlangFloat ((DI.toNumber a) + b)
erlang__op_plus [ErlangFloat a, ErlangInt b] = ErlangFloat (a + (DI.toNumber b))
erlang__op_plus [ErlangFloat a, ErlangFloat b] = ErlangFloat (a + b)
erlang__op_plus [_, _] = EXC.badarg unit
erlang__op_plus args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__op_plus-}) args

-- >=
erlang__op_greaterEq :: ErlangFun
erlang__op_greaterEq [a, b] = boolToTerm (a >= b)
erlang__op_greaterEq [_, _] = EXC.badarg unit
erlang__op_greaterEq args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__op_greaterEq-}) args

-- >
erlang__op_greater :: ErlangFun
erlang__op_greater [a, b] = boolToTerm (a > b)
erlang__op_greater [_, _] = EXC.badarg unit
erlang__op_greater args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__op_greater-}) args

-- =<
erlang__op_lesserEq :: ErlangFun
erlang__op_lesserEq [a, b] = boolToTerm (a <= b)
erlang__op_lesserEq [_, _] = EXC.badarg unit
erlang__op_lesserEq args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__op_lesserEq-}) args

-- <
erlang__op_lesser :: ErlangFun
erlang__op_lesser [a, b] = boolToTerm (a < b)
erlang__op_lesser [_, _] = EXC.badarg unit
erlang__op_lesser args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__op_lesser-}) args

-- --
erlang__op_unAppend :: ErlangFun
erlang__op_unAppend [ErlangEmptyList, ErlangEmptyList] = ErlangEmptyList
erlang__op_unAppend [l@(ErlangCons _ _), ErlangEmptyList] = l
erlang__op_unAppend [ErlangEmptyList, ErlangCons _ _] = ErlangEmptyList
erlang__op_unAppend [ErlangCons hl tl, r@(ErlangCons hr tr)] =
  case erlang__op_exactEq [hl, hr] of
    ErlangAtom "true"  -> erlang__op_unAppend [tl, tr]
    _                  -> ErlangCons hl (erlang__op_unAppend [tl, r])
erlang__op_unAppend [_, _] = EXC.badarg unit
erlang__op_unAppend args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__op_unAppend-}) args

-- ++
erlang__op_append :: ErlangFun
erlang__op_append [ErlangEmptyList, l] = l
erlang__op_append [ErlangCons h t, l] =
  ErlangCons h (erlang__op_append [t, l])
erlang__op_append [_, _] = EXC.badarg unit
erlang__op_append args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__op_append-}) args

-- -
erlang__op_neg :: ErlangFun
erlang__op_neg [ErlangInt n] = ErlangInt (-n)
erlang__op_neg [ErlangFloat n] = ErlangFloat (-n)
erlang__op_neg [_] = EXC.badarg unit
erlang__op_neg args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__op_neg-}) args

-- not
erlang__op_not :: ErlangFun
erlang__op_not [ErlangAtom "false"] = ErlangAtom "true"
erlang__op_not [ErlangAtom "true"] = ErlangAtom "false"
erlang__op_not [_] = EXC.badarg unit
erlang__op_not args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__op_not-}) args

erlang__bor__2 :: ErlangFun
erlang__bor__2 [ErlangInt a, ErlangInt b] = ErlangInt $ DIB.or a b
erlang__bor__2 [_,_] = EXC.badarg unit
erlang__bor__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__bor__2-}) args

erlang__bnot__1 :: ErlangFun
erlang__bnot__1 [ErlangInt a] = ErlangInt $ DIB.complement a
erlang__bnot__1 [_] = EXC.badarg unit
erlang__bnot__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__bnot__1-}) args

erlang__bxor__2 :: ErlangFun
erlang__bxor__2 [ErlangInt a, ErlangInt b] = ErlangInt $ DIB.xor a b
erlang__bxor__2 [_,_] = EXC.badarg unit
erlang__bxor__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__bxor__2-}) args

erlang__or__2 :: ErlangFun
erlang__or__2 args = unimplemented "erlang__or__2"
erlang__or__2 [_,_] = EXC.badarg unit
erlang__or__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__or__2-}) args

erlang__bsr__2 :: ErlangFun
erlang__bsr__2 [ErlangInt a, ErlangInt b] = ErlangInt $ DIB.shr a b
erlang__bsr__2 [_,_] = EXC.badarg unit
erlang__bsr__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__bsr__2-}) args

erlang__xor__2 :: ErlangFun
erlang__xor__2 args = unimplemented "erlang__xor__2"
erlang__xor__2 [_,_] = EXC.badarg unit
erlang__xor__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__xor__2-}) args

erlang__bsl__2 :: ErlangFun
erlang__bsl__2 [ErlangInt a, ErlangInt b] = ErlangInt $ DIB.shl a b
erlang__bsl__2 [_,_] = EXC.badarg unit
erlang__bsl__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__bsl__2-}) args

erlang__band__2 :: ErlangFun
erlang__band__2 [ErlangInt a, ErlangInt b] = ErlangInt $ DIB.and a b
erlang__band__2 [_,_] = EXC.badarg unit
erlang__band__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__band__2-}) args

erlang__min__2 :: ErlangFun
erlang__min__2 [t1, t2] | t1 <= t2  = t1
                        | otherwise = t2
erlang__min__2 [_,_] = EXC.badarg unit
erlang__min__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__min__2-}) args


erlang__max__2 :: ErlangFun
erlang__max__2 [t1, t2] | t1 >= t2  = t1
                        | otherwise = t2
erlang__max__2 [_,_] = EXC.badarg unit
erlang__max__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__max__2-}) args

--------------------------------------------------------------------------------
--- Type tests

erlang__is_integer__1 :: ErlangFun
erlang__is_integer__1 [ErlangInt _] = ErlangAtom "true"
erlang__is_integer__1 [_] = ErlangAtom "false"
erlang__is_integer__1 [_] = EXC.badarg unit
erlang__is_integer__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__is_integer__1-}) args

erlang__is_float__1 :: ErlangFun
erlang__is_float__1 args = unimplemented "erlang__is_float__1"
erlang__is_float__1 [_] = EXC.badarg unit
erlang__is_float__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__is_float__1-}) args

erlang__is_binary__1 :: ErlangFun
erlang__is_binary__1 [ErlangBinary _] = ErlangAtom "true" --FIXME: should return false for bitstrings
erlang__is_binary__1 [_] = ErlangAtom "false"
erlang__is_binary__1 [_] = EXC.badarg unit
erlang__is_binary__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__is_binary__1-}) args

erlang__is_bitstring__1 :: ErlangFun
erlang__is_bitstring__1 args = unimplemented "erlang__is_bitstring__1"
erlang__is_bitstring__1 [_] = EXC.badarg unit
erlang__is_bitstring__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__is_bitstring__1-}) args

erlang__is_port__1 :: ErlangFun
erlang__is_port__1 args = unimplemented "erlang__is_port__1"
erlang__is_port__1 [_] = EXC.badarg unit
erlang__is_port__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__is_port__1-}) args

erlang__is_boolean__1 :: ErlangFun
erlang__is_boolean__1 [ErlangAtom "true"] = boolToTerm true
erlang__is_boolean__1 [ErlangAtom "false"] = boolToTerm true
erlang__is_boolean__1 [_] = boolToTerm false
erlang__is_boolean__1 [_] = EXC.badarg unit
erlang__is_boolean__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__is_boolean__1-}) args

erlang__is_record__2 :: ErlangFun
erlang__is_record__2 args = unimplemented "erlang__is_record__2"
erlang__is_record__2 [_,_] = EXC.badarg unit
erlang__is_record__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__is_record__2-}) args

erlang__is_tuple__1 :: ErlangFun
erlang__is_tuple__1 args = unimplemented "erlang__is_tuple__1"
erlang__is_tuple__1 [_] = EXC.badarg unit
erlang__is_tuple__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__is_tuple__1-}) args

erlang__is_atom__1 :: ErlangFun
erlang__is_atom__1 args = unimplemented "erlang__is_atom__1"
erlang__is_atom__1 [_] = EXC.badarg unit
erlang__is_atom__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__is_atom__1-}) args

erlang__is_number__1 :: ErlangFun
erlang__is_number__1 args = unimplemented "erlang__is_number__1"
erlang__is_number__1 [_] = EXC.badarg unit
erlang__is_number__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__is_number__1-}) args

erlang__is_pid__1 :: ErlangFun
erlang__is_pid__1 [ErlangPID _] = boolToTerm true
erlang__is_pid__1 [_] = boolToTerm false
erlang__is_pid__1 [_] = EXC.badarg unit
erlang__is_pid__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__is_pid__1-}) args

erlang__is_function__1 :: ErlangFun
erlang__is_function__1 args = unimplemented "erlang__is_function__1"
erlang__is_function__1 [_] = EXC.badarg unit
erlang__is_function__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__is_function__1-}) args

erlang__is_reference__1 :: ErlangFun
erlang__is_reference__1 args = unimplemented "erlang__is_reference__1"
erlang__is_reference__1 [_] = EXC.badarg unit
erlang__is_reference__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__is_reference__1-}) args

erlang__is_list__1 :: ErlangFun
erlang__is_list__1 [ErlangEmptyList] = boolToTerm true
erlang__is_list__1 [ErlangCons _ _] = boolToTerm true
erlang__is_list__1 [_] = boolToTerm false
erlang__is_list__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__is_list__1-}) args

erlang__is_map__1 :: ErlangFun
erlang__is_map__1 args = unimplemented "erlang__is_map__1"
erlang__is_map__1 [_] = EXC.badarg unit
erlang__is_map__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__is_map__1-}) args

erlang__is_function__2 :: ErlangFun
erlang__is_function__2 args = unimplemented "erlang__is_function__2"
erlang__is_function__2 [_,_] = EXC.badarg unit
erlang__is_function__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__is_function__2-}) args

erlang__is_record__3 :: ErlangFun
erlang__is_record__3 args = unimplemented "erlang__is_record__3"
erlang__is_record__3 [_,_,_] = EXC.badarg unit
erlang__is_record__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__is_record__3-}) args

--------------------------------------------------------------------------------
--- Type Casts

erlang__integer_to_binary__2 :: ErlangFun
erlang__integer_to_binary__2 args = unimplemented "erlang__integer_to_binary__2"
erlang__integer_to_binary__2 [_,_] = EXC.badarg unit
erlang__integer_to_binary__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__integer_to_binary__2-}) args

erlang__integer_to_list__2 :: ErlangFun
erlang__integer_to_list__2 [ErlangInt num, ErlangInt base]
    | DM.Just radix <- DI.radix base
    = H.make_string $ DI.toStringAs radix num
erlang__integer_to_list__2 [_,_] = EXC.badarg unit
erlang__integer_to_list__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__integer_to_list__2-}) args

erlang__list_to_float__1 :: ErlangFun
erlang__list_to_float__1 args = unimplemented "erlang__list_to_float__1"
erlang__list_to_float__1 [_] = EXC.badarg unit
erlang__list_to_float__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__list_to_float__1-}) args

erlang__list_to_integer__2 :: ErlangFun
erlang__list_to_integer__2 args = unimplemented "erlang__list_to_integer__2"
erlang__list_to_integer__2 [_,_] = EXC.badarg unit
erlang__list_to_integer__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__list_to_integer__2-}) args

erlang__binary_to_atom__2 :: ErlangFun
erlang__binary_to_atom__2 args = unimplemented "erlang__binary_to_atom__2"
erlang__binary_to_atom__2 [_,_] = EXC.badarg unit
erlang__binary_to_atom__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__binary_to_atom__2-}) args

erlang__binary_to_term__2 :: ErlangFun
erlang__binary_to_term__2 args = unimplemented "erlang__binary_to_term__2"
erlang__binary_to_term__2 [_,_] = EXC.badarg unit
erlang__binary_to_term__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__binary_to_term__2-}) args

erlang__integer_to_binary__1 :: ErlangFun
erlang__integer_to_binary__1 args = unimplemented "erlang__integer_to_binary__1"
erlang__integer_to_binary__1 [_] = EXC.badarg unit
erlang__integer_to_binary__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__integer_to_binary__1-}) args

erlang__iolist_to_iovec__1 :: ErlangFun
erlang__iolist_to_iovec__1 args = unimplemented "erlang__iolist_to_iovec__1"
erlang__iolist_to_iovec__1 [_] = EXC.badarg unit
erlang__iolist_to_iovec__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__iolist_to_iovec__1-}) args

erlang__iolist_to_binary__1 :: ErlangFun
erlang__iolist_to_binary__1 args = unimplemented "erlang__iolist_to_binary__1"
erlang__iolist_to_binary__1 [_] = EXC.badarg unit
erlang__iolist_to_binary__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__iolist_to_binary__1-}) args

erlang__list_to_bitstring__1 :: ErlangFun
erlang__list_to_bitstring__1 args = unimplemented "erlang__list_to_bitstring__1"
erlang__list_to_bitstring__1 [_] = EXC.badarg unit
erlang__list_to_bitstring__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__list_to_bitstring__1-}) args

erlang__pid_to_list__1 :: ErlangFun
erlang__pid_to_list__1 args = unimplemented "erlang__pid_to_list__1"
erlang__pid_to_list__1 [_] = EXC.badarg unit
erlang__pid_to_list__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__pid_to_list__1-}) args

erlang__binary_to_integer__2 :: ErlangFun
erlang__binary_to_integer__2 args = unimplemented "erlang__binary_to_integer__2"
erlang__binary_to_integer__2 [_,_] = EXC.badarg unit
erlang__binary_to_integer__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__binary_to_integer__2-}) args

erlang__list_to_existing_atom__1 :: ErlangFun
erlang__list_to_existing_atom__1 args = unimplemented "erlang__list_to_existing_atom__1"
erlang__list_to_existing_atom__1 [_] = EXC.badarg unit
erlang__list_to_existing_atom__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__list_to_existing_atom__1-}) args

erlang__binary_to_existing_atom__2 :: ErlangFun
erlang__binary_to_existing_atom__2 args = unimplemented "erlang__binary_to_existing_atom__2"
erlang__binary_to_existing_atom__2 [_,_] = EXC.badarg unit
erlang__binary_to_existing_atom__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__binary_to_existing_atom__2-}) args

erlang__port_to_list__1 :: ErlangFun
erlang__port_to_list__1 args = unimplemented "erlang__port_to_list__1"
erlang__port_to_list__1 [_] = EXC.badarg unit
erlang__port_to_list__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__port_to_list__1-}) args

erlang__float_to_list__1 :: ErlangFun
erlang__float_to_list__1 args = unimplemented "erlang__float_to_list__1"
erlang__float_to_list__1 [_] = EXC.badarg unit
erlang__float_to_list__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__float_to_list__1-}) args

erlang__float_to_list__2 :: ErlangFun
erlang__float_to_list__2 args = unimplemented "erlang__float_to_list__2"
erlang__float_to_list__2 [_,_] = EXC.badarg unit
erlang__float_to_list__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__float_to_list__2-}) args


erlang__binary_to_list__3 :: ErlangFun
erlang__binary_to_list__3 args = unimplemented "erlang__binary_to_list__3"
erlang__binary_to_list__3 [_,_,_] = EXC.badarg unit
erlang__binary_to_list__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__binary_to_list__3-}) args

erlang__list_to_atom__1 :: ErlangFun
erlang__list_to_atom__1 args = unimplemented "erlang__list_to_atom__1"
erlang__list_to_atom__1 [_] = EXC.badarg unit
erlang__list_to_atom__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__list_to_atom__1-}) args

erlang__ref_to_list__1 :: ErlangFun
erlang__ref_to_list__1 args = unimplemented "erlang__ref_to_list__1"
erlang__ref_to_list__1 [_] = EXC.badarg unit
erlang__ref_to_list__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__ref_to_list__1-}) args

erlang__list_to_binary__1 :: ErlangFun
erlang__list_to_binary__1 args = unimplemented "erlang__list_to_binary__1"
erlang__list_to_binary__1 [_] = EXC.badarg unit
erlang__list_to_binary__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__list_to_binary__1-}) args

erlang__binary_to_list__1 :: ErlangFun
erlang__binary_to_list__1 args = unimplemented "erlang__binary_to_list__1"
erlang__binary_to_list__1 [_] = EXC.badarg unit
erlang__binary_to_list__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__binary_to_list__1-}) args

erlang__term_to_binary__1 :: ErlangFun
erlang__term_to_binary__1 args = unimplemented "erlang__term_to_binary__1"
erlang__term_to_binary__1 [_] = EXC.badarg unit
erlang__term_to_binary__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__term_to_binary__1-}) args

erlang__term_to_binary__2 :: ErlangFun
erlang__term_to_binary__2 args = unimplemented "erlang__term_to_binary__2"
erlang__term_to_binary__2 [_,_] = EXC.badarg unit
erlang__term_to_binary__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__term_to_binary__2-}) args

erlang__tuple_to_list__1 :: ErlangFun
erlang__tuple_to_list__1 args = unimplemented "erlang__tuple_to_list__1"
erlang__tuple_to_list__1 [_] = EXC.badarg unit
erlang__tuple_to_list__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__tuple_to_list__1-}) args

erlang__binary_to_term__1 :: ErlangFun
erlang__binary_to_term__1 args = unimplemented "erlang__binary_to_term__1"
erlang__binary_to_term__1 [_] = EXC.badarg unit
erlang__binary_to_term__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__binary_to_term__1-}) args

erlang__list_to_integer__1 :: ErlangFun
erlang__list_to_integer__1 args = unimplemented "erlang__list_to_integer__1"
erlang__list_to_integer__1 [_] = EXC.badarg unit
erlang__list_to_integer__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__list_to_integer__1-}) args

erlang__list_to_port__1 :: ErlangFun
erlang__list_to_port__1 args = unimplemented "erlang__list_to_port__1"
erlang__list_to_port__1 [_] = EXC.badarg unit
erlang__list_to_port__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__list_to_port__1-}) args

erlang__binary_to_float__1 :: ErlangFun
erlang__binary_to_float__1 args = unimplemented "erlang__binary_to_float__1"
erlang__binary_to_float__1 [_] = EXC.badarg unit
erlang__binary_to_float__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__binary_to_float__1-}) args

erlang__list_to_ref__1 :: ErlangFun
erlang__list_to_ref__1 args = unimplemented "erlang__list_to_ref__1"
erlang__list_to_ref__1 [_] = EXC.badarg unit
erlang__list_to_ref__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__list_to_ref__1-}) args

erlang__float_to_binary__1 :: ErlangFun
erlang__float_to_binary__1 args = unimplemented "erlang__float_to_binary__1"
erlang__float_to_binary__1 [_] = EXC.badarg unit
erlang__float_to_binary__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__float_to_binary__1-}) args

erlang__atom_to_binary__2 :: ErlangFun
erlang__atom_to_binary__2 args = unimplemented "erlang__atom_to_binary__2"
erlang__atom_to_binary__2 [_,_] = EXC.badarg unit
erlang__atom_to_binary__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__atom_to_binary__2-}) args

erlang__binary_to_integer__1 :: ErlangFun
erlang__binary_to_integer__1 args = unimplemented "erlang__binary_to_integer__1"
erlang__binary_to_integer__1 [_] = EXC.badarg unit
erlang__binary_to_integer__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__binary_to_integer__1-}) args

erlang__atom_to_list__1 :: ErlangFun
erlang__atom_to_list__1 [ErlangAtom atom] = H.make_string atom
erlang__atom_to_list__1 [_] = EXC.badarg unit
erlang__atom_to_list__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__atom_to_list__1-}) args

erlang__integer_to_list__1 :: ErlangFun
erlang__integer_to_list__1 [ErlangInt num] = H.make_string $ DI.toStringAs DI.decimal num
erlang__integer_to_list__1 [_] = EXC.badarg unit
erlang__integer_to_list__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__integer_to_list__1-}) args

erlang__fun_to_list__1 :: ErlangFun
erlang__fun_to_list__1 args = unimplemented "erlang__fun_to_list__1"
erlang__fun_to_list__1 [_] = EXC.badarg unit
erlang__fun_to_list__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__fun_to_list__1-}) args

erlang__list_to_pid__1 :: ErlangFun
erlang__list_to_pid__1 args = unimplemented "erlang__list_to_pid__1"
erlang__list_to_pid__1 [_] = EXC.badarg unit
erlang__list_to_pid__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__list_to_pid__1-}) args

erlang__float_to_binary__2 :: ErlangFun
erlang__float_to_binary__2 args = unimplemented "erlang__float_to_binary__2"
erlang__float_to_binary__2 [_,_] = EXC.badarg unit
erlang__float_to_binary__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__float_to_binary__2-}) args

erlang__list_to_tuple__1 :: ErlangFun
erlang__list_to_tuple__1 [list] | DM.Just r <- erlangListToList list =
    ErlangTuple (DA.fromFoldable r)
erlang__list_to_tuple__1 [_] = EXC.badarg unit
erlang__list_to_tuple__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-ErlangTuple-}) args

erlang__bitstring_to_list__1 :: ErlangFun
erlang__bitstring_to_list__1 args = unimplemented "erlang__bitstring_to_list__1"
erlang__bitstring_to_list__1 [_] = EXC.badarg unit
erlang__bitstring_to_list__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__bitstring_to_list__1-}) args

--------------------------------------------------------------------------------

erlang__process_display__2 :: ErlangFun
erlang__process_display__2 args = unimplemented "erlang__process_display__2"
erlang__process_display__2 [_,_] = EXC.badarg unit
erlang__process_display__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__process_display__2-}) args

erlang__fun_info_mfa__1 :: ErlangFun
erlang__fun_info_mfa__1 args = unimplemented "erlang__fun_info_mfa__1"
erlang__fun_info_mfa__1 [_] = EXC.badarg unit
erlang__fun_info_mfa__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__fun_info_mfa__1-}) args

erlang__nif_error__2 :: ErlangFun
erlang__nif_error__2 args = erlang__error__2 args
erlang__nif_error__2 [_,_] = EXC.badarg unit
erlang__nif_error__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__nif_error__2-}) args

erlang__get_stacktrace__0 :: ErlangFun
erlang__get_stacktrace__0 args = unimplemented "erlang__get_stacktrace__0"

erlang__registered__0 :: ErlangFun
erlang__registered__0 args = unimplemented "erlang__registered__0"

erlang__get_module_info__1 :: ErlangFun
erlang__get_module_info__1 args = unimplemented "erlang__get_module_info__1"
erlang__get_module_info__1 [_] = EXC.badarg unit
erlang__get_module_info__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__get_module_info__1-}) args

erlang__module_info__1 :: ErlangFun
erlang__module_info__1 args = unimplemented "erlang__module_info__1"
erlang__module_info__1 [_] = EXC.badarg unit
erlang__module_info__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__module_info__1-}) args

erlang__cancel_timer__1 :: ErlangFun
erlang__cancel_timer__1 args = unimplemented "erlang__cancel_timer__1"
erlang__cancel_timer__1 [_] = EXC.badarg unit
erlang__cancel_timer__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__cancel_timer__1-}) args

erlang__dist_ctrl_get_data_notification__1 :: ErlangFun
erlang__dist_ctrl_get_data_notification__1 args = unimplemented "erlang__dist_ctrl_get_data_notification__1"
erlang__dist_ctrl_get_data_notification__1 [_] = EXC.badarg unit
erlang__dist_ctrl_get_data_notification__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__dist_ctrl_get_data_notification__1-}) args

erlang__is_builtin__3 :: ErlangFun
erlang__is_builtin__3 args = unimplemented "erlang__is_builtin__3"
erlang__is_builtin__3 [_,_,_] = EXC.badarg unit
erlang__is_builtin__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__is_builtin__3-}) args

erlang__suspend_process__1 :: ErlangFun
erlang__suspend_process__1 args = unimplemented "erlang__suspend_process__1"
erlang__suspend_process__1 [_] = EXC.badarg unit
erlang__suspend_process__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__suspend_process__1-}) args

erlang__spawn_link__2 :: ErlangFun
erlang__spawn_link__2 args = unimplemented "erlang__spawn_link__2"
erlang__spawn_link__2 [_,_] = EXC.badarg unit
erlang__spawn_link__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__spawn_link__2-}) args

erlang__get__1 :: ErlangFun
erlang__get__1 args = unimplemented "erlang__get__1"
erlang__get__1 [_] = EXC.badarg unit
erlang__get__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__get__1-}) args

erlang__dist_ctrl_get_data__1 :: ErlangFun
erlang__dist_ctrl_get_data__1 args = unimplemented "erlang__dist_ctrl_get_data__1"
erlang__dist_ctrl_get_data__1 [_] = EXC.badarg unit
erlang__dist_ctrl_get_data__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__dist_ctrl_get_data__1-}) args

erlang__setnode__3 :: ErlangFun
erlang__setnode__3 args = unimplemented "erlang__setnode__3"
erlang__setnode__3 [_,_,_] = EXC.badarg unit
erlang__setnode__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__setnode__3-}) args

erlang__now__0 :: ErlangFun
erlang__now__0 args = unimplemented "erlang__now__0"

erlang__erase__0 :: ErlangFun
erlang__erase__0 args = unimplemented "erlang__erase__0"

erlang__ports__0 :: ErlangFun
erlang__ports__0 args = unimplemented "erlang__ports__0"

erlang__dt_spread_tag__1 :: ErlangFun
erlang__dt_spread_tag__1 args = unimplemented "erlang__dt_spread_tag__1"
erlang__dt_spread_tag__1 [_] = EXC.badarg unit
erlang__dt_spread_tag__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__dt_spread_tag__1-}) args

erlang__convert_time_unit__3 :: ErlangFun
erlang__convert_time_unit__3 args = unimplemented "erlang__convert_time_unit__3"
erlang__convert_time_unit__3 [_,_,_] = EXC.badarg unit
erlang__convert_time_unit__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__convert_time_unit__3-}) args

erlang__decode_packet__3 :: ErlangFun
erlang__decode_packet__3 args = unimplemented "erlang__decode_packet__3"
erlang__decode_packet__3 [_,_,_] = EXC.badarg unit
erlang__decode_packet__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__decode_packet__3-}) args

erlang__get_cookie__0 :: ErlangFun
erlang__get_cookie__0 args = unimplemented "erlang__get_cookie__0"

erlang__put__2 :: ErlangFun
erlang__put__2 args = unimplemented "erlang__put__2"
erlang__put__2 [_,_] = EXC.badarg unit
erlang__put__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__put__2-}) args

erlang__unique_integer__1 :: ErlangFun
erlang__unique_integer__1 args = unimplemented "erlang__unique_integer__1"
erlang__unique_integer__1 [_] = EXC.badarg unit
erlang__unique_integer__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__unique_integer__1-}) args

erlang__exit__2 :: ErlangFun
erlang__exit__2 args = unimplemented "erlang__exit__2"
erlang__exit__2 [_,_] = EXC.badarg unit
erlang__exit__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__exit__2-}) args

erlang__purge_module__1 :: ErlangFun
erlang__purge_module__1 args = unimplemented "erlang__purge_module__1"
erlang__purge_module__1 [_] = EXC.badarg unit
erlang__purge_module__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__purge_module__1-}) args

erlang__dt_prepend_vm_tag_data__1 :: ErlangFun
erlang__dt_prepend_vm_tag_data__1 args = unimplemented "erlang__dt_prepend_vm_tag_data__1"
erlang__dt_prepend_vm_tag_data__1 [_] = EXC.badarg unit
erlang__dt_prepend_vm_tag_data__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__dt_prepend_vm_tag_data__1-}) args

erlang__has_prepared_code_on_load__1 :: ErlangFun
erlang__has_prepared_code_on_load__1 args = unimplemented "erlang__has_prepared_code_on_load__1"
erlang__has_prepared_code_on_load__1 [_] = EXC.badarg unit
erlang__has_prepared_code_on_load__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__has_prepared_code_on_load__1-}) args

erlang__external_size__2 :: ErlangFun
erlang__external_size__2 args = unimplemented "erlang__external_size__2"
erlang__external_size__2 [_,_] = EXC.badarg unit
erlang__external_size__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__external_size__2-}) args

erlang__is_alive__0 :: ErlangFun
erlang__is_alive__0 args = unimplemented "erlang__is_alive__0"

erlang__make_tuple__2 :: ErlangFun
erlang__make_tuple__2 [ErlangInt arity, what] = ErlangTuple $ DA.replicate arity what
erlang__make_tuple__2 [_,_] = EXC.badarg unit
erlang__make_tuple__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__make_tuple__2-}) args

erlang__and__2 :: ErlangFun
erlang__and__2 args = unimplemented "erlang__and__2"
erlang__and__2 [_,_] = EXC.badarg unit
erlang__and__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__and__2-}) args

foreign import do_is_process_alive_1 :: Int -> Boolean
erlang__is_process_alive__1 :: ErlangFun
erlang__is_process_alive__1 [ErlangPID id] =
    boolToTerm $ do_is_process_alive_1 id
erlang__is_process_alive__1 [_] = EXC.badarg unit
erlang__is_process_alive__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__is_process_alive__1-}) args

erlang__dist_get_stat__1 :: ErlangFun
erlang__dist_get_stat__1 args = unimplemented "erlang__dist_get_stat__1"
erlang__dist_get_stat__1 [_] = EXC.badarg unit
erlang__dist_get_stat__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__dist_get_stat__1-}) args

erlang__alloc_sizes__1 :: ErlangFun
erlang__alloc_sizes__1 args = unimplemented "erlang__alloc_sizes__1"
erlang__alloc_sizes__1 [_] = EXC.badarg unit
erlang__alloc_sizes__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__alloc_sizes__1-}) args

erlang__spawn_opt__2 :: ErlangFun
erlang__spawn_opt__2 args = unimplemented "erlang__spawn_opt__2"
erlang__spawn_opt__2 [_,_] = EXC.badarg unit
erlang__spawn_opt__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__spawn_opt__2-}) args

erlang__iolist_size__1 :: ErlangFun
erlang__iolist_size__1 args = unimplemented "erlang__iolist_size__1"
erlang__iolist_size__1 [_] = EXC.badarg unit
erlang__iolist_size__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__iolist_size__1-}) args

erlang__element__2 :: ErlangFun
erlang__element__2 [ErlangInt pos, ErlangTuple array] | DM.Just res <- DA.index array (pos-1) = res
erlang__element__2 [_,_] = EXC.badarg unit
erlang__element__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__element__2-}) args

erlang__port_get_data__1 :: ErlangFun
erlang__port_get_data__1 args = unimplemented "erlang__port_get_data__1"
erlang__port_get_data__1 [_] = EXC.badarg unit
erlang__port_get_data__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__port_get_data__1-}) args

erlang__group_leader__2 :: ErlangFun
erlang__group_leader__2 args = unimplemented "erlang__group_leader__2"
erlang__group_leader__2 [_,_] = EXC.badarg unit
erlang__group_leader__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__group_leader__2-}) args

erlang__split_binary__2 :: ErlangFun
erlang__split_binary__2 args = unimplemented "erlang__split_binary__2"
erlang__split_binary__2 [_,_] = EXC.badarg unit
erlang__split_binary__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__split_binary__2-}) args

erlang__function_exported__3 :: ErlangFun
erlang__function_exported__3 args = unimplemented "erlang__function_exported__3"
erlang__function_exported__3 [_,_,_] = EXC.badarg unit
erlang__function_exported__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__function_exported__3-}) args

erlang__phash__2 :: ErlangFun
erlang__phash__2 args = unimplemented "erlang__phash__2"
erlang__phash__2 [_,_] = EXC.badarg unit
erlang__phash__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__phash__2-}) args

erlang__dist_ctrl_put_data__2 :: ErlangFun
erlang__dist_ctrl_put_data__2 args = unimplemented "erlang__dist_ctrl_put_data__2"
erlang__dist_ctrl_put_data__2 [_,_] = EXC.badarg unit
erlang__dist_ctrl_put_data__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__dist_ctrl_put_data__2-}) args

erlang__garbage_collect_message_area__0 :: ErlangFun
erlang__garbage_collect_message_area__0 args = unimplemented "erlang__garbage_collect_message_area__0"

erlang__spawn_link__1 :: ErlangFun
erlang__spawn_link__1 args = unimplemented "erlang__spawn_link__1"
erlang__spawn_link__1 [_] = EXC.badarg unit
erlang__spawn_link__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__spawn_link__1-}) args

erlang__garbage_collect__0 :: ErlangFun
erlang__garbage_collect__0 args = unimplemented "erlang__garbage_collect__0"

erlang__trace_pattern__3 :: ErlangFun
erlang__trace_pattern__3 args = unimplemented "erlang__trace_pattern__3"
erlang__trace_pattern__3 [_,_,_] = EXC.badarg unit
erlang__trace_pattern__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__trace_pattern__3-}) args

erlang__dt_restore_tag__1 :: ErlangFun
erlang__dt_restore_tag__1 args = unimplemented "erlang__dt_restore_tag__1"
erlang__dt_restore_tag__1 [_] = EXC.badarg unit
erlang__dt_restore_tag__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__dt_restore_tag__1-}) args

erlang__system_profile__0 :: ErlangFun
erlang__system_profile__0 args = unimplemented "erlang__system_profile__0"

erlang__match_spec_test__3 :: ErlangFun
erlang__match_spec_test__3 args = unimplemented "erlang__match_spec_test__3"
erlang__match_spec_test__3 [_,_,_] = EXC.badarg unit
erlang__match_spec_test__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__match_spec_test__3-}) args

erlang__pre_loaded__0 :: ErlangFun
erlang__pre_loaded__0 args = unimplemented "erlang__pre_loaded__0"

erlang__display_string__1 :: ErlangFun
erlang__display_string__1 args = unimplemented "erlang__display_string__1"
erlang__display_string__1 [_] = EXC.badarg unit
erlang__display_string__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__display_string__1-}) args

erlang__finish_loading__1 :: ErlangFun
erlang__finish_loading__1 args = unimplemented "erlang__finish_loading__1"
erlang__finish_loading__1 [_] = EXC.badarg unit
erlang__finish_loading__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__finish_loading__1-}) args

erlang__spawn_link__3 :: ErlangFun
erlang__spawn_link__3 args = unimplemented "erlang__spawn_link__3"
erlang__spawn_link__3 [_,_,_] = EXC.badarg unit
erlang__spawn_link__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__spawn_link__3-}) args

erlang__garbage_collect__2 :: ErlangFun
erlang__garbage_collect__2 args = unimplemented "erlang__garbage_collect__2"
erlang__garbage_collect__2 [_,_] = EXC.badarg unit
erlang__garbage_collect__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__garbage_collect__2-}) args

erlang__system_flag__2 :: ErlangFun
erlang__system_flag__2 args = unimplemented "erlang__system_flag__2"
erlang__system_flag__2 [_,_] = EXC.badarg unit
erlang__system_flag__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__system_flag__2-}) args

erlang__universaltime_to_localtime__1 :: ErlangFun
erlang__universaltime_to_localtime__1 args = unimplemented "erlang__universaltime_to_localtime__1"
erlang__universaltime_to_localtime__1 [_] = EXC.badarg unit
erlang__universaltime_to_localtime__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__universaltime_to_localtime__1-}) args

erlang__whereis__1 :: ErlangFun
erlang__whereis__1 [ErlangAtom "user"] = erlang__group_leader__0 []
erlang__whereis__1 args = unimplemented "erlang__whereis__1"
erlang__whereis__1 [_] = EXC.badarg unit
erlang__whereis__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__whereis__1-}) args

erlang__port_call__3 :: ErlangFun
erlang__port_call__3 args = unimplemented "erlang__port_call__3"
erlang__port_call__3 [_,_,_] = EXC.badarg unit
erlang__port_call__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__port_call__3-}) args

erlang__date__0 :: ErlangFun
erlang__date__0 args = unimplemented "erlang__date__0"

foreign import do_make_ref_0 :: (Int -> ErlangTerm) -> ErlangTerm
erlang__make_ref__0 :: ErlangFun
erlang__make_ref__0 _ = do_make_ref_0 ErlangReference

erlang__port_control__3 :: ErlangFun
erlang__port_control__3 args = unimplemented "erlang__port_control__3"
erlang__port_control__3 [_,_,_] = EXC.badarg unit
erlang__port_control__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__port_control__3-}) args

erlang__byte_size__1 :: ErlangFun
erlang__byte_size__1 args = unimplemented "erlang__byte_size__1"
erlang__byte_size__1 [_] = EXC.badarg unit
erlang__byte_size__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__byte_size__1-}) args

erlang__check_process_code__2 :: ErlangFun
erlang__check_process_code__2 args = unimplemented "erlang__check_process_code__2"
erlang__check_process_code__2 [_,_] = EXC.badarg unit
erlang__check_process_code__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__check_process_code__2-}) args

erlang__system_monitor__0 :: ErlangFun
erlang__system_monitor__0 args = unimplemented "erlang__system_monitor__0"

erlang__phash2__2 :: ErlangFun
erlang__phash2__2 args = unimplemented "erlang__phash2__2"
erlang__phash2__2 [_,_] = EXC.badarg unit
erlang__phash2__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__phash2__2-}) args

erlang__bitsize__1 :: ErlangFun
erlang__bitsize__1 args = unimplemented "erlang__bitsize__1"
erlang__bitsize__1 [_] = EXC.badarg unit
erlang__bitsize__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__bitsize__1-}) args

erlang__nodes__1 :: ErlangFun
erlang__nodes__1 args = unimplemented "erlang__nodes__1"
erlang__nodes__1 [_] = EXC.badarg unit
erlang__nodes__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__nodes__1-}) args

erlang__time__0 :: ErlangFun
erlang__time__0 args = unimplemented "erlang__time__0"

erlang__time_offset__1 :: ErlangFun
erlang__time_offset__1 args = unimplemented "erlang__time_offset__1"
erlang__time_offset__1 [_] = EXC.badarg unit
erlang__time_offset__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__time_offset__1-}) args

erlang__seq_trace_print__1 :: ErlangFun
erlang__seq_trace_print__1 args = unimplemented "erlang__seq_trace_print__1"
erlang__seq_trace_print__1 [_] = EXC.badarg unit
erlang__seq_trace_print__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__seq_trace_print__1-}) args

erlang__send_nosuspend__3 :: ErlangFun
erlang__send_nosuspend__3 [arg1, arg2, _] = erlang__send_nosuspend__2 [arg1, arg2]
erlang__send_nosuspend__3 _ = EXC.badarg unit

erlang__send_nosuspend__2 :: ErlangFun
erlang__send_nosuspend__2 args =
    let
        a = erlang__send__2 args
    in
        boolToTerm true

erlang__send__3 :: ErlangFun
erlang__send__3 [arg1, arg2, _] = erlang__send__2 [arg1, arg2]
erlang__send__3 _ = EXC.badarg unit

foreign import do_send_2 :: Int -> ErlangTerm -> ErlangTerm
erlang__send__2 :: ErlangFun
erlang__send__2 [ErlangPID pid, term] = do_send_2 pid term
erlang__send__2 _ = EXC.badarg unit

foreign import do_receive_2 :: ErlangFun -> Int -> (String -> ErlangTerm) -> ErlangTerm
prim_eval__receive__2 :: ErlangFun
prim_eval__receive__2 [ErlangFun 1 fun, ErlangAtom "infinity"] = do_receive_2 fun (-1) (ErlangAtom)
prim_eval__receive__2 [ErlangFun 1 fun, ErlangInt timeout] | timeout >= 0 =
        do_receive_2 fun timeout (ErlangAtom)
prim_eval__receive__2 _ = EXC.badarg unit

erlang__halt__1 :: ErlangFun
erlang__halt__1 args = unimplemented "erlang__halt__1"
erlang__halt__1 [_] = EXC.badarg unit
erlang__halt__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__halt__1-}) args

erlang__spawn_opt__5 :: ErlangFun
erlang__spawn_opt__5 args = unimplemented "erlang__spawn_opt__5"
erlang__spawn_opt__5 [_,_,_,_,_] = EXC.badarg unit
erlang__spawn_opt__5 args = EXC.badarity (ErlangFun 5 purs_tco_sucks {-erlang__spawn_opt__5-}) args

erlang__size__1 :: ErlangFun
erlang__size__1 args = unimplemented "erlang__size__1"
erlang__size__1 [_] = EXC.badarg unit
erlang__size__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__size__1-}) args

erlang__process_info__1 :: ErlangFun
erlang__process_info__1 args = unimplemented "erlang__process_info__1"
erlang__process_info__1 [_] = EXC.badarg unit
erlang__process_info__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__process_info__1-}) args

erlang__md5__1 :: ErlangFun
erlang__md5__1 args = unimplemented "erlang__md5__1"
erlang__md5__1 [_] = EXC.badarg unit
erlang__md5__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__md5__1-}) args

erlang__binary_part__2 :: ErlangFun
erlang__binary_part__2 args = unimplemented "erlang__binary_part__2"
erlang__binary_part__2 [_,_] = EXC.badarg unit
erlang__binary_part__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__binary_part__2-}) args

erlang__format_cpu_topology__1 :: ErlangFun
erlang__format_cpu_topology__1 args = unimplemented "erlang__format_cpu_topology__1"
erlang__format_cpu_topology__1 [_] = EXC.badarg unit
erlang__format_cpu_topology__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__format_cpu_topology__1-}) args

foreign import do_spawn_1 :: (Unit -> ErlangTerm) -> (Int -> ErlangTerm) -> ErlangTerm
erlang__spawn__1 :: ErlangFun
erlang__spawn__1 [f] =
    do_spawn_1 (\x -> erlang__apply__2 [f, ErlangEmptyList]) ErlangPID
erlang__spawn__1 [_] = EXC.badarg unit
erlang__spawn__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__spawn__1-}) args

erlang__throw__1 :: ErlangFun
erlang__throw__1 [arg] = EXC.throw arg
erlang__throw__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__throw__1-}) args

erlang__load_nif__2 :: ErlangFun
erlang__load_nif__2 args = unimplemented "erlang__load_nif__2"
erlang__load_nif__2 [_,_] = EXC.badarg unit
erlang__load_nif__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__load_nif__2-}) args

erlang__prepare_loading__2 :: ErlangFun
erlang__prepare_loading__2 args = unimplemented "erlang__prepare_loading__2"
erlang__prepare_loading__2 [_,_] = EXC.badarg unit
erlang__prepare_loading__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__prepare_loading__2-}) args

erlang__open_port__2 :: ErlangFun
erlang__open_port__2 args = unimplemented "erlang__open_port__2"
erlang__open_port__2 [_,_] = EXC.badarg unit
erlang__open_port__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__open_port__2-}) args

erlang__port_set_data__2 :: ErlangFun
erlang__port_set_data__2 args = unimplemented "erlang__port_set_data__2"
erlang__port_set_data__2 [_,_] = EXC.badarg unit
erlang__port_set_data__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__port_set_data__2-}) args

foreign import do_self_0 :: (Int -> ErlangTerm) -> ErlangTerm
erlang__self__0 :: ErlangFun
erlang__self__0 _ = do_self_0 ErlangPID

erlang__read_timer__2 :: ErlangFun
erlang__read_timer__2 args = unimplemented "erlang__read_timer__2"
erlang__read_timer__2 [_,_] = EXC.badarg unit
erlang__read_timer__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__read_timer__2-}) args

erlang__statistics__1 :: ErlangFun
erlang__statistics__1 args = unimplemented "erlang__statistics__1"
erlang__statistics__1 [_] = EXC.badarg unit
erlang__statistics__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__statistics__1-}) args

erlang__nodes__0 :: ErlangFun
erlang__nodes__0 args = unimplemented "erlang__nodes__0"

erlang__insert_element__3 :: ErlangFun
erlang__insert_element__3 args = unimplemented "erlang__insert_element__3"
erlang__insert_element__3 [_,_,_] = EXC.badarg unit
erlang__insert_element__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__insert_element__3-}) args

erlang__spawn__3 :: ErlangFun
erlang__spawn__3 args = unimplemented "erlang__spawn__3"
erlang__spawn__3 [_,_,_] = EXC.badarg unit
erlang__spawn__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__spawn__3-}) args

erlang__send_after__4 :: ErlangFun
erlang__send_after__4 args = unimplemented "erlang__send_after__4"
erlang__send_after__4 [_,_,_,_] = EXC.badarg unit
erlang__send_after__4 args = EXC.badarity (ErlangFun 4 purs_tco_sucks {-erlang__send_after__4-}) args

erlang__trace__3 :: ErlangFun
erlang__trace__3 args = unimplemented "erlang__trace__3"
erlang__trace__3 [_,_,_] = EXC.badarg unit
erlang__trace__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__trace__3-}) args

erlang__adler32__1 :: ErlangFun
erlang__adler32__1 args = unimplemented "erlang__adler32__1"
erlang__adler32__1 [_] = EXC.badarg unit
erlang__adler32__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__adler32__1-}) args

erlang__dt_get_tag_data__0 :: ErlangFun
erlang__dt_get_tag_data__0 args = unimplemented "erlang__dt_get_tag_data__0"

erlang__resume_process__1 :: ErlangFun
erlang__resume_process__1 args = unimplemented "erlang__resume_process__1"
erlang__resume_process__1 [_] = EXC.badarg unit
erlang__resume_process__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__resume_process__1-}) args

erlang__port_close__1 :: ErlangFun
erlang__port_close__1 args = unimplemented "erlang__port_close__1"
erlang__port_close__1 [_] = EXC.badarg unit
erlang__port_close__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__port_close__1-}) args

erlang__port_call__2 :: ErlangFun
erlang__port_call__2 args = unimplemented "erlang__port_call__2"
erlang__port_call__2 [_,_] = EXC.badarg unit
erlang__port_call__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__port_call__2-}) args

erlang__dt_get_tag__0 :: ErlangFun
erlang__dt_get_tag__0 args = unimplemented "erlang__dt_get_tag__0"

erlang__universaltime_to_posixtime__1 :: ErlangFun
erlang__universaltime_to_posixtime__1 args = unimplemented "erlang__universaltime_to_posixtime__1"
erlang__universaltime_to_posixtime__1 [_] = EXC.badarg unit
erlang__universaltime_to_posixtime__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__universaltime_to_posixtime__1-}) args

erlang__md5_init__0 :: ErlangFun
erlang__md5_init__0 args = unimplemented "erlang__md5_init__0"

erlang__link__1 :: ErlangFun
erlang__link__1 args = unimplemented "erlang__link__1"
erlang__link__1 [_] = EXC.badarg unit
erlang__link__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__link__1-}) args

erlang__spawn_opt__1 :: ErlangFun
erlang__spawn_opt__1 args = unimplemented "erlang__spawn_opt__1"
erlang__spawn_opt__1 [_] = EXC.badarg unit
erlang__spawn_opt__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__spawn_opt__1-}) args

erlang__monitor_node__2 :: ErlangFun
erlang__monitor_node__2 args = unimplemented "erlang__monitor_node__2"
erlang__monitor_node__2 [_,_] = EXC.badarg unit
erlang__monitor_node__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__monitor_node__2-}) args

erlang__time_offset__0 :: ErlangFun
erlang__time_offset__0 args = unimplemented "erlang__time_offset__0"

erlang__port_connect__2 :: ErlangFun
erlang__port_connect__2 args = unimplemented "erlang__port_connect__2"
erlang__port_connect__2 [_,_] = EXC.badarg unit
erlang__port_connect__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__port_connect__2-}) args

erlang__setelement__3 :: ErlangFun
erlang__setelement__3 [ErlangInt pos, ErlangTuple tuple, new_el]
    | DM.Just new_tuple <- DA.updateAt (pos-1) new_el tuple =
    ErlangTuple new_tuple
erlang__setelement__3 [_,_,_] = EXC.badarg unit
erlang__setelement__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__setelement__3-}) args

erlang__gather_gc_info_result__1 :: ErlangFun
erlang__gather_gc_info_result__1 args = unimplemented "erlang__gather_gc_info_result__1"
erlang__gather_gc_info_result__1 [_] = EXC.badarg unit
erlang__gather_gc_info_result__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__gather_gc_info_result__1-}) args

erlang__tuple_size__1 :: ErlangFun
erlang__tuple_size__1 args = unimplemented "erlang__tuple_size__1"
erlang__tuple_size__1 [_] = EXC.badarg unit
erlang__tuple_size__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__tuple_size__1-}) args

erlang__system_monitor__1 :: ErlangFun
erlang__system_monitor__1 args = unimplemented "erlang__system_monitor__1"
erlang__system_monitor__1 [_] = EXC.badarg unit
erlang__system_monitor__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__system_monitor__1-}) args

erlang__timestamp__0 :: ErlangFun
erlang__timestamp__0 args = unimplemented "erlang__timestamp__0"

erlang__system_time__1 :: ErlangFun
erlang__system_time__1 args = unimplemented "erlang__system_time__1"
erlang__system_time__1 [_] = EXC.badarg unit
erlang__system_time__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__system_time__1-}) args

erlang__register__2 :: ErlangFun
erlang__register__2 args = unimplemented "erlang__register__2"
erlang__register__2 [_,_] = EXC.badarg unit
erlang__register__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__register__2-}) args

erlang__dmonitor_node__3 :: ErlangFun
erlang__dmonitor_node__3 args = unimplemented "erlang__dmonitor_node__3"
erlang__dmonitor_node__3 [_,_,_] = EXC.badarg unit
erlang__dmonitor_node__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__dmonitor_node__3-}) args

erlang__fun_info__2 :: ErlangFun
erlang__fun_info__2 args = unimplemented "erlang__fun_info__2"
erlang__fun_info__2 [_,_] = EXC.badarg unit
erlang__fun_info__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__fun_info__2-}) args

erlang__finish_after_on_load__2 :: ErlangFun
erlang__finish_after_on_load__2 args = unimplemented "erlang__finish_after_on_load__2"
erlang__finish_after_on_load__2 [_,_] = EXC.badarg unit
erlang__finish_after_on_load__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__finish_after_on_load__2-}) args

erlang__send_after__3 :: ErlangFun
erlang__send_after__3 args = unimplemented "erlang__send_after__3"
erlang__send_after__3 [_,_,_] = EXC.badarg unit
erlang__send_after__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__send_after__3-}) args

erlang__trace_pattern__2 :: ErlangFun
erlang__trace_pattern__2 args = unimplemented "erlang__trace_pattern__2"
erlang__trace_pattern__2 [_,_] = EXC.badarg unit
erlang__trace_pattern__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__trace_pattern__2-}) args

erlang__port_command__3 :: ErlangFun
erlang__port_command__3 args = unimplemented "erlang__port_command__3"
erlang__port_command__3 [_,_,_] = EXC.badarg unit
erlang__port_command__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__port_command__3-}) args

erlang__alloc_info__1 :: ErlangFun
erlang__alloc_info__1 args = unimplemented "erlang__alloc_info__1"
erlang__alloc_info__1 [_] = EXC.badarg unit
erlang__alloc_info__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__alloc_info__1-}) args

erlang__port_command__2 :: ErlangFun
erlang__port_command__2 args = unimplemented "erlang__port_command__2"
erlang__port_command__2 [_,_] = EXC.badarg unit
erlang__port_command__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__port_command__2-}) args

erlang__external_size__1 :: ErlangFun
erlang__external_size__1 args = unimplemented "erlang__external_size__1"
erlang__external_size__1 [_] = EXC.badarg unit
erlang__external_size__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__external_size__1-}) args

erlang__spawn_opt__3 :: ErlangFun
erlang__spawn_opt__3 args = unimplemented "erlang__spawn_opt__3"
erlang__spawn_opt__3 [_,_,_] = EXC.badarg unit
erlang__spawn_opt__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__spawn_opt__3-}) args

erlang__exit_signal__2 :: ErlangFun
erlang__exit_signal__2 args = unimplemented "erlang__exit_signal__2"
erlang__exit_signal__2 [_,_] = EXC.badarg unit
erlang__exit_signal__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__exit_signal__2-}) args

erlang__display_nl__0 :: ErlangFun
erlang__display_nl__0 args = unimplemented "erlang__display_nl__0"

erlang__append_element__2 :: ErlangFun
erlang__append_element__2 args = unimplemented "erlang__append_element__2"
erlang__append_element__2 [_,_] = EXC.badarg unit
erlang__append_element__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__append_element__2-}) args

erlang__delete_element__2 :: ErlangFun
erlang__delete_element__2 args = unimplemented "erlang__delete_element__2"
erlang__delete_element__2 [_,_] = EXC.badarg unit
erlang__delete_element__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__delete_element__2-}) args

erlang__not__1 :: ErlangFun
erlang__not__1 args = unimplemented "erlang__not__1"
erlang__not__1 [_] = EXC.badarg unit
erlang__not__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__not__1-}) args

erlang__round__1 :: ErlangFun
erlang__round__1 args = unimplemented "erlang__round__1"
erlang__round__1 [_] = EXC.badarg unit
erlang__round__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__round__1-}) args

erlang__crc32__2 :: ErlangFun
erlang__crc32__2 args = unimplemented "erlang__crc32__2"
erlang__crc32__2 [_,_] = EXC.badarg unit
erlang__crc32__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__crc32__2-}) args

erlang__adler32__2 :: ErlangFun
erlang__adler32__2 args = unimplemented "erlang__adler32__2"
erlang__adler32__2 [_,_] = EXC.badarg unit
erlang__adler32__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__adler32__2-}) args

erlang__md5_final__1 :: ErlangFun
erlang__md5_final__1 args = unimplemented "erlang__md5_final__1"
erlang__md5_final__1 [_] = EXC.badarg unit
erlang__md5_final__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__md5_final__1-}) args

erlang__monitor_node__3 :: ErlangFun
erlang__monitor_node__3 args = unimplemented "erlang__monitor_node__3"
erlang__monitor_node__3 [_,_,_] = EXC.badarg unit
erlang__monitor_node__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__monitor_node__3-}) args

erlang__monotonic_time__0 :: ErlangFun
erlang__monotonic_time__0 args = unimplemented "erlang__monotonic_time__0"

erlang__nif_error__1 :: ErlangFun
erlang__nif_error__1 args = erlang__error__1 args
erlang__nif_error__1 [_] = EXC.badarg unit
erlang__nif_error__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__nif_error__1-}) args

erlang__check_process_code__3 :: ErlangFun
erlang__check_process_code__3 args = unimplemented "erlang__check_process_code__3"
erlang__check_process_code__3 [_,_,_] = EXC.badarg unit
erlang__check_process_code__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__check_process_code__3-}) args

erlang__localtime__0 :: ErlangFun
erlang__localtime__0 args = unimplemented "erlang__localtime__0"

erlang__trace_delivered__1 :: ErlangFun
erlang__trace_delivered__1 args = unimplemented "erlang__trace_delivered__1"
erlang__trace_delivered__1 [_] = EXC.badarg unit
erlang__trace_delivered__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__trace_delivered__1-}) args

erlang__module_info__0 :: ErlangFun
erlang__module_info__0 args = unimplemented "erlang__module_info__0"

erlang__spawn__2 :: ErlangFun
erlang__spawn__2 args = unimplemented "erlang__spawn__2"
erlang__spawn__2 [_,_] = EXC.badarg unit
erlang__spawn__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__spawn__2-}) args

erlang__set_cookie__2 :: ErlangFun
erlang__set_cookie__2 args = unimplemented "erlang__set_cookie__2"
erlang__set_cookie__2 [_,_] = EXC.badarg unit
erlang__set_cookie__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__set_cookie__2-}) args

erlang__seq_trace_print__2 :: ErlangFun
erlang__seq_trace_print__2 args = unimplemented "erlang__seq_trace_print__2"
erlang__seq_trace_print__2 [_,_] = EXC.badarg unit
erlang__seq_trace_print__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__seq_trace_print__2-}) args

erlang__suspend_process__2 :: ErlangFun
erlang__suspend_process__2 args = unimplemented "erlang__suspend_process__2"
erlang__suspend_process__2 [_,_] = EXC.badarg unit
erlang__suspend_process__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__suspend_process__2-}) args

erlang__crc32_combine__3 :: ErlangFun
erlang__crc32_combine__3 args = unimplemented "erlang__crc32_combine__3"
erlang__crc32_combine__3 [_,_,_] = EXC.badarg unit
erlang__crc32_combine__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__crc32_combine__3-}) args

erlang__process_info__2 :: ErlangFun
erlang__process_info__2 args = unimplemented "erlang__process_info__2"
erlang__process_info__2 [_,_] = EXC.badarg unit
erlang__process_info__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__process_info__2-}) args

erlang__unique_integer__0 :: ErlangFun
erlang__unique_integer__0 args = unimplemented "erlang__unique_integer__0"

erlang__system_time__0 :: ErlangFun
erlang__system_time__0 args = unimplemented "erlang__system_time__0"

erlang__yield__0 :: ErlangFun
erlang__yield__0 args = unimplemented "erlang__yield__0"

erlang__posixtime_to_universaltime__1 :: ErlangFun
erlang__posixtime_to_universaltime__1 args = unimplemented "erlang__posixtime_to_universaltime__1"
erlang__posixtime_to_universaltime__1 [_] = EXC.badarg unit
erlang__posixtime_to_universaltime__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__posixtime_to_universaltime__1-}) args

erlang__start_timer__3 :: ErlangFun
erlang__start_timer__3 args = unimplemented "erlang__start_timer__3"
erlang__start_timer__3 [_,_,_] = EXC.badarg unit
erlang__start_timer__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__start_timer__3-}) args

erlang__erase__1 :: ErlangFun
erlang__erase__1 args = unimplemented "erlang__erase__1"
erlang__erase__1 [_] = EXC.badarg unit
erlang__erase__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__erase__1-}) args

erlang__port_info__1 :: ErlangFun
erlang__port_info__1 args = unimplemented "erlang__port_info__1"
erlang__port_info__1 [_] = EXC.badarg unit
erlang__port_info__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__port_info__1-}) args

erlang__process_flag__2 :: ErlangFun
erlang__process_flag__2 args = unimplemented "erlang__process_flag__2"
erlang__process_flag__2 [_,_] = EXC.badarg unit
erlang__process_flag__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__process_flag__2-}) args

erlang__monotonic_time__1 :: ErlangFun
erlang__monotonic_time__1 args = unimplemented "erlang__monotonic_time__1"
erlang__monotonic_time__1 [_] = EXC.badarg unit
erlang__monotonic_time__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__monotonic_time__1-}) args

erlang__seq_trace_info__1 :: ErlangFun
erlang__seq_trace_info__1 args = unimplemented "erlang__seq_trace_info__1"
erlang__seq_trace_info__1 [_] = EXC.badarg unit
erlang__seq_trace_info__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__seq_trace_info__1-}) args

erlang__memory__1 :: ErlangFun
erlang__memory__1 args = unimplemented "erlang__memory__1"
erlang__memory__1 [_] = EXC.badarg unit
erlang__memory__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__memory__1-}) args

erlang__get_module_info__2 :: ErlangFun
erlang__get_module_info__2 args = unimplemented "erlang__get_module_info__2"
erlang__get_module_info__2 [_,_] = EXC.badarg unit
erlang__get_module_info__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__get_module_info__2-}) args

erlang__seq_trace__2 :: ErlangFun
erlang__seq_trace__2 args = unimplemented "erlang__seq_trace__2"
erlang__seq_trace__2 [_,_] = EXC.badarg unit
erlang__seq_trace__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__seq_trace__2-}) args

erlang__disconnect_node__1 :: ErlangFun
erlang__disconnect_node__1 args = unimplemented "erlang__disconnect_node__1"
erlang__disconnect_node__1 [_] = EXC.badarg unit
erlang__disconnect_node__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__disconnect_node__1-}) args

erlang__setnode__2 :: ErlangFun
erlang__setnode__2 args = unimplemented "erlang__setnode__2"
erlang__setnode__2 [_,_] = EXC.badarg unit
erlang__setnode__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__setnode__2-}) args

erlang__get_keys__0 :: ErlangFun
erlang__get_keys__0 args = unimplemented "erlang__get_keys__0"

erlang__node__0 :: ErlangFun
erlang__node__0 args = unimplemented "erlang__node__0"

erlang__fun_info__1 :: ErlangFun
erlang__fun_info__1 args = unimplemented "erlang__fun_info__1"
erlang__fun_info__1 [_] = EXC.badarg unit
erlang__fun_info__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__fun_info__1-}) args

erlang__exit__1 :: ErlangFun
erlang__exit__1 [arg] = EXC.exit arg
erlang__exit__1 [_] = EXC.badarg unit
erlang__exit__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__exit__1-}) args

erlang__system_info__1 :: ErlangFun
erlang__system_info__1 args = unimplemented "erlang__system_info__1"
erlang__system_info__1 [_] = EXC.badarg unit
erlang__system_info__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__system_info__1-}) args

erlang__binary_part__3 :: ErlangFun
erlang__binary_part__3 args = unimplemented "erlang__binary_part__3"
erlang__binary_part__3 [_,_,_] = EXC.badarg unit
erlang__binary_part__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__binary_part__3-}) args

erlang__halt__2 :: ErlangFun
erlang__halt__2 args = unimplemented "erlang__halt__2"
erlang__halt__2 [_,_] = EXC.badarg unit
erlang__halt__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__halt__2-}) args

erlang__localtime_to_universaltime__2 :: ErlangFun
erlang__localtime_to_universaltime__2 args = unimplemented "erlang__localtime_to_universaltime__2"
erlang__localtime_to_universaltime__2 [_,_] = EXC.badarg unit
erlang__localtime_to_universaltime__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__localtime_to_universaltime__2-}) args

erlang__cancel_timer__2 :: ErlangFun
erlang__cancel_timer__2 args = unimplemented "erlang__cancel_timer__2"
erlang__cancel_timer__2 [_,_] = EXC.badarg unit
erlang__cancel_timer__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__cancel_timer__2-}) args

erlang__display__1 :: ErlangFun
erlang__display__1 args = unimplemented "erlang__display__1"
erlang__display__1 [_] = EXC.badarg unit
erlang__display__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__display__1-}) args

erlang__load_module__2 :: ErlangFun
erlang__load_module__2 args = unimplemented "erlang__load_module__2"
erlang__load_module__2 [_,_] = EXC.badarg unit
erlang__load_module__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__load_module__2-}) args

erlang__dt_append_vm_tag_data__1 :: ErlangFun
erlang__dt_append_vm_tag_data__1 args = unimplemented "erlang__dt_append_vm_tag_data__1"
erlang__dt_append_vm_tag_data__1 [_] = EXC.badarg unit
erlang__dt_append_vm_tag_data__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__dt_append_vm_tag_data__1-}) args

erlang__port_info__2 :: ErlangFun
erlang__port_info__2 args = unimplemented "erlang__port_info__2"
erlang__port_info__2 [_,_] = EXC.badarg unit
erlang__port_info__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__port_info__2-}) args

erlang__get__0 :: ErlangFun
erlang__get__0 args = unimplemented "erlang__get__0"

erlang__unregister__1 :: ErlangFun
erlang__unregister__1 args = unimplemented "erlang__unregister__1"
erlang__unregister__1 [_] = EXC.badarg unit
erlang__unregister__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__unregister__1-}) args

erlang__dist_ctrl_input_handler__2 :: ErlangFun
erlang__dist_ctrl_input_handler__2 args = unimplemented "erlang__dist_ctrl_input_handler__2"
erlang__dist_ctrl_input_handler__2 [_,_] = EXC.badarg unit
erlang__dist_ctrl_input_handler__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__dist_ctrl_input_handler__2-}) args

erlang__trunc__1 :: ErlangFun
erlang__trunc__1 args = unimplemented "erlang__trunc__1"
erlang__trunc__1 [_] = EXC.badarg unit
erlang__trunc__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__trunc__1-}) args

erlang__crasher__6 :: ErlangFun
erlang__crasher__6 args = unimplemented "erlang__crasher__6"
erlang__crasher__6 [_,_,_,_,_,_] = EXC.badarg unit
erlang__crasher__6 args = EXC.badarity (ErlangFun 6 purs_tco_sucks {-erlang__crasher__6-}) args

erlang__read_timer__1 :: ErlangFun
erlang__read_timer__1 args = unimplemented "erlang__read_timer__1"
erlang__read_timer__1 [_] = EXC.badarg unit
erlang__read_timer__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__read_timer__1-}) args

erlang__module_loaded__1 :: ErlangFun
erlang__module_loaded__1 args = unimplemented "erlang__module_loaded__1"
erlang__module_loaded__1 [_] = EXC.badarg unit
erlang__module_loaded__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__module_loaded__1-}) args

erlang__md5_update__2 :: ErlangFun
erlang__md5_update__2 args = unimplemented "erlang__md5_update__2"
erlang__md5_update__2 [_,_] = EXC.badarg unit
erlang__md5_update__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__md5_update__2-}) args

erlang__localtime_to_universaltime__1 :: ErlangFun
erlang__localtime_to_universaltime__1 args = unimplemented "erlang__localtime_to_universaltime__1"
erlang__localtime_to_universaltime__1 [_] = EXC.badarg unit
erlang__localtime_to_universaltime__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__localtime_to_universaltime__1-}) args

erlang__spawn_opt__4 :: ErlangFun
erlang__spawn_opt__4 args = unimplemented "erlang__spawn_opt__4"
erlang__spawn_opt__4 [_,_,_,_] = EXC.badarg unit
erlang__spawn_opt__4 args = EXC.badarity (ErlangFun 4 purs_tco_sucks {-erlang__spawn_opt__4-}) args

erlang__group_leader__0 :: ErlangFun
erlang__group_leader__0 args = erlang__self__0 [] -- FIXME

erlang__dt_put_tag__1 :: ErlangFun
erlang__dt_put_tag__1 args = unimplemented "erlang__dt_put_tag__1"
erlang__dt_put_tag__1 [_] = EXC.badarg unit
erlang__dt_put_tag__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__dt_put_tag__1-}) args

erlang__phash2__1 :: ErlangFun
erlang__phash2__1 args = unimplemented "erlang__phash2__1"
erlang__phash2__1 [_] = EXC.badarg unit
erlang__phash2__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__phash2__1-}) args

erlang__spawn__4 :: ErlangFun
erlang__spawn__4 args = unimplemented "erlang__spawn__4"
erlang__spawn__4 [_,_,_,_] = EXC.badarg unit
erlang__spawn__4 args = EXC.badarity (ErlangFun 4 purs_tco_sucks {-erlang__spawn__4-}) args

erlang__crc32__1 :: ErlangFun
erlang__crc32__1 args = unimplemented "erlang__crc32__1"
erlang__crc32__1 [_] = EXC.badarg unit
erlang__crc32__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__crc32__1-}) args

erlang__system_monitor__2 :: ErlangFun
erlang__system_monitor__2 args = unimplemented "erlang__system_monitor__2"
erlang__system_monitor__2 [_,_] = EXC.badarg unit
erlang__system_monitor__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__system_monitor__2-}) args

erlang__spawn_monitor__3 :: ErlangFun
erlang__spawn_monitor__3 args = unimplemented "erlang__spawn_monitor__3"
erlang__spawn_monitor__3 [_,_,_] = EXC.badarg unit
erlang__spawn_monitor__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__spawn_monitor__3-}) args

erlang__get_keys__1 :: ErlangFun
erlang__get_keys__1 args = unimplemented "erlang__get_keys__1"
erlang__get_keys__1 [_] = EXC.badarg unit
erlang__get_keys__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__get_keys__1-}) args

erlang__demonitor__2 :: ErlangFun
erlang__demonitor__2 args = unimplemented "erlang__demonitor__2"
erlang__demonitor__2 [_,_] = EXC.badarg unit
erlang__demonitor__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__demonitor__2-}) args

erlang__raise__3 :: ErlangFun
erlang__raise__3 args = unimplemented "erlang__raise__3"
erlang__raise__3 [_,_,_] = EXC.badarg unit
erlang__raise__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__raise__3-}) args

erlang__hibernate__3 :: ErlangFun
erlang__hibernate__3 args = unimplemented "erlang__hibernate__3"
erlang__hibernate__3 [_,_,_] = EXC.badarg unit
erlang__hibernate__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__hibernate__3-}) args

erlang__adler32_combine__3 :: ErlangFun
erlang__adler32_combine__3 args = unimplemented "erlang__adler32_combine__3"
erlang__adler32_combine__3 [_,_,_] = EXC.badarg unit
erlang__adler32_combine__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__adler32_combine__3-}) args

erlang__node__1 :: ErlangFun
erlang__node__1 args = unimplemented "erlang__node__1"
erlang__node__1 [_] = EXC.badarg unit
erlang__node__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__node__1-}) args

erlang__monitor__2 :: ErlangFun
erlang__monitor__2 args = unimplemented "erlang__monitor__2"
erlang__monitor__2 [_,_] = EXC.badarg unit
erlang__monitor__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__monitor__2-}) args

erlang__start_timer__4 :: ErlangFun
erlang__start_timer__4 args = unimplemented "erlang__start_timer__4"
erlang__start_timer__4 [_,_,_,_] = EXC.badarg unit
erlang__start_timer__4 args = EXC.badarity (ErlangFun 4 purs_tco_sucks {-erlang__start_timer__4-}) args

erlang__bit_size__1 :: ErlangFun
erlang__bit_size__1 args = unimplemented "erlang__bit_size__1"
erlang__bit_size__1 [_] = EXC.badarg unit
erlang__bit_size__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__bit_size__1-}) args

erlang__call_on_load_function__1 :: ErlangFun
erlang__call_on_load_function__1 args = unimplemented "erlang__call_on_load_function__1"
erlang__call_on_load_function__1 [_] = EXC.badarg unit
erlang__call_on_load_function__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__call_on_load_function__1-}) args

erlang__processes__0 :: ErlangFun
erlang__processes__0 args = unimplemented "erlang__processes__0"

erlang__error__2 :: ErlangFun
erlang__error__2 [err, _] = erlang__error__1 [err]
erlang__error__2 [_,_] = EXC.badarg unit
erlang__error__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__error__2-}) args

erlang__loaded__0 :: ErlangFun
erlang__loaded__0 args = unimplemented "erlang__loaded__0"

erlang__bump_reductions__1 :: ErlangFun
erlang__bump_reductions__1 args = unimplemented "erlang__bump_reductions__1"
erlang__bump_reductions__1 [_] = EXC.badarg unit
erlang__bump_reductions__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__bump_reductions__1-}) args

erlang__make_tuple__3 :: ErlangFun
erlang__make_tuple__3 args = unimplemented "erlang__make_tuple__3"
erlang__make_tuple__3 [_,_,_] = EXC.badarg unit
erlang__make_tuple__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__make_tuple__3-}) args

erlang__unlink__1 :: ErlangFun
erlang__unlink__1 args = unimplemented "erlang__unlink__1"
erlang__unlink__1 [_] = EXC.badarg unit
erlang__unlink__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__unlink__1-}) args

erlang__demonitor__1 :: ErlangFun
erlang__demonitor__1 args = unimplemented "erlang__demonitor__1"
erlang__demonitor__1 [_] = EXC.badarg unit
erlang__demonitor__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__demonitor__1-}) args

erlang__trace_info__2 :: ErlangFun
erlang__trace_info__2 args = unimplemented "erlang__trace_info__2"
erlang__trace_info__2 [_,_] = EXC.badarg unit
erlang__trace_info__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__trace_info__2-}) args

erlang__delete_module__1 :: ErlangFun
erlang__delete_module__1 args = unimplemented "erlang__delete_module__1"
erlang__delete_module__1 [_] = EXC.badarg unit
erlang__delete_module__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__delete_module__1-}) args

erlang__garbage_collect__1 :: ErlangFun
erlang__garbage_collect__1 args = unimplemented "erlang__garbage_collect__1"
erlang__garbage_collect__1 [_] = EXC.badarg unit
erlang__garbage_collect__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__garbage_collect__1-}) args

erlang__check_old_code__1 :: ErlangFun
erlang__check_old_code__1 args = unimplemented "erlang__check_old_code__1"
erlang__check_old_code__1 [_] = EXC.badarg unit
erlang__check_old_code__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__check_old_code__1-}) args

erlang__spawn_monitor__1 :: ErlangFun
erlang__spawn_monitor__1 args = unimplemented "erlang__spawn_monitor__1"
erlang__spawn_monitor__1 [_] = EXC.badarg unit
erlang__spawn_monitor__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__spawn_monitor__1-}) args

erlang__system_profile__2 :: ErlangFun
erlang__system_profile__2 args = unimplemented "erlang__system_profile__2"
erlang__system_profile__2 [_,_] = EXC.badarg unit
erlang__system_profile__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__system_profile__2-}) args

erlang__error__1 :: ErlangFun
erlang__error__1 [arg] = EXC.error arg
erlang__error__1 [_] = EXC.badarg unit
erlang__error__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__error__1-}) args

erlang__delay_trap__2 :: ErlangFun
erlang__delay_trap__2 args = unimplemented "erlang__delay_trap__2"
erlang__delay_trap__2 [_,_] = EXC.badarg unit
erlang__delay_trap__2 args = EXC.badarity (ErlangFun 2 purs_tco_sucks {-erlang__delay_trap__2-}) args

erlang__spawn_link__4 :: ErlangFun
erlang__spawn_link__4 args = unimplemented "erlang__spawn_link__4"
erlang__spawn_link__4 [_,_,_,_] = EXC.badarg unit
erlang__spawn_link__4 args = EXC.badarity (ErlangFun 4 purs_tco_sucks {-erlang__spawn_link__4-}) args

erlang__memory__0 :: ErlangFun
erlang__memory__0 args = unimplemented "erlang__memory__0"

erlang__halt__0 :: ErlangFun
erlang__halt__0 args = unimplemented "erlang__halt__0"

erlang__process_flag__3 :: ErlangFun
erlang__process_flag__3 args = unimplemented "erlang__process_flag__3"
erlang__process_flag__3 [_,_,_] = EXC.badarg unit
erlang__process_flag__3 args = EXC.badarity (ErlangFun 3 purs_tco_sucks {-erlang__process_flag__3-}) args

erlang__set_cpu_topology__1 :: ErlangFun
erlang__set_cpu_topology__1 args = unimplemented "erlang__set_cpu_topology__1"
erlang__set_cpu_topology__1 [_] = EXC.badarg unit
erlang__set_cpu_topology__1 args = EXC.badarity (ErlangFun 1 purs_tco_sucks {-erlang__set_cpu_topology__1-}) args

erlang__universaltime__0 :: ErlangFun
erlang__universaltime__0 args = unimplemented "erlang__universaltime__0"
