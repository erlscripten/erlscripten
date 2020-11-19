module Erlang.Builtins where

import Erlang.Type
import Erlang.Exception as EXT
import Erlang.Helpers as H
import Prelude
import Data.Maybe as DM
import Data.Array as DA
import Data.List as DL
import Data.Int as DI
import Control.Monad
import Effect.Exception (throw)
import Effect
import Effect.Unsafe

unimplemented :: Unit -> ErlangTerm
unimplemented _ = unsafePerformEffect (throw "unimplemented BIF")

lists__keysearch__3 :: ErlangFun
lists__keysearch__3 [_, _, ErlangEmptyList] = boolToTerm false
lists__keysearch__3 [key, idx@(ErlangNum idxNum), ErlangCons el rest] = case el of
  ErlangTuple tup | DM.Just x <- DA.index tup idxNum  ->
    case erlang__op_exactEq [x, key] of
      ErlangAtom "true" -> (ErlangTuple [ErlangAtom "value", el])
      _                 -> lists__keysearch__3 [key, idx, rest]
  _ -> lists__keysearch__3 [key, idx, rest]

lists__keymember__3 :: ErlangFun
lists__keymember__3 [_, _, ErlangEmptyList] = boolToTerm false
lists__keymember__3 [key, idx@(ErlangNum idxNum), ErlangCons el rest] = case el of
  ErlangTuple tup | DM.Just x <- DA.index tup idxNum  ->
    case erlang__op_exactEq [x, key] of
      ErlangAtom "true" -> ErlangAtom "true"
      _                 -> lists__keymember__3 [key, idx, rest]
  _ -> lists__keymember__3 [key, idx, rest]

lists__reverse__2 :: ErlangFun
lists__reverse__2 [ErlangEmptyList, acc] = acc
lists__reverse__2 [ErlangCons h t, acc] = lists__reverse__2 [t, ErlangCons h acc]

lists__member__2 :: ErlangFun
lists__member__2 [_, ErlangEmptyList] = boolToTerm false
lists__member__2 [x, ErlangCons el rest] =
  case erlang__op_exactEq [x, el] of
    ErlangAtom "true" -> ErlangAtom "true"
    _                 -> lists__member__2 [x, rest]

lists__keyfind__3 :: ErlangFun
lists__keyfind__3 [_, _, ErlangEmptyList] = boolToTerm false
lists__keyfind__3 [key, idx@(ErlangNum idxNum), ErlangCons el rest] = case el of
  ErlangTuple tup | DM.Just x <- DA.index tup idxNum  ->
    case erlang__op_exactEq [x, key] of
      ErlangAtom "true" -> el
      _                 -> lists__keyfind__3 [key, idx, rest]
  _ -> lists__keyfind__3 [key, idx, rest]

--------------------------------------------------------------------------------

maps__get__2 :: ErlangFun
maps__get__2 [] = unimplemented unit

maps__find__2 :: ErlangFun
maps__find__2 [] = unimplemented unit

maps__from_list__1 :: ErlangFun
maps__from_list__1 [] = unimplemented unit

maps__is_key__2 :: ErlangFun
maps__is_key__2 [] = unimplemented unit

maps__keys__1 :: ErlangFun
maps__keys__1 [] = unimplemented unit

maps__merge__2 :: ErlangFun
maps__merge__2 [] = unimplemented unit

maps__put__3 :: ErlangFun
maps__put__3 [] = unimplemented unit

maps__remove__2 :: ErlangFun
maps__remove__2 [] = unimplemented unit

maps__take__2 :: ErlangFun
maps__take__2 [] = unimplemented unit

maps__to_list__1 :: ErlangFun
maps__to_list__1 [] = unimplemented unit

maps__update__3 :: ErlangFun
maps__update__3 [] = unimplemented unit

maps__values__1 :: ErlangFun
maps__values__1 [] = unimplemented unit


--------------------------------------------------------------------------------


-- =/=
erlang__op_exactNeq :: ErlangFun
erlang__op_exactNeq [a, b] = boolToTerm (a /= b)  -- FIXME (funs)

-- =:=
erlang__op_exactEq :: ErlangFun
erlang__op_exactEq [a, b] = boolToTerm (a == b) -- FIXME (funs)

-- /=
erlang__op_neq :: ErlangFun
erlang__op_neq [a, b] = boolToTerm (a /= b) -- FIXME (funs, floats)

-- ==
erlang__op_eq :: ErlangFun
erlang__op_eq [a, b] = boolToTerm (a == b) -- FIXME (funs, floats)

-- and
erlang__op_and :: ErlangFun
erlang__op_and [ErlangAtom "true",  ErlangAtom "true"]  = boolToTerm true
erlang__op_and [ErlangAtom "false", ErlangAtom "true"]  = boolToTerm false
erlang__op_and [ErlangAtom "true",  ErlangAtom "false"] = boolToTerm false
erlang__op_and [ErlangAtom "false", ErlangAtom "false"] = boolToTerm false

-- or
erlang__op_or :: ErlangFun
erlang__op_or [ErlangAtom "true",  ErlangAtom "true"]  = boolToTerm true
erlang__op_or [ErlangAtom "false", ErlangAtom "true"]  = boolToTerm true
erlang__op_or [ErlangAtom "true",  ErlangAtom "false"] = boolToTerm true
erlang__op_or [ErlangAtom "false", ErlangAtom "false"] = boolToTerm false

-- andalso
erlang__op_andalso :: ErlangFun
erlang__op_andalso [ErlangAtom "true", other] = other
erlang__op_andalso [ErlangAtom "false", _] = boolToTerm false

-- orelse
erlang__op_orelse :: ErlangFun
erlang__op_orelse [ErlangAtom "true", _] = boolToTerm true
erlang__op_orelse [ErlangAtom "false", other] = other

-- /
erlang__op_div :: ErlangFun
erlang__op_div [ErlangNum a, ErlangNum b] = ErlangNum (a `div` b)

-- *
erlang__op_mult :: ErlangFun
erlang__op_mult [ErlangNum a, ErlangNum b] = ErlangNum (a * b)

-- -
erlang__op_minus :: ErlangFun
erlang__op_minus [ErlangNum a, ErlangNum b] = ErlangNum (a - b)

-- +
erlang__op_plus :: ErlangFun
erlang__op_plus [ErlangNum a, ErlangNum b] = ErlangNum (a + b)

-- >=
erlang__op_greaterEq :: ErlangFun
erlang__op_greaterEq [a, b] = boolToTerm (a >= b)

-- >
erlang__op_greater :: ErlangFun
erlang__op_greater [a, b] = boolToTerm (a > b)

-- =<
erlang__op_lesserEq :: ErlangFun
erlang__op_lesserEq [a, b] = boolToTerm (a <= b)

-- <
erlang__op_lesser :: ErlangFun
erlang__op_lesser [a, b] = boolToTerm (a < b)

-- --
erlang__op_unAppend :: ErlangFun
erlang__op_unAppend [ErlangEmptyList, ErlangEmptyList] = ErlangEmptyList
erlang__op_unAppend [l@(ErlangCons _ _), ErlangEmptyList] = l
erlang__op_unAppend [ErlangEmptyList, ErlangCons _ _] = ErlangEmptyList
erlang__op_unAppend [ErlangCons hl tl, r@(ErlangCons hr tr)] =
  case erlang__op_exactEq [hl, hr] of
    ErlangAtom "true"  -> erlang__op_unAppend [tl, tr]
    ErlangAtom "false" -> ErlangCons hl (erlang__op_unAppend [tl, r])

-- ++
erlang__op_append :: ErlangFun
erlang__op_append [ErlangEmptyList, ErlangEmptyList] = ErlangEmptyList
erlang__op_append [ErlangEmptyList, l@(ErlangCons _ _)] = l
erlang__op_append [l@(ErlangCons _ _), ErlangEmptyList] = l
erlang__op_append [ErlangCons h t, l@(ErlangCons _ _)] =
  ErlangCons h (erlang__op_append [t, l])
erlang__op_append _ = EXT.error_badarg unit

-- -
erlang__op_neg :: ErlangFun
erlang__op_neg [ErlangNum n] = ErlangNum (-n)

-- not
erlang__op_not :: ErlangFun
erlang__op_not [ErlangAtom "false"] = ErlangAtom "true"
erlang__op_not [ErlangAtom "true"] = ErlangAtom "false"


erlang__process_display__2 :: ErlangFun
erlang__process_display__2 args = unimplemented unit

erlang__integer_to_binary__2 :: ErlangFun
erlang__integer_to_binary__2 args = unimplemented unit

erlang__integer_to_list__2 :: ErlangFun
erlang__integer_to_list__2 [ErlangNum num, ErlangNum base]
    | DM.Just radix <- DI.radix base
    = H.make_string $ DI.toStringAs radix num
erlang__integer_to_list__2 _ = EXT.error_badarg unit

erlang__fun_info_mfa__1 :: ErlangFun
erlang__fun_info_mfa__1 args = unimplemented unit

erlang__nif_error__2 :: ErlangFun
erlang__nif_error__2 args = erlang__error__2 args

erlang__get_stacktrace__0 :: ErlangFun
erlang__get_stacktrace__0 args = unimplemented unit

erlang__registered__0 :: ErlangFun
erlang__registered__0 args = unimplemented unit

erlang__get_module_info__1 :: ErlangFun
erlang__get_module_info__1 args = unimplemented unit

erlang__module_info__1 :: ErlangFun
erlang__module_info__1 args = unimplemented unit

erlang__map_get__2 :: ErlangFun
erlang__map_get__2 args = unimplemented unit

erlang__cancel_timer__1 :: ErlangFun
erlang__cancel_timer__1 args = unimplemented unit

erlang__dist_ctrl_get_data_notification__1 :: ErlangFun
erlang__dist_ctrl_get_data_notification__1 args = unimplemented unit

erlang__list_to_float__1 :: ErlangFun
erlang__list_to_float__1 args = unimplemented unit

erlang__apply__3 :: ErlangFun
erlang__apply__3 args = unimplemented unit

erlang__is_builtin__3 :: ErlangFun
erlang__is_builtin__3 args = unimplemented unit

erlang__list_to_integer__2 :: ErlangFun
erlang__list_to_integer__2 args = unimplemented unit

erlang__binary_to_atom__2 :: ErlangFun
erlang__binary_to_atom__2 args = unimplemented unit

erlang__suspend_process__1 :: ErlangFun
erlang__suspend_process__1 args = unimplemented unit

erlang__binary_to_term__2 :: ErlangFun
erlang__binary_to_term__2 args = unimplemented unit

erlang__spawn_link__2 :: ErlangFun
erlang__spawn_link__2 args = unimplemented unit

erlang__integer_to_binary__1 :: ErlangFun
erlang__integer_to_binary__1 args = unimplemented unit

erlang__get__1 :: ErlangFun
erlang__get__1 args = unimplemented unit

erlang__dist_ctrl_get_data__1 :: ErlangFun
erlang__dist_ctrl_get_data__1 args = unimplemented unit

erlang__setnode__3 :: ErlangFun
erlang__setnode__3 args = unimplemented unit

erlang__hd__1 :: ErlangFun
erlang__hd__1 args = unimplemented unit

erlang__now__0 :: ErlangFun
erlang__now__0 args = unimplemented unit

erlang__is_integer__1 :: ErlangFun
erlang__is_integer__1 [ErlangNum _] = ErlangAtom "true"
erlang__is_integer__1 [_] = ErlangAtom "false"

erlang__erase__0 :: ErlangFun
erlang__erase__0 args = unimplemented unit

erlang__ports__0 :: ErlangFun
erlang__ports__0 args = unimplemented unit

erlang__dt_spread_tag__1 :: ErlangFun
erlang__dt_spread_tag__1 args = unimplemented unit

erlang__convert_time_unit__3 :: ErlangFun
erlang__convert_time_unit__3 args = unimplemented unit

erlang__iolist_to_iovec__1 :: ErlangFun
erlang__iolist_to_iovec__1 args = unimplemented unit

erlang__iolist_to_binary__1 :: ErlangFun
erlang__iolist_to_binary__1 args = unimplemented unit

erlang__decode_packet__3 :: ErlangFun
erlang__decode_packet__3 args = unimplemented unit

erlang__get_cookie__0 :: ErlangFun
erlang__get_cookie__0 args = unimplemented unit

erlang__put__2 :: ErlangFun
erlang__put__2 args = unimplemented unit

erlang__unique_integer__1 :: ErlangFun
erlang__unique_integer__1 args = unimplemented unit

erlang__exit__2 :: ErlangFun
erlang__exit__2 args = unimplemented unit

erlang__purge_module__1 :: ErlangFun
erlang__purge_module__1 args = unimplemented unit

erlang__subtract__2 :: ErlangFun
erlang__subtract__2 args = unimplemented unit

erlang__dt_prepend_vm_tag_data__1 :: ErlangFun
erlang__dt_prepend_vm_tag_data__1 args = unimplemented unit

erlang__has_prepared_code_on_load__1 :: ErlangFun
erlang__has_prepared_code_on_load__1 args = unimplemented unit

erlang__external_size__2 :: ErlangFun
erlang__external_size__2 args = unimplemented unit

erlang__is_alive__0 :: ErlangFun
erlang__is_alive__0 args = unimplemented unit

erlang__make_tuple__2 :: ErlangFun
erlang__make_tuple__2 [ErlangNum arity, what] = ErlangTuple $ DA.replicate arity what
erlang__make_tuple__2 _ = EXT.error_badarg unit

erlang__is_port__1 :: ErlangFun
erlang__is_port__1 args = unimplemented unit

erlang__and__2 :: ErlangFun
erlang__and__2 args = unimplemented unit

erlang__is_process_alive__1 :: ErlangFun
erlang__is_process_alive__1 args = unimplemented unit

erlang__is_boolean__1 :: ErlangFun
erlang__is_boolean__1 [ErlangAtom "true"] = boolToTerm true
erlang__is_boolean__1 [ErlangAtom "false"] = boolToTerm true
erlang__is_boolean__1 [_] = boolToTerm false
erlang__is_boolean__1 _ = EXT.error_badarg unit

erlang__is_record__2 :: ErlangFun
erlang__is_record__2 args = unimplemented unit

erlang__list_to_bitstring__1 :: ErlangFun
erlang__list_to_bitstring__1 args = unimplemented unit

erlang__pid_to_list__1 :: ErlangFun
erlang__pid_to_list__1 args = unimplemented unit

erlang__dist_get_stat__1 :: ErlangFun
erlang__dist_get_stat__1 args = unimplemented unit

erlang__binary_to_integer__2 :: ErlangFun
erlang__binary_to_integer__2 args = unimplemented unit

erlang__alloc_sizes__1 :: ErlangFun
erlang__alloc_sizes__1 args = unimplemented unit

erlang__spawn_opt__2 :: ErlangFun
erlang__spawn_opt__2 args = unimplemented unit

erlang__iolist_size__1 :: ErlangFun
erlang__iolist_size__1 args = unimplemented unit

erlang__element__2 :: ErlangFun
erlang__element__2 [ErlangNum pos, ErlangTuple array] | DM.Just res <- DA.index array (pos-1) = res
erlang__element__2 _ = EXT.error_badarg unit

erlang__port_get_data__1 :: ErlangFun
erlang__port_get_data__1 args = unimplemented unit

erlang__group_leader__2 :: ErlangFun
erlang__group_leader__2 args = unimplemented unit

erlang__split_binary__2 :: ErlangFun
erlang__split_binary__2 args = unimplemented unit

erlang__function_exported__3 :: ErlangFun
erlang__function_exported__3 args = unimplemented unit

erlang__list_to_existing_atom__1 :: ErlangFun
erlang__list_to_existing_atom__1 args = unimplemented unit

erlang__phash__2 :: ErlangFun
erlang__phash__2 args = unimplemented unit

erlang__dist_ctrl_put_data__2 :: ErlangFun
erlang__dist_ctrl_put_data__2 args = unimplemented unit

erlang__garbage_collect_message_area__0 :: ErlangFun
erlang__garbage_collect_message_area__0 args = unimplemented unit

erlang__is_binary__1 :: ErlangFun
erlang__is_binary__1 args = unimplemented unit

erlang__bor__2 :: ErlangFun
erlang__bor__2 args = unimplemented unit

erlang__spawn_link__1 :: ErlangFun
erlang__spawn_link__1 args = unimplemented unit

erlang__is_tuple__1 :: ErlangFun
erlang__is_tuple__1 args = unimplemented unit

erlang__bnot__1 :: ErlangFun
erlang__bnot__1 args = unimplemented unit

erlang__is_atom__1 :: ErlangFun
erlang__is_atom__1 args = unimplemented unit

erlang__bxor__2 :: ErlangFun
erlang__bxor__2 args = unimplemented unit

erlang__garbage_collect__0 :: ErlangFun
erlang__garbage_collect__0 args = unimplemented unit

erlang__trace_pattern__3 :: ErlangFun
erlang__trace_pattern__3 args = unimplemented unit

erlang__binary_to_existing_atom__2 :: ErlangFun
erlang__binary_to_existing_atom__2 args = unimplemented unit

erlang__dt_restore_tag__1 :: ErlangFun
erlang__dt_restore_tag__1 args = unimplemented unit

erlang__port_to_list__1 :: ErlangFun
erlang__port_to_list__1 args = unimplemented unit

erlang__system_profile__0 :: ErlangFun
erlang__system_profile__0 args = unimplemented unit

erlang__match_spec_test__3 :: ErlangFun
erlang__match_spec_test__3 args = unimplemented unit

erlang__ceil__1 :: ErlangFun
erlang__ceil__1 args = unimplemented unit

erlang__float_to_list__1 :: ErlangFun
erlang__float_to_list__1 args = unimplemented unit

erlang__pre_loaded__0 :: ErlangFun
erlang__pre_loaded__0 args = unimplemented unit

erlang__display_string__1 :: ErlangFun
erlang__display_string__1 args = unimplemented unit

erlang__finish_loading__1 :: ErlangFun
erlang__finish_loading__1 args = unimplemented unit

erlang__spawn_link__3 :: ErlangFun
erlang__spawn_link__3 args = unimplemented unit

erlang__abs__1 :: ErlangFun
erlang__abs__1 args = unimplemented unit

erlang__binary_to_list__3 :: ErlangFun
erlang__binary_to_list__3 args = unimplemented unit

erlang__garbage_collect__2 :: ErlangFun
erlang__garbage_collect__2 args = unimplemented unit

erlang__system_flag__2 :: ErlangFun
erlang__system_flag__2 args = unimplemented unit

erlang__make_fun__3 :: ErlangFun
erlang__make_fun__3 args = unimplemented unit

erlang__map_size__1 :: ErlangFun
erlang__map_size__1 args = unimplemented unit

erlang__universaltime_to_localtime__1 :: ErlangFun
erlang__universaltime_to_localtime__1 args = unimplemented unit

erlang__whereis__1 :: ErlangFun
erlang__whereis__1 args = unimplemented unit

erlang__list_to_atom__1 :: ErlangFun
erlang__list_to_atom__1 args = unimplemented unit

erlang__port_call__3 :: ErlangFun
erlang__port_call__3 args = unimplemented unit

erlang__is_float__1 :: ErlangFun
erlang__is_float__1 args = unimplemented unit

erlang__date__0 :: ErlangFun
erlang__date__0 args = unimplemented unit

erlang__make_ref__0 :: ErlangFun
erlang__make_ref__0 args = unimplemented unit

erlang__or__2 :: ErlangFun
erlang__or__2 args = unimplemented unit

erlang__ref_to_list__1 :: ErlangFun
erlang__ref_to_list__1 args = unimplemented unit

erlang__port_control__3 :: ErlangFun
erlang__port_control__3 args = unimplemented unit

erlang__byte_size__1 :: ErlangFun
erlang__byte_size__1 args = unimplemented unit

erlang__check_process_code__2 :: ErlangFun
erlang__check_process_code__2 args = unimplemented unit

erlang__binary_to_list__1 :: ErlangFun
erlang__binary_to_list__1 args = unimplemented unit

erlang__is_number__1 :: ErlangFun
erlang__is_number__1 args = unimplemented unit

erlang__system_monitor__0 :: ErlangFun
erlang__system_monitor__0 args = unimplemented unit

erlang__phash2__2 :: ErlangFun
erlang__phash2__2 args = unimplemented unit

erlang__is_pid__1 :: ErlangFun
erlang__is_pid__1 args = unimplemented unit

erlang__floor__1 :: ErlangFun
erlang__floor__1 args = unimplemented unit

erlang__bitsize__1 :: ErlangFun
erlang__bitsize__1 args = unimplemented unit

erlang__list_to_binary__1 :: ErlangFun
erlang__list_to_binary__1 args = unimplemented unit

erlang__nodes__1 :: ErlangFun
erlang__nodes__1 args = unimplemented unit

erlang__term_to_binary__1 :: ErlangFun
erlang__term_to_binary__1 args = unimplemented unit

erlang__time__0 :: ErlangFun
erlang__time__0 args = unimplemented unit

erlang__time_offset__1 :: ErlangFun
erlang__time_offset__1 args = unimplemented unit

erlang__seq_trace_print__1 :: ErlangFun
erlang__seq_trace_print__1 args = unimplemented unit

erlang__send__2 :: ErlangFun
erlang__send__2 args = unimplemented unit

erlang__halt__1 :: ErlangFun
erlang__halt__1 args = unimplemented unit

erlang__spawn_opt__5 :: ErlangFun
erlang__spawn_opt__5 args = unimplemented unit

erlang__size__1 :: ErlangFun
erlang__size__1 args = unimplemented unit

erlang__process_info__1 :: ErlangFun
erlang__process_info__1 args = unimplemented unit

erlang__md5__1 :: ErlangFun
erlang__md5__1 args = unimplemented unit

erlang__binary_part__2 :: ErlangFun
erlang__binary_part__2 args = unimplemented unit

erlang__format_cpu_topology__1 :: ErlangFun
erlang__format_cpu_topology__1 args = unimplemented unit

erlang__spawn__1 :: ErlangFun
erlang__spawn__1 args = unimplemented unit

erlang__throw__1 :: ErlangFun
erlang__throw__1 [arg] = EXT.throw arg

erlang__float_to_list__2 :: ErlangFun
erlang__float_to_list__2 args = unimplemented unit

erlang__load_nif__2 :: ErlangFun
erlang__load_nif__2 args = unimplemented unit

erlang__prepare_loading__2 :: ErlangFun
erlang__prepare_loading__2 args = unimplemented unit

erlang__open_port__2 :: ErlangFun
erlang__open_port__2 args = unimplemented unit

erlang__term_to_binary__2 :: ErlangFun
erlang__term_to_binary__2 args = unimplemented unit

erlang__port_set_data__2 :: ErlangFun
erlang__port_set_data__2 args = unimplemented unit

erlang__tuple_to_list__1 :: ErlangFun
erlang__tuple_to_list__1 args = unimplemented unit

erlang__self__0 :: ErlangFun
erlang__self__0 args = unimplemented unit

erlang__read_timer__2 :: ErlangFun
erlang__read_timer__2 args = unimplemented unit

erlang__statistics__1 :: ErlangFun
erlang__statistics__1 args = unimplemented unit

erlang__max__2 :: ErlangFun
erlang__max__2 [t1, t2] | t1 >= t2  = t1
                        | otherwise = t2
erlang__max__2 _ = EXT.error_badarg unit

erlang__apply__2 :: ErlangFun
erlang__apply__2 args = unimplemented unit

erlang__nodes__0 :: ErlangFun
erlang__nodes__0 args = unimplemented unit

erlang__insert_element__3 :: ErlangFun
erlang__insert_element__3 args = unimplemented unit

erlang__binary_to_term__1 :: ErlangFun
erlang__binary_to_term__1 args = unimplemented unit

erlang__is_bitstring__1 :: ErlangFun
erlang__is_bitstring__1 args = unimplemented unit

erlang__bsr__2 :: ErlangFun
erlang__bsr__2 args = unimplemented unit

erlang__list_to_integer__1 :: ErlangFun
erlang__list_to_integer__1 args = unimplemented unit

erlang__spawn__3 :: ErlangFun
erlang__spawn__3 args = unimplemented unit

erlang__send_after__4 :: ErlangFun
erlang__send_after__4 args = unimplemented unit

erlang__trace__3 :: ErlangFun
erlang__trace__3 args = unimplemented unit

erlang__adler32__1 :: ErlangFun
erlang__adler32__1 args = unimplemented unit

erlang__send_nosuspend__2 :: ErlangFun
erlang__send_nosuspend__2 args = unimplemented unit

erlang__dt_get_tag_data__0 :: ErlangFun
erlang__dt_get_tag_data__0 args = unimplemented unit

erlang__resume_process__1 :: ErlangFun
erlang__resume_process__1 args = unimplemented unit

erlang__rem__2 :: ErlangFun
erlang__rem__2 [ErlangNum left, ErlangNum right] = ErlangNum (mod left right)
erlang__rem__2 _ = EXT.error_badarg unit

erlang__port_close__1 :: ErlangFun
erlang__port_close__1 args = unimplemented unit

erlang__is_function__1 :: ErlangFun
erlang__is_function__1 args = unimplemented unit

erlang__port_call__2 :: ErlangFun
erlang__port_call__2 args = unimplemented unit

erlang__dt_get_tag__0 :: ErlangFun
erlang__dt_get_tag__0 args = unimplemented unit

erlang__universaltime_to_posixtime__1 :: ErlangFun
erlang__universaltime_to_posixtime__1 args = unimplemented unit

erlang__append__2 :: ErlangFun
erlang__append__2 args = unimplemented unit

erlang__md5_init__0 :: ErlangFun
erlang__md5_init__0 args = unimplemented unit

erlang__list_to_port__1 :: ErlangFun
erlang__list_to_port__1 args = unimplemented unit

erlang__link__1 :: ErlangFun
erlang__link__1 args = unimplemented unit

erlang__spawn_opt__1 :: ErlangFun
erlang__spawn_opt__1 args = unimplemented unit

erlang__binary_to_float__1 :: ErlangFun
erlang__binary_to_float__1 args = unimplemented unit

erlang__monitor_node__2 :: ErlangFun
erlang__monitor_node__2 args = unimplemented unit

erlang__time_offset__0 :: ErlangFun
erlang__time_offset__0 args = unimplemented unit

erlang__port_connect__2 :: ErlangFun
erlang__port_connect__2 args = unimplemented unit

erlang__setelement__3 :: ErlangFun
erlang__setelement__3 [ErlangNum pos, ErlangTuple tuple, new_el]
    | DM.Just new_tuple <- DA.updateAt (pos-1) new_el tuple =
    ErlangTuple new_tuple
erlang__setelement__3 _ = EXT.error_badarg unit

erlang__gather_gc_info_result__1 :: ErlangFun
erlang__gather_gc_info_result__1 args = unimplemented unit

erlang__tuple_size__1 :: ErlangFun
erlang__tuple_size__1 args = unimplemented unit

erlang__system_monitor__1 :: ErlangFun
erlang__system_monitor__1 args = unimplemented unit

erlang__timestamp__0 :: ErlangFun
erlang__timestamp__0 args = unimplemented unit

erlang__system_time__1 :: ErlangFun
erlang__system_time__1 args = unimplemented unit

erlang__register__2 :: ErlangFun
erlang__register__2 args = unimplemented unit

erlang__dmonitor_node__3 :: ErlangFun
erlang__dmonitor_node__3 args = unimplemented unit

erlang__fun_info__2 :: ErlangFun
erlang__fun_info__2 args = unimplemented unit

erlang__finish_after_on_load__2 :: ErlangFun
erlang__finish_after_on_load__2 args = unimplemented unit

erlang__tl__1 :: ErlangFun
erlang__tl__1 args = unimplemented unit

erlang__send_after__3 :: ErlangFun
erlang__send_after__3 args = unimplemented unit

erlang__trace_pattern__2 :: ErlangFun
erlang__trace_pattern__2 args = unimplemented unit

erlang__port_command__3 :: ErlangFun
erlang__port_command__3 args = unimplemented unit

erlang__alloc_info__1 :: ErlangFun
erlang__alloc_info__1 args = unimplemented unit

erlang__list_to_ref__1 :: ErlangFun
erlang__list_to_ref__1 args = unimplemented unit

erlang__port_command__2 :: ErlangFun
erlang__port_command__2 args = unimplemented unit

erlang__external_size__1 :: ErlangFun
erlang__external_size__1 args = unimplemented unit

erlang__atom_to_binary__2 :: ErlangFun
erlang__atom_to_binary__2 args = unimplemented unit

erlang__spawn_opt__3 :: ErlangFun
erlang__spawn_opt__3 args = unimplemented unit

erlang__exit_signal__2 :: ErlangFun
erlang__exit_signal__2 args = unimplemented unit

erlang__display_nl__0 :: ErlangFun
erlang__display_nl__0 args = unimplemented unit

erlang__append_element__2 :: ErlangFun
erlang__append_element__2 args = unimplemented unit

erlang__delete_element__2 :: ErlangFun
erlang__delete_element__2 args = unimplemented unit

erlang__xor__2 :: ErlangFun
erlang__xor__2 args = unimplemented unit

erlang__is_reference__1 :: ErlangFun
erlang__is_reference__1 args = unimplemented unit

erlang__round__1 :: ErlangFun
erlang__round__1 args = unimplemented unit

erlang__crc32__2 :: ErlangFun
erlang__crc32__2 args = unimplemented unit

erlang__not__1 :: ErlangFun
erlang__not__1 args = unimplemented unit

erlang__adler32__2 :: ErlangFun
erlang__adler32__2 args = unimplemented unit

erlang__md5_final__1 :: ErlangFun
erlang__md5_final__1 args = unimplemented unit

erlang__monitor_node__3 :: ErlangFun
erlang__monitor_node__3 args = unimplemented unit

erlang__monotonic_time__0 :: ErlangFun
erlang__monotonic_time__0 args = unimplemented unit

erlang__length__1 :: ErlangFun
erlang__length__1 [ErlangEmptyList] = ErlangNum 0
erlang__length__1 [ErlangCons _ t] =
    case erlang__length__1 [t] of
      ErlangNum tl -> ErlangNum $ tl+1
      _ -> EXT.error_badarg unit
erlang__length__1 _ = EXT.error_badarg unit

erlang__nif_error__1 :: ErlangFun
erlang__nif_error__1 args = erlang__error__1 args

erlang__check_process_code__3 :: ErlangFun
erlang__check_process_code__3 args = unimplemented unit

erlang__localtime__0 :: ErlangFun
erlang__localtime__0 args = unimplemented unit

erlang__trace_delivered__1 :: ErlangFun
erlang__trace_delivered__1 args = unimplemented unit

erlang__module_info__0 :: ErlangFun
erlang__module_info__0 args = unimplemented unit

erlang__spawn__2 :: ErlangFun
erlang__spawn__2 args = unimplemented unit

erlang__set_cookie__2 :: ErlangFun
erlang__set_cookie__2 args = unimplemented unit

erlang__seq_trace_print__2 :: ErlangFun
erlang__seq_trace_print__2 args = unimplemented unit

erlang__suspend_process__2 :: ErlangFun
erlang__suspend_process__2 args = unimplemented unit

erlang__crc32_combine__3 :: ErlangFun
erlang__crc32_combine__3 args = unimplemented unit

erlang__process_info__2 :: ErlangFun
erlang__process_info__2 args = unimplemented unit

erlang__unique_integer__0 :: ErlangFun
erlang__unique_integer__0 args = unimplemented unit

erlang__system_time__0 :: ErlangFun
erlang__system_time__0 args = unimplemented unit

erlang__yield__0 :: ErlangFun
erlang__yield__0 args = unimplemented unit

erlang__posixtime_to_universaltime__1 :: ErlangFun
erlang__posixtime_to_universaltime__1 args = unimplemented unit

erlang__start_timer__3 :: ErlangFun
erlang__start_timer__3 args = unimplemented unit

erlang__float_to_binary__1 :: ErlangFun
erlang__float_to_binary__1 args = unimplemented unit

erlang__erase__1 :: ErlangFun
erlang__erase__1 args = unimplemented unit

erlang__port_info__1 :: ErlangFun
erlang__port_info__1 args = unimplemented unit

erlang__binary_to_integer__1 :: ErlangFun
erlang__binary_to_integer__1 args = unimplemented unit

erlang__process_flag__2 :: ErlangFun
erlang__process_flag__2 args = unimplemented unit

erlang__monotonic_time__1 :: ErlangFun
erlang__monotonic_time__1 args = unimplemented unit

erlang__seq_trace_info__1 :: ErlangFun
erlang__seq_trace_info__1 args = unimplemented unit

erlang__memory__1 :: ErlangFun
erlang__memory__1 args = unimplemented unit

erlang__get_module_info__2 :: ErlangFun
erlang__get_module_info__2 args = unimplemented unit

erlang__seq_trace__2 :: ErlangFun
erlang__seq_trace__2 args = unimplemented unit

erlang__float__1 :: ErlangFun
erlang__float__1 args = unimplemented unit

erlang__disconnect_node__1 :: ErlangFun
erlang__disconnect_node__1 args = unimplemented unit

erlang__setnode__2 :: ErlangFun
erlang__setnode__2 args = unimplemented unit

erlang__bsl__2 :: ErlangFun
erlang__bsl__2 args = unimplemented unit

erlang__atom_to_list__1 :: ErlangFun
erlang__atom_to_list__1 [ErlangAtom atom] = H.make_string atom
erlang__atom_to_list__1 _ = EXT.error_badarg unit

erlang__get_keys__0 :: ErlangFun
erlang__get_keys__0 args = unimplemented unit

erlang__is_list__1 :: ErlangFun
erlang__is_list__1 [ErlangEmptyList] = boolToTerm true
erlang__is_list__1 [ErlangCons _ _] = boolToTerm true
erlang__is_list__1 [_] = boolToTerm false

erlang__node__0 :: ErlangFun
erlang__node__0 args = unimplemented unit

erlang__fun_info__1 :: ErlangFun
erlang__fun_info__1 args = unimplemented unit

erlang__exit__1 :: ErlangFun
erlang__exit__1 [arg] = EXT.exit arg

erlang__system_info__1 :: ErlangFun
erlang__system_info__1 args = unimplemented unit

erlang__binary_part__3 :: ErlangFun
erlang__binary_part__3 args = unimplemented unit

erlang__is_map__1 :: ErlangFun
erlang__is_map__1 args = unimplemented unit

erlang__is_map_key__2 :: ErlangFun
erlang__is_map_key__2 args = unimplemented unit

erlang__halt__2 :: ErlangFun
erlang__halt__2 args = unimplemented unit

erlang__localtime_to_universaltime__2 :: ErlangFun
erlang__localtime_to_universaltime__2 args = unimplemented unit

erlang__cancel_timer__2 :: ErlangFun
erlang__cancel_timer__2 args = unimplemented unit

erlang__display__1 :: ErlangFun
erlang__display__1 args = unimplemented unit

erlang__load_module__2 :: ErlangFun
erlang__load_module__2 args = unimplemented unit

erlang__dt_append_vm_tag_data__1 :: ErlangFun
erlang__dt_append_vm_tag_data__1 args = unimplemented unit

erlang__port_info__2 :: ErlangFun
erlang__port_info__2 args = unimplemented unit

erlang__get__0 :: ErlangFun
erlang__get__0 args = unimplemented unit

erlang__unregister__1 :: ErlangFun
erlang__unregister__1 args = unimplemented unit

erlang__integer_to_list__1 :: ErlangFun
erlang__integer_to_list__1 [ErlangNum num] = H.make_string $ DI.toStringAs DI.decimal num
erlang__integer_to_list__1 _ = EXT.error_badarg unit

erlang__dist_ctrl_input_handler__2 :: ErlangFun
erlang__dist_ctrl_input_handler__2 args = unimplemented unit

erlang__trunc__1 :: ErlangFun
erlang__trunc__1 args = unimplemented unit

erlang__fun_to_list__1 :: ErlangFun
erlang__fun_to_list__1 args = unimplemented unit

erlang__crasher__6 :: ErlangFun
erlang__crasher__6 args = unimplemented unit

erlang__read_timer__1 :: ErlangFun
erlang__read_timer__1 args = unimplemented unit

erlang__module_loaded__1 :: ErlangFun
erlang__module_loaded__1 args = unimplemented unit

erlang__md5_update__2 :: ErlangFun
erlang__md5_update__2 args = unimplemented unit

erlang__localtime_to_universaltime__1 :: ErlangFun
erlang__localtime_to_universaltime__1 args = unimplemented unit

erlang__spawn_opt__4 :: ErlangFun
erlang__spawn_opt__4 args = unimplemented unit

erlang__group_leader__0 :: ErlangFun
erlang__group_leader__0 args = unimplemented unit

erlang__dt_put_tag__1 :: ErlangFun
erlang__dt_put_tag__1 args = unimplemented unit

erlang__phash2__1 :: ErlangFun
erlang__phash2__1 args = unimplemented unit

erlang__spawn__4 :: ErlangFun
erlang__spawn__4 args = unimplemented unit

erlang__crc32__1 :: ErlangFun
erlang__crc32__1 args = unimplemented unit

erlang__band__2 :: ErlangFun
erlang__band__2 args = unimplemented unit

erlang__send__3 :: ErlangFun
erlang__send__3 args = unimplemented unit

erlang__is_function__2 :: ErlangFun
erlang__is_function__2 args = unimplemented unit

erlang__system_monitor__2 :: ErlangFun
erlang__system_monitor__2 args = unimplemented unit

erlang__spawn_monitor__3 :: ErlangFun
erlang__spawn_monitor__3 args = unimplemented unit

erlang__list_to_pid__1 :: ErlangFun
erlang__list_to_pid__1 args = unimplemented unit

erlang__get_keys__1 :: ErlangFun
erlang__get_keys__1 args = unimplemented unit

erlang__demonitor__2 :: ErlangFun
erlang__demonitor__2 args = unimplemented unit

erlang__raise__3 :: ErlangFun
erlang__raise__3 args = unimplemented unit

erlang__hibernate__3 :: ErlangFun
erlang__hibernate__3 args = unimplemented unit

erlang__adler32_combine__3 :: ErlangFun
erlang__adler32_combine__3 args = unimplemented unit

erlang__node__1 :: ErlangFun
erlang__node__1 args = unimplemented unit

erlang__monitor__2 :: ErlangFun
erlang__monitor__2 args = unimplemented unit

erlang__start_timer__4 :: ErlangFun
erlang__start_timer__4 args = unimplemented unit

erlang__bit_size__1 :: ErlangFun
erlang__bit_size__1 args = unimplemented unit

erlang__call_on_load_function__1 :: ErlangFun
erlang__call_on_load_function__1 args = unimplemented unit

erlang__processes__0 :: ErlangFun
erlang__processes__0 args = unimplemented unit

erlang__error__2 :: ErlangFun
erlang__error__2 [err, _] = erlang__error__1 [err]
erlang__error__2 _ = EXT.error_badarg unit

erlang__loaded__0 :: ErlangFun
erlang__loaded__0 args = unimplemented unit

erlang__bump_reductions__1 :: ErlangFun
erlang__bump_reductions__1 args = unimplemented unit

erlang__send_nosuspend__3 :: ErlangFun
erlang__send_nosuspend__3 args = unimplemented unit

erlang__make_tuple__3 :: ErlangFun
erlang__make_tuple__3 args = unimplemented unit

erlang__unlink__1 :: ErlangFun
erlang__unlink__1 args = unimplemented unit

erlang__demonitor__1 :: ErlangFun
erlang__demonitor__1 args = unimplemented unit

erlang__trace_info__2 :: ErlangFun
erlang__trace_info__2 args = unimplemented unit

erlang__delete_module__1 :: ErlangFun
erlang__delete_module__1 args = unimplemented unit

erlang__garbage_collect__1 :: ErlangFun
erlang__garbage_collect__1 args = unimplemented unit

erlang__check_old_code__1 :: ErlangFun
erlang__check_old_code__1 args = unimplemented unit

erlang__spawn_monitor__1 :: ErlangFun
erlang__spawn_monitor__1 args = unimplemented unit

erlang__min__2 :: ErlangFun
erlang__min__2 [t1, t2] | t1 <= t2  = t1
                        | otherwise = t2
erlang__min__2 _ = EXT.error_badarg unit

erlang__float_to_binary__2 :: ErlangFun
erlang__float_to_binary__2 args = unimplemented unit

erlang__system_profile__2 :: ErlangFun
erlang__system_profile__2 args = unimplemented unit

erlang__error__1 :: ErlangFun
erlang__error__1 [arg] = EXT.error arg
erlang__error__1 _ = EXT.error_badarg unit

erlang__delay_trap__2 :: ErlangFun
erlang__delay_trap__2 args = unimplemented unit

erlang__spawn_link__4 :: ErlangFun
erlang__spawn_link__4 args = unimplemented unit

erlang__is_record__3 :: ErlangFun
erlang__is_record__3 args = unimplemented unit

erlang__memory__0 :: ErlangFun
erlang__memory__0 args = unimplemented unit

erlang__halt__0 :: ErlangFun
erlang__halt__0 args = unimplemented unit

erlang__process_flag__3 :: ErlangFun
erlang__process_flag__3 args = unimplemented unit

erlang__set_cpu_topology__1 :: ErlangFun
erlang__set_cpu_topology__1 args = unimplemented unit

erlang__list_to_tuple__1 :: ErlangFun
erlang__list_to_tuple__1 [list] | DM.Just r <- erlangListToList list =
    ErlangTuple (DA.fromFoldable r)
erlang__list_to_tuple__1 _ = EXT.error_badarg unit

erlang__universaltime__0 :: ErlangFun
erlang__universaltime__0 args = unimplemented unit

erlang__bitstring_to_list__1 :: ErlangFun
erlang__bitstring_to_list__1 args = unimplemented unit
