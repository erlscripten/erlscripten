module Erlang.Builtins where

import Erlang.Type
import Prelude
import Data.Maybe as DM
import Data.Array as DA
import Data.List as DL
import Control.Monad
import Effect.Exception (throw)
import Effect


lists__keysearch__3 :: ErlangFun
lists__keysearch__3 [_, _, ErlangEmptyList] = pure (boolToTerm false)
lists__keysearch__3 [key, idx@(ErlangNum idxNum), ErlangCons el rest] = case el of
  ErlangTuple tup | DM.Just x <- DA.index tup idxNum  -> do
    found <- erlang__op_exactEq [x, key]
    if found == boolToTerm true
      then pure (ErlangTuple [ErlangAtom "value", el])
      else lists__keysearch__3 [key, idx, rest]
  _ -> lists__keysearch__3 [key, idx, rest]

lists__keymember__3 :: ErlangFun
lists__keymember__3 [_, _, ErlangEmptyList] = pure (boolToTerm false)
lists__keymember__3 [key, idx@(ErlangNum idxNum), ErlangCons el rest] = case el of
  ErlangTuple tup | DM.Just x <- DA.index tup idxNum  -> do
    found <- erlang__op_exactEq [x, key]
    if found == boolToTerm true
      then pure found
      else lists__keymember__3 [key, idx, rest]
  _ -> lists__keymember__3 [key, idx, rest]

lists__reverse__2 :: ErlangFun
lists__reverse__2 [ErlangEmptyList, acc] = pure acc
lists__reverse__2 [ErlangCons h t, acc] = lists__reverse__2 [t, ErlangCons h acc]

lists__member__2 :: ErlangFun
lists__member__2 [_, ErlangEmptyList] = pure (boolToTerm false)
lists__member__2 [x, ErlangCons el rest] = do
  found <- erlang__op_exactEq [x, el]
  if found == boolToTerm true
    then pure found
    else lists__member__2 [x, rest]

lists__keyfind__3 :: ErlangFun
lists__keyfind__3 [_, _, ErlangEmptyList] = pure (boolToTerm false)
lists__keyfind__3 [key, idx@(ErlangNum idxNum), ErlangCons el rest] = case el of
  ErlangTuple tup | DM.Just x <- DA.index tup idxNum  -> do
    found <- erlang__op_exactEq [x, key]
    if found == boolToTerm true
      then pure el
      else lists__keyfind__3 [key, idx, rest]
  _ -> lists__keyfind__3 [key, idx, rest]

--------------------------------------------------------------------------------

maps__get__2 :: ErlangFun
maps__get__2 [] = throw "unimplemented"

maps__find__2 :: ErlangFun
maps__find__2 [] = throw "unimplemented"

maps__from_list__1 :: ErlangFun
maps__from_list__1 [] = throw "unimplemented"

maps__is_key__2 :: ErlangFun
maps__is_key__2 [] = throw "unimplemented"

maps__keys__1 :: ErlangFun
maps__keys__1 [] = throw "unimplemented"

maps__merge__2 :: ErlangFun
maps__merge__2 [] = throw "unimplemented"

maps__put__3 :: ErlangFun
maps__put__3 [] = throw "unimplemented"

maps__remove__2 :: ErlangFun
maps__remove__2 [] = throw "unimplemented"

maps__take__2 :: ErlangFun
maps__take__2 [] = throw "unimplemented"

maps__to_list__1 :: ErlangFun
maps__to_list__1 [] = throw "unimplemented"

maps__update__3 :: ErlangFun
maps__update__3 [] = throw "unimplemented"

maps__values__1 :: ErlangFun
maps__values__1 [] = throw "unimplemented"


--------------------------------------------------------------------------------


-- =/=
erlang__op_exactNeq :: ErlangFun
erlang__op_exactNeq [a, b] = pure (boolToTerm (a /= b))  -- FIXME (funs)

-- =:=
erlang__op_exactEq :: ErlangFun
erlang__op_exactEq [a, b] = pure (boolToTerm (a == b)) -- FIXME (funs)

-- /=
erlang__op_neq :: ErlangFun
erlang__op_neq [a, b] = pure (boolToTerm (a /= b)) -- FIXME (funs, floats)

-- ==
erlang__op_eq :: ErlangFun
erlang__op_eq [a, b] = pure (boolToTerm (a == b)) -- FIXME (funs, floats)

-- and
erlang__op_and :: ErlangFun
erlang__op_and [ErlangAtom "true",  ErlangAtom "true"]  = pure (boolToTerm true)
erlang__op_and [ErlangAtom "false", ErlangAtom "true"]  = pure (boolToTerm false)
erlang__op_and [ErlangAtom "true",  ErlangAtom "false"] = pure (boolToTerm false)
erlang__op_and [ErlangAtom "false", ErlangAtom "false"] = pure (boolToTerm false)

-- or
erlang__op_or :: ErlangFun
erlang__op_or [ErlangAtom "true",  ErlangAtom "true"]  = pure (boolToTerm true)
erlang__op_or [ErlangAtom "false", ErlangAtom "true"]  = pure (boolToTerm true)
erlang__op_or [ErlangAtom "true",  ErlangAtom "false"] = pure (boolToTerm true)
erlang__op_or [ErlangAtom "false", ErlangAtom "false"] = pure (boolToTerm false)

-- andalso
erlang__op_andalso :: ErlangFun
erlang__op_andalso [ErlangAtom "true", other] = pure other
erlang__op_andalso [ErlangAtom "false", _] = pure (boolToTerm false)

-- orelse
erlang__op_orelse :: ErlangFun
erlang__op_orelse [ErlangAtom "true", _] = pure (boolToTerm true)
erlang__op_orelse [ErlangAtom "false", other] = pure other

-- /
erlang__op_div :: ErlangFun
erlang__op_div [ErlangNum a, ErlangNum b] = pure (ErlangNum (a `div` b))

-- *
erlang__op_mult :: ErlangFun
erlang__op_mult [ErlangNum a, ErlangNum b] = pure (ErlangNum (a * b))

-- -
erlang__op_minus :: ErlangFun
erlang__op_minus [ErlangNum a, ErlangNum b] = pure (ErlangNum (a - b))

-- +
erlang__op_plus :: ErlangFun
erlang__op_plus [ErlangNum a, ErlangNum b] = pure (ErlangNum (a + b))

-- >=
erlang__op_greaterEq :: ErlangFun
erlang__op_greaterEq [a, b] = pure (boolToTerm (a >= b))

-- >
erlang__op_greater :: ErlangFun
erlang__op_greater [a, b] = pure (boolToTerm (a > b))

-- =<
erlang__op_lesserEq :: ErlangFun
erlang__op_lesserEq [a, b] = pure (boolToTerm (a <= b))

-- <
erlang__op_lesser :: ErlangFun
erlang__op_lesser [a, b] = pure (boolToTerm (a < b))

-- --
erlang__op_unAppend :: ErlangFun
erlang__op_unAppend [ErlangEmptyList, ErlangEmptyList] = pure ErlangEmptyList
erlang__op_unAppend [l@(ErlangCons _ _), ErlangEmptyList] = pure l
erlang__op_unAppend [ErlangEmptyList, ErlangCons _ _] = pure ErlangEmptyList
erlang__op_unAppend [ErlangCons hl tl, r@(ErlangCons hr tr)] = do
  remove <- erlang__op_exactEq [hl, hr]
  case remove of
    ErlangAtom "true"  -> erlang__op_unAppend [tl, tr]
    ErlangAtom "false" -> map (ErlangCons hl) (erlang__op_unAppend [tl, r])

-- ++
erlang__op_append :: ErlangFun
erlang__op_append [ErlangEmptyList, ErlangEmptyList] = pure ErlangEmptyList
erlang__op_append [ErlangEmptyList, l@(ErlangCons _ _)] = pure l
erlang__op_append [l@(ErlangCons _ _), ErlangEmptyList] = pure l
erlang__op_append [ErlangCons h t, l@(ErlangCons _ _)] =
  map (ErlangCons h) (erlang__op_append [t, l])



erlang__process_display__2 :: ErlangFun
erlang__process_display__2 args = throw "unimplemented"

erlang__integer_to_binary__2 :: ErlangFun
erlang__integer_to_binary__2 args = throw "unimplemented"

erlang__integer_to_list__2 :: ErlangFun
erlang__integer_to_list__2 args = throw "unimplemented"

erlang__fun_info_mfa__1 :: ErlangFun
erlang__fun_info_mfa__1 args = throw "unimplemented"

erlang__nif_error__2 :: ErlangFun
erlang__nif_error__2 args = throw "unimplemented"

erlang__get_stacktrace__0 :: ErlangFun
erlang__get_stacktrace__0 args = throw "unimplemented"

erlang__registered__0 :: ErlangFun
erlang__registered__0 args = throw "unimplemented"

erlang__get_module_info__1 :: ErlangFun
erlang__get_module_info__1 args = throw "unimplemented"

erlang__module_info__1 :: ErlangFun
erlang__module_info__1 args = throw "unimplemented"

erlang__map_get__2 :: ErlangFun
erlang__map_get__2 args = throw "unimplemented"

erlang__cancel_timer__1 :: ErlangFun
erlang__cancel_timer__1 args = throw "unimplemented"

erlang__dist_ctrl_get_data_notification__1 :: ErlangFun
erlang__dist_ctrl_get_data_notification__1 args = throw "unimplemented"

erlang__list_to_float__1 :: ErlangFun
erlang__list_to_float__1 args = throw "unimplemented"

erlang__apply__3 :: ErlangFun
erlang__apply__3 args = throw "unimplemented"

erlang__is_builtin__3 :: ErlangFun
erlang__is_builtin__3 args = throw "unimplemented"

erlang__list_to_integer__2 :: ErlangFun
erlang__list_to_integer__2 args = throw "unimplemented"

erlang__binary_to_atom__2 :: ErlangFun
erlang__binary_to_atom__2 args = throw "unimplemented"

erlang__suspend_process__1 :: ErlangFun
erlang__suspend_process__1 args = throw "unimplemented"

erlang__binary_to_term__2 :: ErlangFun
erlang__binary_to_term__2 args = throw "unimplemented"

erlang__spawn_link__2 :: ErlangFun
erlang__spawn_link__2 args = throw "unimplemented"

erlang__integer_to_binary__1 :: ErlangFun
erlang__integer_to_binary__1 args = throw "unimplemented"

erlang__get__1 :: ErlangFun
erlang__get__1 args = throw "unimplemented"

erlang__dist_ctrl_get_data__1 :: ErlangFun
erlang__dist_ctrl_get_data__1 args = throw "unimplemented"

erlang__setnode__3 :: ErlangFun
erlang__setnode__3 args = throw "unimplemented"

erlang__hd__1 :: ErlangFun
erlang__hd__1 args = throw "unimplemented"

erlang__now__0 :: ErlangFun
erlang__now__0 args = throw "unimplemented"

erlang__is_integer__1 :: ErlangFun
erlang__is_integer__1 args = throw "unimplemented"

erlang__erase__0 :: ErlangFun
erlang__erase__0 args = throw "unimplemented"

erlang__ports__0 :: ErlangFun
erlang__ports__0 args = throw "unimplemented"

erlang__dt_spread_tag__1 :: ErlangFun
erlang__dt_spread_tag__1 args = throw "unimplemented"

erlang__convert_time_unit__3 :: ErlangFun
erlang__convert_time_unit__3 args = throw "unimplemented"

erlang__iolist_to_iovec__1 :: ErlangFun
erlang__iolist_to_iovec__1 args = throw "unimplemented"

erlang__iolist_to_binary__1 :: ErlangFun
erlang__iolist_to_binary__1 args = throw "unimplemented"

erlang__decode_packet__3 :: ErlangFun
erlang__decode_packet__3 args = throw "unimplemented"

erlang__get_cookie__0 :: ErlangFun
erlang__get_cookie__0 args = throw "unimplemented"

erlang__put__2 :: ErlangFun
erlang__put__2 args = throw "unimplemented"

erlang__unique_integer__1 :: ErlangFun
erlang__unique_integer__1 args = throw "unimplemented"

erlang__exit__2 :: ErlangFun
erlang__exit__2 args = throw "unimplemented"

erlang__purge_module__1 :: ErlangFun
erlang__purge_module__1 args = throw "unimplemented"

erlang__subtract__2 :: ErlangFun
erlang__subtract__2 args = throw "unimplemented"

erlang__dt_prepend_vm_tag_data__1 :: ErlangFun
erlang__dt_prepend_vm_tag_data__1 args = throw "unimplemented"

erlang__has_prepared_code_on_load__1 :: ErlangFun
erlang__has_prepared_code_on_load__1 args = throw "unimplemented"

erlang__external_size__2 :: ErlangFun
erlang__external_size__2 args = throw "unimplemented"

erlang__is_alive__0 :: ErlangFun
erlang__is_alive__0 args = throw "unimplemented"

erlang__make_tuple__2 :: ErlangFun
erlang__make_tuple__2 args = throw "unimplemented"

erlang__is_port__1 :: ErlangFun
erlang__is_port__1 args = throw "unimplemented"

erlang__and__2 :: ErlangFun
erlang__and__2 args = throw "unimplemented"

erlang__is_process_alive__1 :: ErlangFun
erlang__is_process_alive__1 args = throw "unimplemented"

erlang__is_boolean__1 :: ErlangFun
erlang__is_boolean__1 args = throw "unimplemented"

erlang__is_record__2 :: ErlangFun
erlang__is_record__2 args = throw "unimplemented"

erlang__list_to_bitstring__1 :: ErlangFun
erlang__list_to_bitstring__1 args = throw "unimplemented"

erlang__pid_to_list__1 :: ErlangFun
erlang__pid_to_list__1 args = throw "unimplemented"

erlang__dist_get_stat__1 :: ErlangFun
erlang__dist_get_stat__1 args = throw "unimplemented"

erlang__binary_to_integer__2 :: ErlangFun
erlang__binary_to_integer__2 args = throw "unimplemented"

erlang__alloc_sizes__1 :: ErlangFun
erlang__alloc_sizes__1 args = throw "unimplemented"

erlang__spawn_opt__2 :: ErlangFun
erlang__spawn_opt__2 args = throw "unimplemented"

erlang__iolist_size__1 :: ErlangFun
erlang__iolist_size__1 args = throw "unimplemented"

erlang__element__2 :: ErlangFun
erlang__element__2 args = throw "unimplemented"

erlang__port_get_data__1 :: ErlangFun
erlang__port_get_data__1 args = throw "unimplemented"

erlang__group_leader__2 :: ErlangFun
erlang__group_leader__2 args = throw "unimplemented"

erlang__split_binary__2 :: ErlangFun
erlang__split_binary__2 args = throw "unimplemented"

erlang__function_exported__3 :: ErlangFun
erlang__function_exported__3 args = throw "unimplemented"

erlang__list_to_existing_atom__1 :: ErlangFun
erlang__list_to_existing_atom__1 args = throw "unimplemented"

erlang__phash__2 :: ErlangFun
erlang__phash__2 args = throw "unimplemented"

erlang__dist_ctrl_put_data__2 :: ErlangFun
erlang__dist_ctrl_put_data__2 args = throw "unimplemented"

erlang__garbage_collect_message_area__0 :: ErlangFun
erlang__garbage_collect_message_area__0 args = throw "unimplemented"

erlang__is_binary__1 :: ErlangFun
erlang__is_binary__1 args = throw "unimplemented"

erlang__bor__2 :: ErlangFun
erlang__bor__2 args = throw "unimplemented"

erlang__spawn_link__1 :: ErlangFun
erlang__spawn_link__1 args = throw "unimplemented"

erlang__is_tuple__1 :: ErlangFun
erlang__is_tuple__1 args = throw "unimplemented"

erlang__bnot__1 :: ErlangFun
erlang__bnot__1 args = throw "unimplemented"

erlang__is_atom__1 :: ErlangFun
erlang__is_atom__1 args = throw "unimplemented"

erlang__bxor__2 :: ErlangFun
erlang__bxor__2 args = throw "unimplemented"

erlang__garbage_collect__0 :: ErlangFun
erlang__garbage_collect__0 args = throw "unimplemented"

erlang__trace_pattern__3 :: ErlangFun
erlang__trace_pattern__3 args = throw "unimplemented"

erlang__binary_to_existing_atom__2 :: ErlangFun
erlang__binary_to_existing_atom__2 args = throw "unimplemented"

erlang__dt_restore_tag__1 :: ErlangFun
erlang__dt_restore_tag__1 args = throw "unimplemented"

erlang__port_to_list__1 :: ErlangFun
erlang__port_to_list__1 args = throw "unimplemented"

erlang__system_profile__0 :: ErlangFun
erlang__system_profile__0 args = throw "unimplemented"

erlang__match_spec_test__3 :: ErlangFun
erlang__match_spec_test__3 args = throw "unimplemented"

erlang__ceil__1 :: ErlangFun
erlang__ceil__1 args = throw "unimplemented"

erlang__float_to_list__1 :: ErlangFun
erlang__float_to_list__1 args = throw "unimplemented"

erlang__pre_loaded__0 :: ErlangFun
erlang__pre_loaded__0 args = throw "unimplemented"

erlang__display_string__1 :: ErlangFun
erlang__display_string__1 args = throw "unimplemented"

erlang__finish_loading__1 :: ErlangFun
erlang__finish_loading__1 args = throw "unimplemented"

erlang__spawn_link__3 :: ErlangFun
erlang__spawn_link__3 args = throw "unimplemented"

erlang__abs__1 :: ErlangFun
erlang__abs__1 args = throw "unimplemented"

erlang__binary_to_list__3 :: ErlangFun
erlang__binary_to_list__3 args = throw "unimplemented"

erlang__garbage_collect__2 :: ErlangFun
erlang__garbage_collect__2 args = throw "unimplemented"

erlang__system_flag__2 :: ErlangFun
erlang__system_flag__2 args = throw "unimplemented"

erlang__make_fun__3 :: ErlangFun
erlang__make_fun__3 args = throw "unimplemented"

erlang__map_size__1 :: ErlangFun
erlang__map_size__1 args = throw "unimplemented"

erlang__universaltime_to_localtime__1 :: ErlangFun
erlang__universaltime_to_localtime__1 args = throw "unimplemented"

erlang__whereis__1 :: ErlangFun
erlang__whereis__1 args = throw "unimplemented"

erlang__list_to_atom__1 :: ErlangFun
erlang__list_to_atom__1 args = throw "unimplemented"

erlang__port_call__3 :: ErlangFun
erlang__port_call__3 args = throw "unimplemented"

erlang__is_float__1 :: ErlangFun
erlang__is_float__1 args = throw "unimplemented"

erlang__date__0 :: ErlangFun
erlang__date__0 args = throw "unimplemented"

erlang__make_ref__0 :: ErlangFun
erlang__make_ref__0 args = throw "unimplemented"

erlang__or__2 :: ErlangFun
erlang__or__2 args = throw "unimplemented"

erlang__ref_to_list__1 :: ErlangFun
erlang__ref_to_list__1 args = throw "unimplemented"

erlang__port_control__3 :: ErlangFun
erlang__port_control__3 args = throw "unimplemented"

erlang__byte_size__1 :: ErlangFun
erlang__byte_size__1 args = throw "unimplemented"

erlang__check_process_code__2 :: ErlangFun
erlang__check_process_code__2 args = throw "unimplemented"

erlang__binary_to_list__1 :: ErlangFun
erlang__binary_to_list__1 args = throw "unimplemented"

erlang__is_number__1 :: ErlangFun
erlang__is_number__1 args = throw "unimplemented"

erlang__system_monitor__0 :: ErlangFun
erlang__system_monitor__0 args = throw "unimplemented"

erlang__phash2__2 :: ErlangFun
erlang__phash2__2 args = throw "unimplemented"

erlang__is_pid__1 :: ErlangFun
erlang__is_pid__1 args = throw "unimplemented"

erlang__floor__1 :: ErlangFun
erlang__floor__1 args = throw "unimplemented"

erlang__bitsize__1 :: ErlangFun
erlang__bitsize__1 args = throw "unimplemented"

erlang__list_to_binary__1 :: ErlangFun
erlang__list_to_binary__1 args = throw "unimplemented"

erlang__nodes__1 :: ErlangFun
erlang__nodes__1 args = throw "unimplemented"

erlang__term_to_binary__1 :: ErlangFun
erlang__term_to_binary__1 args = throw "unimplemented"

erlang__time__0 :: ErlangFun
erlang__time__0 args = throw "unimplemented"

erlang__time_offset__1 :: ErlangFun
erlang__time_offset__1 args = throw "unimplemented"

erlang__seq_trace_print__1 :: ErlangFun
erlang__seq_trace_print__1 args = throw "unimplemented"

erlang__send__2 :: ErlangFun
erlang__send__2 args = throw "unimplemented"

erlang__halt__1 :: ErlangFun
erlang__halt__1 args = throw "unimplemented"

erlang__spawn_opt__5 :: ErlangFun
erlang__spawn_opt__5 args = throw "unimplemented"

erlang__size__1 :: ErlangFun
erlang__size__1 args = throw "unimplemented"

erlang__process_info__1 :: ErlangFun
erlang__process_info__1 args = throw "unimplemented"

erlang__md5__1 :: ErlangFun
erlang__md5__1 args = throw "unimplemented"

erlang__binary_part__2 :: ErlangFun
erlang__binary_part__2 args = throw "unimplemented"

erlang__format_cpu_topology__1 :: ErlangFun
erlang__format_cpu_topology__1 args = throw "unimplemented"

erlang__spawn__1 :: ErlangFun
erlang__spawn__1 args = throw "unimplemented"

erlang__throw__1 :: ErlangFun
erlang__throw__1 args = throw "unimplemented"

erlang__float_to_list__2 :: ErlangFun
erlang__float_to_list__2 args = throw "unimplemented"

erlang__load_nif__2 :: ErlangFun
erlang__load_nif__2 args = throw "unimplemented"

erlang__prepare_loading__2 :: ErlangFun
erlang__prepare_loading__2 args = throw "unimplemented"

erlang__open_port__2 :: ErlangFun
erlang__open_port__2 args = throw "unimplemented"

erlang__term_to_binary__2 :: ErlangFun
erlang__term_to_binary__2 args = throw "unimplemented"

erlang__port_set_data__2 :: ErlangFun
erlang__port_set_data__2 args = throw "unimplemented"

erlang__tuple_to_list__1 :: ErlangFun
erlang__tuple_to_list__1 args = throw "unimplemented"

erlang__self__0 :: ErlangFun
erlang__self__0 args = throw "unimplemented"

erlang__read_timer__2 :: ErlangFun
erlang__read_timer__2 args = throw "unimplemented"

erlang__statistics__1 :: ErlangFun
erlang__statistics__1 args = throw "unimplemented"

erlang__max__2 :: ErlangFun
erlang__max__2 args = throw "unimplemented"

erlang__apply__2 :: ErlangFun
erlang__apply__2 args = throw "unimplemented"

erlang__nodes__0 :: ErlangFun
erlang__nodes__0 args = throw "unimplemented"

erlang__insert_element__3 :: ErlangFun
erlang__insert_element__3 args = throw "unimplemented"

erlang__binary_to_term__1 :: ErlangFun
erlang__binary_to_term__1 args = throw "unimplemented"

erlang__is_bitstring__1 :: ErlangFun
erlang__is_bitstring__1 args = throw "unimplemented"

erlang__bsr__2 :: ErlangFun
erlang__bsr__2 args = throw "unimplemented"

erlang__list_to_integer__1 :: ErlangFun
erlang__list_to_integer__1 args = throw "unimplemented"

erlang__spawn__3 :: ErlangFun
erlang__spawn__3 args = throw "unimplemented"

erlang__send_after__4 :: ErlangFun
erlang__send_after__4 args = throw "unimplemented"

erlang__trace__3 :: ErlangFun
erlang__trace__3 args = throw "unimplemented"

erlang__adler32__1 :: ErlangFun
erlang__adler32__1 args = throw "unimplemented"

erlang__send_nosuspend__2 :: ErlangFun
erlang__send_nosuspend__2 args = throw "unimplemented"

erlang__dt_get_tag_data__0 :: ErlangFun
erlang__dt_get_tag_data__0 args = throw "unimplemented"

erlang__resume_process__1 :: ErlangFun
erlang__resume_process__1 args = throw "unimplemented"

erlang__rem__2 :: ErlangFun
erlang__rem__2 args = throw "unimplemented"

erlang__port_close__1 :: ErlangFun
erlang__port_close__1 args = throw "unimplemented"

erlang__is_function__1 :: ErlangFun
erlang__is_function__1 args = throw "unimplemented"

erlang__port_call__2 :: ErlangFun
erlang__port_call__2 args = throw "unimplemented"

erlang__dt_get_tag__0 :: ErlangFun
erlang__dt_get_tag__0 args = throw "unimplemented"

erlang__universaltime_to_posixtime__1 :: ErlangFun
erlang__universaltime_to_posixtime__1 args = throw "unimplemented"

erlang__append__2 :: ErlangFun
erlang__append__2 args = throw "unimplemented"

erlang__md5_init__0 :: ErlangFun
erlang__md5_init__0 args = throw "unimplemented"

erlang__list_to_port__1 :: ErlangFun
erlang__list_to_port__1 args = throw "unimplemented"

erlang__link__1 :: ErlangFun
erlang__link__1 args = throw "unimplemented"

erlang__spawn_opt__1 :: ErlangFun
erlang__spawn_opt__1 args = throw "unimplemented"

erlang__binary_to_float__1 :: ErlangFun
erlang__binary_to_float__1 args = throw "unimplemented"

erlang__monitor_node__2 :: ErlangFun
erlang__monitor_node__2 args = throw "unimplemented"

erlang__time_offset__0 :: ErlangFun
erlang__time_offset__0 args = throw "unimplemented"

erlang__port_connect__2 :: ErlangFun
erlang__port_connect__2 args = throw "unimplemented"

erlang__setelement__3 :: ErlangFun
erlang__setelement__3 args = throw "unimplemented"

erlang__gather_gc_info_result__1 :: ErlangFun
erlang__gather_gc_info_result__1 args = throw "unimplemented"

erlang__tuple_size__1 :: ErlangFun
erlang__tuple_size__1 args = throw "unimplemented"

erlang__system_monitor__1 :: ErlangFun
erlang__system_monitor__1 args = throw "unimplemented"

erlang__timestamp__0 :: ErlangFun
erlang__timestamp__0 args = throw "unimplemented"

erlang__system_time__1 :: ErlangFun
erlang__system_time__1 args = throw "unimplemented"

erlang__register__2 :: ErlangFun
erlang__register__2 args = throw "unimplemented"

erlang__dmonitor_node__3 :: ErlangFun
erlang__dmonitor_node__3 args = throw "unimplemented"

erlang__fun_info__2 :: ErlangFun
erlang__fun_info__2 args = throw "unimplemented"

erlang__finish_after_on_load__2 :: ErlangFun
erlang__finish_after_on_load__2 args = throw "unimplemented"

erlang__tl__1 :: ErlangFun
erlang__tl__1 args = throw "unimplemented"

erlang__send_after__3 :: ErlangFun
erlang__send_after__3 args = throw "unimplemented"

erlang__trace_pattern__2 :: ErlangFun
erlang__trace_pattern__2 args = throw "unimplemented"

erlang__port_command__3 :: ErlangFun
erlang__port_command__3 args = throw "unimplemented"

erlang__alloc_info__1 :: ErlangFun
erlang__alloc_info__1 args = throw "unimplemented"

erlang__list_to_ref__1 :: ErlangFun
erlang__list_to_ref__1 args = throw "unimplemented"

erlang__port_command__2 :: ErlangFun
erlang__port_command__2 args = throw "unimplemented"

erlang__external_size__1 :: ErlangFun
erlang__external_size__1 args = throw "unimplemented"

erlang__atom_to_binary__2 :: ErlangFun
erlang__atom_to_binary__2 args = throw "unimplemented"

erlang__spawn_opt__3 :: ErlangFun
erlang__spawn_opt__3 args = throw "unimplemented"

erlang__exit_signal__2 :: ErlangFun
erlang__exit_signal__2 args = throw "unimplemented"

erlang__display_nl__0 :: ErlangFun
erlang__display_nl__0 args = throw "unimplemented"

erlang__append_element__2 :: ErlangFun
erlang__append_element__2 args = throw "unimplemented"

erlang__delete_element__2 :: ErlangFun
erlang__delete_element__2 args = throw "unimplemented"

erlang__xor__2 :: ErlangFun
erlang__xor__2 args = throw "unimplemented"

erlang__is_reference__1 :: ErlangFun
erlang__is_reference__1 args = throw "unimplemented"

erlang__round__1 :: ErlangFun
erlang__round__1 args = throw "unimplemented"

erlang__crc32__2 :: ErlangFun
erlang__crc32__2 args = throw "unimplemented"

erlang__not__1 :: ErlangFun
erlang__not__1 args = throw "unimplemented"

erlang__adler32__2 :: ErlangFun
erlang__adler32__2 args = throw "unimplemented"

erlang__md5_final__1 :: ErlangFun
erlang__md5_final__1 args = throw "unimplemented"

erlang__monitor_node__3 :: ErlangFun
erlang__monitor_node__3 args = throw "unimplemented"

erlang__monotonic_time__0 :: ErlangFun
erlang__monotonic_time__0 args = throw "unimplemented"

erlang__length__1 :: ErlangFun
erlang__length__1 args = throw "unimplemented"

erlang__nif_error__1 :: ErlangFun
erlang__nif_error__1 args = throw "unimplemented"

erlang__check_process_code__3 :: ErlangFun
erlang__check_process_code__3 args = throw "unimplemented"

erlang__localtime__0 :: ErlangFun
erlang__localtime__0 args = throw "unimplemented"

erlang__trace_delivered__1 :: ErlangFun
erlang__trace_delivered__1 args = throw "unimplemented"

erlang__module_info__0 :: ErlangFun
erlang__module_info__0 args = throw "unimplemented"

erlang__spawn__2 :: ErlangFun
erlang__spawn__2 args = throw "unimplemented"

erlang__set_cookie__2 :: ErlangFun
erlang__set_cookie__2 args = throw "unimplemented"

erlang__seq_trace_print__2 :: ErlangFun
erlang__seq_trace_print__2 args = throw "unimplemented"

erlang__suspend_process__2 :: ErlangFun
erlang__suspend_process__2 args = throw "unimplemented"

erlang__crc32_combine__3 :: ErlangFun
erlang__crc32_combine__3 args = throw "unimplemented"

erlang__process_info__2 :: ErlangFun
erlang__process_info__2 args = throw "unimplemented"

erlang__unique_integer__0 :: ErlangFun
erlang__unique_integer__0 args = throw "unimplemented"

erlang__system_time__0 :: ErlangFun
erlang__system_time__0 args = throw "unimplemented"

erlang__yield__0 :: ErlangFun
erlang__yield__0 args = throw "unimplemented"

erlang__posixtime_to_universaltime__1 :: ErlangFun
erlang__posixtime_to_universaltime__1 args = throw "unimplemented"

erlang__start_timer__3 :: ErlangFun
erlang__start_timer__3 args = throw "unimplemented"

erlang__float_to_binary__1 :: ErlangFun
erlang__float_to_binary__1 args = throw "unimplemented"

erlang__erase__1 :: ErlangFun
erlang__erase__1 args = throw "unimplemented"

erlang__port_info__1 :: ErlangFun
erlang__port_info__1 args = throw "unimplemented"

erlang__binary_to_integer__1 :: ErlangFun
erlang__binary_to_integer__1 args = throw "unimplemented"

erlang__process_flag__2 :: ErlangFun
erlang__process_flag__2 args = throw "unimplemented"

erlang__monotonic_time__1 :: ErlangFun
erlang__monotonic_time__1 args = throw "unimplemented"

erlang__seq_trace_info__1 :: ErlangFun
erlang__seq_trace_info__1 args = throw "unimplemented"

erlang__memory__1 :: ErlangFun
erlang__memory__1 args = throw "unimplemented"

erlang__get_module_info__2 :: ErlangFun
erlang__get_module_info__2 args = throw "unimplemented"

erlang__seq_trace__2 :: ErlangFun
erlang__seq_trace__2 args = throw "unimplemented"

erlang__float__1 :: ErlangFun
erlang__float__1 args = throw "unimplemented"

erlang__disconnect_node__1 :: ErlangFun
erlang__disconnect_node__1 args = throw "unimplemented"

erlang__setnode__2 :: ErlangFun
erlang__setnode__2 args = throw "unimplemented"

erlang__bsl__2 :: ErlangFun
erlang__bsl__2 args = throw "unimplemented"

erlang__atom_to_list__1 :: ErlangFun
erlang__atom_to_list__1 args = throw "unimplemented"

erlang__get_keys__0 :: ErlangFun
erlang__get_keys__0 args = throw "unimplemented"

erlang__is_list__1 :: ErlangFun
erlang__is_list__1 [ErlangEmptyList] = pure $ boolToTerm true
erlang__is_list__1 [ErlangCons _ _] = pure $ boolToTerm true
erlang__is_list__1 [_] = pure $ boolToTerm false

erlang__node__0 :: ErlangFun
erlang__node__0 args = throw "unimplemented"

erlang__fun_info__1 :: ErlangFun
erlang__fun_info__1 args = throw "unimplemented"

erlang__exit__1 :: ErlangFun
erlang__exit__1 args = throw "unimplemented"

erlang__system_info__1 :: ErlangFun
erlang__system_info__1 args = throw "unimplemented"

erlang__binary_part__3 :: ErlangFun
erlang__binary_part__3 args = throw "unimplemented"

erlang__is_map__1 :: ErlangFun
erlang__is_map__1 args = throw "unimplemented"

erlang__is_map_key__2 :: ErlangFun
erlang__is_map_key__2 args = throw "unimplemented"

erlang__halt__2 :: ErlangFun
erlang__halt__2 args = throw "unimplemented"

erlang__localtime_to_universaltime__2 :: ErlangFun
erlang__localtime_to_universaltime__2 args = throw "unimplemented"

erlang__cancel_timer__2 :: ErlangFun
erlang__cancel_timer__2 args = throw "unimplemented"

erlang__display__1 :: ErlangFun
erlang__display__1 args = throw "unimplemented"

erlang__load_module__2 :: ErlangFun
erlang__load_module__2 args = throw "unimplemented"

erlang__dt_append_vm_tag_data__1 :: ErlangFun
erlang__dt_append_vm_tag_data__1 args = throw "unimplemented"

erlang__port_info__2 :: ErlangFun
erlang__port_info__2 args = throw "unimplemented"

erlang__get__0 :: ErlangFun
erlang__get__0 args = throw "unimplemented"

erlang__unregister__1 :: ErlangFun
erlang__unregister__1 args = throw "unimplemented"

erlang__integer_to_list__1 :: ErlangFun
erlang__integer_to_list__1 args = throw "unimplemented"

erlang__dist_ctrl_input_handler__2 :: ErlangFun
erlang__dist_ctrl_input_handler__2 args = throw "unimplemented"

erlang__trunc__1 :: ErlangFun
erlang__trunc__1 args = throw "unimplemented"

erlang__fun_to_list__1 :: ErlangFun
erlang__fun_to_list__1 args = throw "unimplemented"

erlang__crasher__6 :: ErlangFun
erlang__crasher__6 args = throw "unimplemented"

erlang__read_timer__1 :: ErlangFun
erlang__read_timer__1 args = throw "unimplemented"

erlang__module_loaded__1 :: ErlangFun
erlang__module_loaded__1 args = throw "unimplemented"

erlang__md5_update__2 :: ErlangFun
erlang__md5_update__2 args = throw "unimplemented"

erlang__localtime_to_universaltime__1 :: ErlangFun
erlang__localtime_to_universaltime__1 args = throw "unimplemented"

erlang__spawn_opt__4 :: ErlangFun
erlang__spawn_opt__4 args = throw "unimplemented"

erlang__group_leader__0 :: ErlangFun
erlang__group_leader__0 args = throw "unimplemented"

erlang__dt_put_tag__1 :: ErlangFun
erlang__dt_put_tag__1 args = throw "unimplemented"

erlang__phash2__1 :: ErlangFun
erlang__phash2__1 args = throw "unimplemented"

erlang__spawn__4 :: ErlangFun
erlang__spawn__4 args = throw "unimplemented"

erlang__crc32__1 :: ErlangFun
erlang__crc32__1 args = throw "unimplemented"

erlang__band__2 :: ErlangFun
erlang__band__2 args = throw "unimplemented"

erlang__send__3 :: ErlangFun
erlang__send__3 args = throw "unimplemented"

erlang__is_function__2 :: ErlangFun
erlang__is_function__2 args = throw "unimplemented"

erlang__system_monitor__2 :: ErlangFun
erlang__system_monitor__2 args = throw "unimplemented"

erlang__spawn_monitor__3 :: ErlangFun
erlang__spawn_monitor__3 args = throw "unimplemented"

erlang__list_to_pid__1 :: ErlangFun
erlang__list_to_pid__1 args = throw "unimplemented"

erlang__get_keys__1 :: ErlangFun
erlang__get_keys__1 args = throw "unimplemented"

erlang__demonitor__2 :: ErlangFun
erlang__demonitor__2 args = throw "unimplemented"

erlang__raise__3 :: ErlangFun
erlang__raise__3 args = throw "unimplemented"

erlang__hibernate__3 :: ErlangFun
erlang__hibernate__3 args = throw "unimplemented"

erlang__adler32_combine__3 :: ErlangFun
erlang__adler32_combine__3 args = throw "unimplemented"

erlang__node__1 :: ErlangFun
erlang__node__1 args = throw "unimplemented"

erlang__monitor__2 :: ErlangFun
erlang__monitor__2 args = throw "unimplemented"

erlang__start_timer__4 :: ErlangFun
erlang__start_timer__4 args = throw "unimplemented"

erlang__bit_size__1 :: ErlangFun
erlang__bit_size__1 args = throw "unimplemented"

erlang__call_on_load_function__1 :: ErlangFun
erlang__call_on_load_function__1 args = throw "unimplemented"

erlang__processes__0 :: ErlangFun
erlang__processes__0 args = throw "unimplemented"

erlang__error__2 :: ErlangFun
erlang__error__2 args = throw "unimplemented"

erlang__loaded__0 :: ErlangFun
erlang__loaded__0 args = throw "unimplemented"

erlang__bump_reductions__1 :: ErlangFun
erlang__bump_reductions__1 args = throw "unimplemented"

erlang__send_nosuspend__3 :: ErlangFun
erlang__send_nosuspend__3 args = throw "unimplemented"

erlang__make_tuple__3 :: ErlangFun
erlang__make_tuple__3 args = throw "unimplemented"

erlang__unlink__1 :: ErlangFun
erlang__unlink__1 args = throw "unimplemented"

erlang__demonitor__1 :: ErlangFun
erlang__demonitor__1 args = throw "unimplemented"

erlang__trace_info__2 :: ErlangFun
erlang__trace_info__2 args = throw "unimplemented"

erlang__delete_module__1 :: ErlangFun
erlang__delete_module__1 args = throw "unimplemented"

erlang__garbage_collect__1 :: ErlangFun
erlang__garbage_collect__1 args = throw "unimplemented"

erlang__check_old_code__1 :: ErlangFun
erlang__check_old_code__1 args = throw "unimplemented"

erlang__spawn_monitor__1 :: ErlangFun
erlang__spawn_monitor__1 args = throw "unimplemented"

erlang__min__2 :: ErlangFun
erlang__min__2 args = throw "unimplemented"

erlang__float_to_binary__2 :: ErlangFun
erlang__float_to_binary__2 args = throw "unimplemented"

erlang__system_profile__2 :: ErlangFun
erlang__system_profile__2 args = throw "unimplemented"

erlang__error__1 :: ErlangFun
erlang__error__1 args = throw "unimplemented"

erlang__delay_trap__2 :: ErlangFun
erlang__delay_trap__2 args = throw "unimplemented"

erlang__spawn_link__4 :: ErlangFun
erlang__spawn_link__4 args = throw "unimplemented"

erlang__is_record__3 :: ErlangFun
erlang__is_record__3 args = throw "unimplemented"

erlang__memory__0 :: ErlangFun
erlang__memory__0 args = throw "unimplemented"

erlang__halt__0 :: ErlangFun
erlang__halt__0 args = throw "unimplemented"

erlang__process_flag__3 :: ErlangFun
erlang__process_flag__3 args = throw "unimplemented"

erlang__set_cpu_topology__1 :: ErlangFun
erlang__set_cpu_topology__1 args = throw "unimplemented"

erlang__list_to_tuple__1 :: ErlangFun
erlang__list_to_tuple__1 args = throw "unimplemented"

erlang__universaltime__0 :: ErlangFun
erlang__universaltime__0 args = throw "unimplemented"

erlang__bitstring_to_list__1 :: ErlangFun
erlang__bitstring_to_list__1 args = throw "unimplemented"
