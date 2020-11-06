module Erlang.Builtins where

import Erlang.Type
import Prelude
import Data.Maybe as DM
import Data.Array as DA
import Data.List as DL
import Control.Monad
import Effect.Exception (throw)
import Effect

-- erlang''op_plus :: ErlangFun
-- erlang''op_plus [ErlangNum x, ErlangNum y] = pure $ ErlangNum (x + y)

-- erlang''op_minus :: ErlangFun
-- erlang''op_minus [ErlangNum x, ErlangNum y] = pure $ ErlangNum (x - y)

-- erlang''op_mult :: ErlangFun
-- erlang''op_mult [ErlangNum x, ErlangNum y] = pure $ ErlangNum (x * y)

-- erlang''op_div :: ErlangFun
-- erlang''op_div [ErlangNum x, ErlangNum y] = pure $ ErlangNum (x / y)

-- erlang''apply''2 :: ErlangFun
-- erlang''apply''2 [ErlangFun arity@(ErlangNum arityVal) fun, args]
--   | DM.Just argsL <- erlangListToList args
--   , DL.length argsL == arityVal =
--     fun args

-- lists''reverse''2 :: ErlangFun
-- lists''reverse''2 [ErlangEmptyList, acc] = pure acc
-- lists''reverse''2 [ErlangCons h t, acc] =
--   lists''reverse''2 [t, ErlangCons h acc]

-- erlang''length''1 :: ErlangFun
-- erlang''length''1 [l] = pure $ ErlangNum (go 0 l) where
--   go acc ErlangEmptyList = acc
--   go acc (ErlangCons _ t) = go (acc + 1) t


lists''keysearch''3 :: ErlangFun
lists''keysearch''3 args = throw "unimplemented"

lists''keymember''3 :: ErlangFun
lists''keymember''3 args = throw "unimplemented"

lists''reverse''2 :: ErlangFun
lists''reverse''2 args = throw "unimplemented"

lists''member''2 :: ErlangFun
lists''member''2 args = throw "unimplemented"

lists''keyfind''3 :: ErlangFun
lists''keyfind''3 args = throw "unimplemented"


erlang''process_display''2 :: ErlangFun
erlang''process_display''2 args = throw "unimplemented"

erlang''integer_to_binary''2 :: ErlangFun
erlang''integer_to_binary''2 args = throw "unimplemented"

erlang''integer_to_list''2 :: ErlangFun
erlang''integer_to_list''2 args = throw "unimplemented"

erlang''op_div :: ErlangFun
erlang''op_div args = throw "unimplemented"

erlang''fun_info_mfa''1 :: ErlangFun
erlang''fun_info_mfa''1 args = throw "unimplemented"

erlang''nif_error''2 :: ErlangFun
erlang''nif_error''2 args = throw "unimplemented"

erlang''get_stacktrace''0 :: ErlangFun
erlang''get_stacktrace''0 args = throw "unimplemented"

erlang''registered''0 :: ErlangFun
erlang''registered''0 args = throw "unimplemented"

erlang''op_exactNeq :: ErlangFun
erlang''op_exactNeq args = throw "unimplemented"

erlang''get_module_info''1 :: ErlangFun
erlang''get_module_info''1 args = throw "unimplemented"

erlang''module_info''1 :: ErlangFun
erlang''module_info''1 args = throw "unimplemented"

erlang''map_get''2 :: ErlangFun
erlang''map_get''2 args = throw "unimplemented"

erlang''cancel_timer''1 :: ErlangFun
erlang''cancel_timer''1 args = throw "unimplemented"

erlang''dist_ctrl_get_data_notification''1 :: ErlangFun
erlang''dist_ctrl_get_data_notification''1 args = throw "unimplemented"

erlang''list_to_float''1 :: ErlangFun
erlang''list_to_float''1 args = throw "unimplemented"

erlang''apply''3 :: ErlangFun
erlang''apply''3 args = throw "unimplemented"

erlang''is_builtin''3 :: ErlangFun
erlang''is_builtin''3 args = throw "unimplemented"

erlang''list_to_integer''2 :: ErlangFun
erlang''list_to_integer''2 args = throw "unimplemented"

erlang''binary_to_atom''2 :: ErlangFun
erlang''binary_to_atom''2 args = throw "unimplemented"

erlang''suspend_process''1 :: ErlangFun
erlang''suspend_process''1 args = throw "unimplemented"

erlang''binary_to_term''2 :: ErlangFun
erlang''binary_to_term''2 args = throw "unimplemented"

erlang''spawn_link''2 :: ErlangFun
erlang''spawn_link''2 args = throw "unimplemented"

erlang''integer_to_binary''1 :: ErlangFun
erlang''integer_to_binary''1 args = throw "unimplemented"

erlang''get''1 :: ErlangFun
erlang''get''1 args = throw "unimplemented"

erlang''dist_ctrl_get_data''1 :: ErlangFun
erlang''dist_ctrl_get_data''1 args = throw "unimplemented"

erlang''setnode''3 :: ErlangFun
erlang''setnode''3 args = throw "unimplemented"

erlang''hd''1 :: ErlangFun
erlang''hd''1 args = throw "unimplemented"

erlang''now''0 :: ErlangFun
erlang''now''0 args = throw "unimplemented"

erlang''is_integer''1 :: ErlangFun
erlang''is_integer''1 args = throw "unimplemented"

erlang''erase''0 :: ErlangFun
erlang''erase''0 args = throw "unimplemented"

erlang''ports''0 :: ErlangFun
erlang''ports''0 args = throw "unimplemented"

erlang''dt_spread_tag''1 :: ErlangFun
erlang''dt_spread_tag''1 args = throw "unimplemented"

erlang''convert_time_unit''3 :: ErlangFun
erlang''convert_time_unit''3 args = throw "unimplemented"

erlang''iolist_to_iovec''1 :: ErlangFun
erlang''iolist_to_iovec''1 args = throw "unimplemented"

erlang''iolist_to_binary''1 :: ErlangFun
erlang''iolist_to_binary''1 args = throw "unimplemented"

erlang''decode_packet''3 :: ErlangFun
erlang''decode_packet''3 args = throw "unimplemented"

erlang''get_cookie''0 :: ErlangFun
erlang''get_cookie''0 args = throw "unimplemented"

erlang''put''2 :: ErlangFun
erlang''put''2 args = throw "unimplemented"

erlang''unique_integer''1 :: ErlangFun
erlang''unique_integer''1 args = throw "unimplemented"

erlang''exit''2 :: ErlangFun
erlang''exit''2 args = throw "unimplemented"

erlang''purge_module''1 :: ErlangFun
erlang''purge_module''1 args = throw "unimplemented"

erlang''subtract''2 :: ErlangFun
erlang''subtract''2 args = throw "unimplemented"

erlang''dt_prepend_vm_tag_data''1 :: ErlangFun
erlang''dt_prepend_vm_tag_data''1 args = throw "unimplemented"

erlang''has_prepared_code_on_load''1 :: ErlangFun
erlang''has_prepared_code_on_load''1 args = throw "unimplemented"

erlang''external_size''2 :: ErlangFun
erlang''external_size''2 args = throw "unimplemented"

erlang''is_alive''0 :: ErlangFun
erlang''is_alive''0 args = throw "unimplemented"

erlang''make_tuple''2 :: ErlangFun
erlang''make_tuple''2 args = throw "unimplemented"

erlang''is_port''1 :: ErlangFun
erlang''is_port''1 args = throw "unimplemented"

erlang''and''2 :: ErlangFun
erlang''and''2 args = throw "unimplemented"

erlang''is_process_alive''1 :: ErlangFun
erlang''is_process_alive''1 args = throw "unimplemented"

erlang''is_boolean''1 :: ErlangFun
erlang''is_boolean''1 args = throw "unimplemented"

erlang''is_record''2 :: ErlangFun
erlang''is_record''2 args = throw "unimplemented"

erlang''list_to_bitstring''1 :: ErlangFun
erlang''list_to_bitstring''1 args = throw "unimplemented"

erlang''pid_to_list''1 :: ErlangFun
erlang''pid_to_list''1 args = throw "unimplemented"

erlang''dist_get_stat''1 :: ErlangFun
erlang''dist_get_stat''1 args = throw "unimplemented"

erlang''binary_to_integer''2 :: ErlangFun
erlang''binary_to_integer''2 args = throw "unimplemented"

erlang''op_and :: ErlangFun
erlang''op_and args = throw "unimplemented"

erlang''alloc_sizes''1 :: ErlangFun
erlang''alloc_sizes''1 args = throw "unimplemented"

erlang''spawn_opt''2 :: ErlangFun
erlang''spawn_opt''2 args = throw "unimplemented"

erlang''iolist_size''1 :: ErlangFun
erlang''iolist_size''1 args = throw "unimplemented"

erlang''element''2 :: ErlangFun
erlang''element''2 args = throw "unimplemented"

erlang''port_get_data''1 :: ErlangFun
erlang''port_get_data''1 args = throw "unimplemented"

erlang''group_leader''2 :: ErlangFun
erlang''group_leader''2 args = throw "unimplemented"

erlang''split_binary''2 :: ErlangFun
erlang''split_binary''2 args = throw "unimplemented"

erlang''function_exported''3 :: ErlangFun
erlang''function_exported''3 args = throw "unimplemented"

erlang''list_to_existing_atom''1 :: ErlangFun
erlang''list_to_existing_atom''1 args = throw "unimplemented"

erlang''phash''2 :: ErlangFun
erlang''phash''2 args = throw "unimplemented"

erlang''dist_ctrl_put_data''2 :: ErlangFun
erlang''dist_ctrl_put_data''2 args = throw "unimplemented"

erlang''garbage_collect_message_area''0 :: ErlangFun
erlang''garbage_collect_message_area''0 args = throw "unimplemented"

erlang''is_binary''1 :: ErlangFun
erlang''is_binary''1 args = throw "unimplemented"

erlang''bor''2 :: ErlangFun
erlang''bor''2 args = throw "unimplemented"

erlang''spawn_link''1 :: ErlangFun
erlang''spawn_link''1 args = throw "unimplemented"

erlang''is_tuple''1 :: ErlangFun
erlang''is_tuple''1 args = throw "unimplemented"

erlang''op_lesser :: ErlangFun
erlang''op_lesser [ErlangNum x, ErlangNum y] =
  pure (if x < y then ErlangAtom "true" else ErlangAtom "false")

erlang''bnot''1 :: ErlangFun
erlang''bnot''1 args = throw "unimplemented"

erlang''is_atom''1 :: ErlangFun
erlang''is_atom''1 args = throw "unimplemented"

erlang''bxor''2 :: ErlangFun
erlang''bxor''2 args = throw "unimplemented"

erlang''garbage_collect''0 :: ErlangFun
erlang''garbage_collect''0 args = throw "unimplemented"

erlang''trace_pattern''3 :: ErlangFun
erlang''trace_pattern''3 args = throw "unimplemented"

erlang''binary_to_existing_atom''2 :: ErlangFun
erlang''binary_to_existing_atom''2 args = throw "unimplemented"

erlang''dt_restore_tag''1 :: ErlangFun
erlang''dt_restore_tag''1 args = throw "unimplemented"

erlang''port_to_list''1 :: ErlangFun
erlang''port_to_list''1 args = throw "unimplemented"

erlang''system_profile''0 :: ErlangFun
erlang''system_profile''0 args = throw "unimplemented"

erlang''match_spec_test''3 :: ErlangFun
erlang''match_spec_test''3 args = throw "unimplemented"

erlang''ceil''1 :: ErlangFun
erlang''ceil''1 args = throw "unimplemented"

erlang''float_to_list''1 :: ErlangFun
erlang''float_to_list''1 args = throw "unimplemented"

erlang''pre_loaded''0 :: ErlangFun
erlang''pre_loaded''0 args = throw "unimplemented"

erlang''display_string''1 :: ErlangFun
erlang''display_string''1 args = throw "unimplemented"

erlang''finish_loading''1 :: ErlangFun
erlang''finish_loading''1 args = throw "unimplemented"

erlang''op_mult :: ErlangFun
erlang''op_mult args = throw "unimplemented"

erlang''spawn_link''3 :: ErlangFun
erlang''spawn_link''3 args = throw "unimplemented"

erlang''abs''1 :: ErlangFun
erlang''abs''1 args = throw "unimplemented"

erlang''binary_to_list''3 :: ErlangFun
erlang''binary_to_list''3 args = throw "unimplemented"

erlang''garbage_collect''2 :: ErlangFun
erlang''garbage_collect''2 args = throw "unimplemented"

erlang''system_flag''2 :: ErlangFun
erlang''system_flag''2 args = throw "unimplemented"

erlang''make_fun''3 :: ErlangFun
erlang''make_fun''3 args = throw "unimplemented"

erlang''map_size''1 :: ErlangFun
erlang''map_size''1 args = throw "unimplemented"

erlang''universaltime_to_localtime''1 :: ErlangFun
erlang''universaltime_to_localtime''1 args = throw "unimplemented"

erlang''whereis''1 :: ErlangFun
erlang''whereis''1 args = throw "unimplemented"

erlang''list_to_atom''1 :: ErlangFun
erlang''list_to_atom''1 args = throw "unimplemented"

erlang''port_call''3 :: ErlangFun
erlang''port_call''3 args = throw "unimplemented"

erlang''is_float''1 :: ErlangFun
erlang''is_float''1 args = throw "unimplemented"

erlang''date''0 :: ErlangFun
erlang''date''0 args = throw "unimplemented"

erlang''make_ref''0 :: ErlangFun
erlang''make_ref''0 args = throw "unimplemented"

erlang''or''2 :: ErlangFun
erlang''or''2 args = throw "unimplemented"

erlang''ref_to_list''1 :: ErlangFun
erlang''ref_to_list''1 args = throw "unimplemented"

erlang''port_control''3 :: ErlangFun
erlang''port_control''3 args = throw "unimplemented"

erlang''byte_size''1 :: ErlangFun
erlang''byte_size''1 args = throw "unimplemented"

erlang''check_process_code''2 :: ErlangFun
erlang''check_process_code''2 args = throw "unimplemented"

erlang''binary_to_list''1 :: ErlangFun
erlang''binary_to_list''1 args = throw "unimplemented"

erlang''is_number''1 :: ErlangFun
erlang''is_number''1 args = throw "unimplemented"

erlang''system_monitor''0 :: ErlangFun
erlang''system_monitor''0 args = throw "unimplemented"

erlang''phash2''2 :: ErlangFun
erlang''phash2''2 args = throw "unimplemented"

erlang''is_pid''1 :: ErlangFun
erlang''is_pid''1 args = throw "unimplemented"

erlang''floor''1 :: ErlangFun
erlang''floor''1 args = throw "unimplemented"

erlang''bitsize''1 :: ErlangFun
erlang''bitsize''1 args = throw "unimplemented"

erlang''list_to_binary''1 :: ErlangFun
erlang''list_to_binary''1 args = throw "unimplemented"

erlang''nodes''1 :: ErlangFun
erlang''nodes''1 args = throw "unimplemented"

erlang''term_to_binary''1 :: ErlangFun
erlang''term_to_binary''1 args = throw "unimplemented"

erlang''time''0 :: ErlangFun
erlang''time''0 args = throw "unimplemented"

erlang''time_offset''1 :: ErlangFun
erlang''time_offset''1 args = throw "unimplemented"

erlang''seq_trace_print''1 :: ErlangFun
erlang''seq_trace_print''1 args = throw "unimplemented"

erlang''send''2 :: ErlangFun
erlang''send''2 args = throw "unimplemented"

erlang''halt''1 :: ErlangFun
erlang''halt''1 args = throw "unimplemented"

erlang''spawn_opt''5 :: ErlangFun
erlang''spawn_opt''5 args = throw "unimplemented"

erlang''size''1 :: ErlangFun
erlang''size''1 args = throw "unimplemented"

erlang''process_info''1 :: ErlangFun
erlang''process_info''1 args = throw "unimplemented"

erlang''md5''1 :: ErlangFun
erlang''md5''1 args = throw "unimplemented"

erlang''binary_part''2 :: ErlangFun
erlang''binary_part''2 args = throw "unimplemented"

erlang''format_cpu_topology''1 :: ErlangFun
erlang''format_cpu_topology''1 args = throw "unimplemented"

erlang''spawn''1 :: ErlangFun
erlang''spawn''1 args = throw "unimplemented"

erlang''throw''1 :: ErlangFun
erlang''throw''1 args = throw "unimplemented"

erlang''op_exactEq :: ErlangFun
erlang''op_exactEq args = throw "unimplemented"

erlang''float_to_list''2 :: ErlangFun
erlang''float_to_list''2 args = throw "unimplemented"

erlang''load_nif''2 :: ErlangFun
erlang''load_nif''2 args = throw "unimplemented"

erlang''prepare_loading''2 :: ErlangFun
erlang''prepare_loading''2 args = throw "unimplemented"

erlang''open_port''2 :: ErlangFun
erlang''open_port''2 args = throw "unimplemented"

erlang''term_to_binary''2 :: ErlangFun
erlang''term_to_binary''2 args = throw "unimplemented"

erlang''port_set_data''2 :: ErlangFun
erlang''port_set_data''2 args = throw "unimplemented"

erlang''tuple_to_list''1 :: ErlangFun
erlang''tuple_to_list''1 args = throw "unimplemented"

erlang''self''0 :: ErlangFun
erlang''self''0 args = throw "unimplemented"

erlang''read_timer''2 :: ErlangFun
erlang''read_timer''2 args = throw "unimplemented"

erlang''statistics''1 :: ErlangFun
erlang''statistics''1 args = throw "unimplemented"

erlang''max''2 :: ErlangFun
erlang''max''2 args = throw "unimplemented"

erlang''apply''2 :: ErlangFun
erlang''apply''2 args = throw "unimplemented"

erlang''nodes''0 :: ErlangFun
erlang''nodes''0 args = throw "unimplemented"

erlang''op_minus :: ErlangFun
erlang''op_minus args = throw "unimplemented"

erlang''insert_element''3 :: ErlangFun
erlang''insert_element''3 args = throw "unimplemented"

erlang''binary_to_term''1 :: ErlangFun
erlang''binary_to_term''1 args = throw "unimplemented"

erlang''is_bitstring''1 :: ErlangFun
erlang''is_bitstring''1 args = throw "unimplemented"

erlang''bsr''2 :: ErlangFun
erlang''bsr''2 args = throw "unimplemented"

erlang''list_to_integer''1 :: ErlangFun
erlang''list_to_integer''1 args = throw "unimplemented"

erlang''spawn''3 :: ErlangFun
erlang''spawn''3 args = throw "unimplemented"

erlang''send_after''4 :: ErlangFun
erlang''send_after''4 args = throw "unimplemented"

erlang''trace''3 :: ErlangFun
erlang''trace''3 args = throw "unimplemented"

erlang''adler32''1 :: ErlangFun
erlang''adler32''1 args = throw "unimplemented"

erlang''send_nosuspend''2 :: ErlangFun
erlang''send_nosuspend''2 args = throw "unimplemented"

erlang''dt_get_tag_data''0 :: ErlangFun
erlang''dt_get_tag_data''0 args = throw "unimplemented"

erlang''resume_process''1 :: ErlangFun
erlang''resume_process''1 args = throw "unimplemented"

erlang''rem''2 :: ErlangFun
erlang''rem''2 args = throw "unimplemented"

erlang''op_plus :: ErlangFun
erlang''op_plus args = throw "unimplemented"

erlang''port_close''1 :: ErlangFun
erlang''port_close''1 args = throw "unimplemented"

erlang''is_function''1 :: ErlangFun
erlang''is_function''1 args = throw "unimplemented"

erlang''port_call''2 :: ErlangFun
erlang''port_call''2 args = throw "unimplemented"

erlang''dt_get_tag''0 :: ErlangFun
erlang''dt_get_tag''0 args = throw "unimplemented"

erlang''universaltime_to_posixtime''1 :: ErlangFun
erlang''universaltime_to_posixtime''1 args = throw "unimplemented"

erlang''append''2 :: ErlangFun
erlang''append''2 args = throw "unimplemented"

erlang''md5_init''0 :: ErlangFun
erlang''md5_init''0 args = throw "unimplemented"

erlang''list_to_port''1 :: ErlangFun
erlang''list_to_port''1 args = throw "unimplemented"

erlang''link''1 :: ErlangFun
erlang''link''1 args = throw "unimplemented"

erlang''spawn_opt''1 :: ErlangFun
erlang''spawn_opt''1 args = throw "unimplemented"

erlang''binary_to_float''1 :: ErlangFun
erlang''binary_to_float''1 args = throw "unimplemented"

erlang''monitor_node''2 :: ErlangFun
erlang''monitor_node''2 args = throw "unimplemented"

erlang''time_offset''0 :: ErlangFun
erlang''time_offset''0 args = throw "unimplemented"

erlang''port_connect''2 :: ErlangFun
erlang''port_connect''2 args = throw "unimplemented"

erlang''setelement''3 :: ErlangFun
erlang''setelement''3 args = throw "unimplemented"

erlang''gather_gc_info_result''1 :: ErlangFun
erlang''gather_gc_info_result''1 args = throw "unimplemented"

erlang''tuple_size''1 :: ErlangFun
erlang''tuple_size''1 args = throw "unimplemented"

erlang''system_monitor''1 :: ErlangFun
erlang''system_monitor''1 args = throw "unimplemented"

erlang''timestamp''0 :: ErlangFun
erlang''timestamp''0 args = throw "unimplemented"

erlang''system_time''1 :: ErlangFun
erlang''system_time''1 args = throw "unimplemented"

erlang''register''2 :: ErlangFun
erlang''register''2 args = throw "unimplemented"

erlang''dmonitor_node''3 :: ErlangFun
erlang''dmonitor_node''3 args = throw "unimplemented"

erlang''fun_info''2 :: ErlangFun
erlang''fun_info''2 args = throw "unimplemented"

erlang''finish_after_on_load''2 :: ErlangFun
erlang''finish_after_on_load''2 args = throw "unimplemented"

erlang''tl''1 :: ErlangFun
erlang''tl''1 args = throw "unimplemented"

erlang''send_after''3 :: ErlangFun
erlang''send_after''3 args = throw "unimplemented"

erlang''trace_pattern''2 :: ErlangFun
erlang''trace_pattern''2 args = throw "unimplemented"

erlang''port_command''3 :: ErlangFun
erlang''port_command''3 args = throw "unimplemented"

erlang''alloc_info''1 :: ErlangFun
erlang''alloc_info''1 args = throw "unimplemented"

erlang''list_to_ref''1 :: ErlangFun
erlang''list_to_ref''1 args = throw "unimplemented"

erlang''port_command''2 :: ErlangFun
erlang''port_command''2 args = throw "unimplemented"

erlang''external_size''1 :: ErlangFun
erlang''external_size''1 args = throw "unimplemented"

erlang''atom_to_binary''2 :: ErlangFun
erlang''atom_to_binary''2 args = throw "unimplemented"

erlang''spawn_opt''3 :: ErlangFun
erlang''spawn_opt''3 args = throw "unimplemented"

erlang''exit_signal''2 :: ErlangFun
erlang''exit_signal''2 args = throw "unimplemented"

erlang''display_nl''0 :: ErlangFun
erlang''display_nl''0 args = throw "unimplemented"

erlang''append_element''2 :: ErlangFun
erlang''append_element''2 args = throw "unimplemented"

erlang''delete_element''2 :: ErlangFun
erlang''delete_element''2 args = throw "unimplemented"

erlang''xor''2 :: ErlangFun
erlang''xor''2 args = throw "unimplemented"

erlang''is_reference''1 :: ErlangFun
erlang''is_reference''1 args = throw "unimplemented"

erlang''round''1 :: ErlangFun
erlang''round''1 args = throw "unimplemented"

erlang''crc32''2 :: ErlangFun
erlang''crc32''2 args = throw "unimplemented"

erlang''not''1 :: ErlangFun
erlang''not''1 args = throw "unimplemented"

erlang''adler32''2 :: ErlangFun
erlang''adler32''2 args = throw "unimplemented"

erlang''md5_final''1 :: ErlangFun
erlang''md5_final''1 args = throw "unimplemented"

erlang''monitor_node''3 :: ErlangFun
erlang''monitor_node''3 args = throw "unimplemented"

erlang''monotonic_time''0 :: ErlangFun
erlang''monotonic_time''0 args = throw "unimplemented"

erlang''length''1 :: ErlangFun
erlang''length''1 args = throw "unimplemented"

erlang''nif_error''1 :: ErlangFun
erlang''nif_error''1 args = throw "unimplemented"

erlang''check_process_code''3 :: ErlangFun
erlang''check_process_code''3 args = throw "unimplemented"

erlang''localtime''0 :: ErlangFun
erlang''localtime''0 args = throw "unimplemented"

erlang''trace_delivered''1 :: ErlangFun
erlang''trace_delivered''1 args = throw "unimplemented"

erlang''module_info''0 :: ErlangFun
erlang''module_info''0 args = throw "unimplemented"

erlang''spawn''2 :: ErlangFun
erlang''spawn''2 args = throw "unimplemented"

erlang''set_cookie''2 :: ErlangFun
erlang''set_cookie''2 args = throw "unimplemented"

erlang''seq_trace_print''2 :: ErlangFun
erlang''seq_trace_print''2 args = throw "unimplemented"

erlang''suspend_process''2 :: ErlangFun
erlang''suspend_process''2 args = throw "unimplemented"

erlang''crc32_combine''3 :: ErlangFun
erlang''crc32_combine''3 args = throw "unimplemented"

erlang''process_info''2 :: ErlangFun
erlang''process_info''2 args = throw "unimplemented"

erlang''unique_integer''0 :: ErlangFun
erlang''unique_integer''0 args = throw "unimplemented"

erlang''system_time''0 :: ErlangFun
erlang''system_time''0 args = throw "unimplemented"

erlang''yield''0 :: ErlangFun
erlang''yield''0 args = throw "unimplemented"

erlang''posixtime_to_universaltime''1 :: ErlangFun
erlang''posixtime_to_universaltime''1 args = throw "unimplemented"

erlang''start_timer''3 :: ErlangFun
erlang''start_timer''3 args = throw "unimplemented"

erlang''float_to_binary''1 :: ErlangFun
erlang''float_to_binary''1 args = throw "unimplemented"

erlang''op_greaterEq :: ErlangFun
erlang''op_greaterEq args = throw "unimplemented"

erlang''op_or :: ErlangFun
erlang''op_or args = throw "unimplemented"

erlang''erase''1 :: ErlangFun
erlang''erase''1 args = throw "unimplemented"

erlang''port_info''1 :: ErlangFun
erlang''port_info''1 args = throw "unimplemented"

erlang''binary_to_integer''1 :: ErlangFun
erlang''binary_to_integer''1 args = throw "unimplemented"

erlang''process_flag''2 :: ErlangFun
erlang''process_flag''2 args = throw "unimplemented"

erlang''monotonic_time''1 :: ErlangFun
erlang''monotonic_time''1 args = throw "unimplemented"

erlang''seq_trace_info''1 :: ErlangFun
erlang''seq_trace_info''1 args = throw "unimplemented"

erlang''memory''1 :: ErlangFun
erlang''memory''1 args = throw "unimplemented"

erlang''get_module_info''2 :: ErlangFun
erlang''get_module_info''2 args = throw "unimplemented"

erlang''seq_trace''2 :: ErlangFun
erlang''seq_trace''2 args = throw "unimplemented"

erlang''float''1 :: ErlangFun
erlang''float''1 args = throw "unimplemented"

erlang''disconnect_node''1 :: ErlangFun
erlang''disconnect_node''1 args = throw "unimplemented"

erlang''setnode''2 :: ErlangFun
erlang''setnode''2 args = throw "unimplemented"

erlang''bsl''2 :: ErlangFun
erlang''bsl''2 args = throw "unimplemented"

erlang''atom_to_list''1 :: ErlangFun
erlang''atom_to_list''1 args = throw "unimplemented"

erlang''get_keys''0 :: ErlangFun
erlang''get_keys''0 args = throw "unimplemented"

erlang''is_list''1 :: ErlangFun
erlang''is_list''1 args = throw "unimplemented"

erlang''node''0 :: ErlangFun
erlang''node''0 args = throw "unimplemented"

erlang''fun_info''1 :: ErlangFun
erlang''fun_info''1 args = throw "unimplemented"

erlang''exit''1 :: ErlangFun
erlang''exit''1 args = throw "unimplemented"

erlang''system_info''1 :: ErlangFun
erlang''system_info''1 args = throw "unimplemented"

erlang''binary_part''3 :: ErlangFun
erlang''binary_part''3 args = throw "unimplemented"

erlang''is_map''1 :: ErlangFun
erlang''is_map''1 args = throw "unimplemented"

erlang''is_map_key''2 :: ErlangFun
erlang''is_map_key''2 args = throw "unimplemented"

erlang''halt''2 :: ErlangFun
erlang''halt''2 args = throw "unimplemented"

erlang''localtime_to_universaltime''2 :: ErlangFun
erlang''localtime_to_universaltime''2 args = throw "unimplemented"

erlang''cancel_timer''2 :: ErlangFun
erlang''cancel_timer''2 args = throw "unimplemented"

erlang''op_lesserEq :: ErlangFun
erlang''op_lesserEq args = throw "unimplemented"

erlang''display''1 :: ErlangFun
erlang''display''1 args = throw "unimplemented"

erlang''load_module''2 :: ErlangFun
erlang''load_module''2 args = throw "unimplemented"

erlang''dt_append_vm_tag_data''1 :: ErlangFun
erlang''dt_append_vm_tag_data''1 args = throw "unimplemented"

erlang''port_info''2 :: ErlangFun
erlang''port_info''2 args = throw "unimplemented"

erlang''get''0 :: ErlangFun
erlang''get''0 args = throw "unimplemented"

erlang''op_eq :: ErlangFun
erlang''op_eq args = throw "unimplemented"

erlang''unregister''1 :: ErlangFun
erlang''unregister''1 args = throw "unimplemented"

erlang''integer_to_list''1 :: ErlangFun
erlang''integer_to_list''1 args = throw "unimplemented"

erlang''dist_ctrl_input_handler''2 :: ErlangFun
erlang''dist_ctrl_input_handler''2 args = throw "unimplemented"

erlang''trunc''1 :: ErlangFun
erlang''trunc''1 args = throw "unimplemented"

erlang''fun_to_list''1 :: ErlangFun
erlang''fun_to_list''1 args = throw "unimplemented"

erlang''crasher''6 :: ErlangFun
erlang''crasher''6 args = throw "unimplemented"

erlang''read_timer''1 :: ErlangFun
erlang''read_timer''1 args = throw "unimplemented"

erlang''module_loaded''1 :: ErlangFun
erlang''module_loaded''1 args = throw "unimplemented"

erlang''md5_update''2 :: ErlangFun
erlang''md5_update''2 args = throw "unimplemented"

erlang''localtime_to_universaltime''1 :: ErlangFun
erlang''localtime_to_universaltime''1 args = throw "unimplemented"

erlang''spawn_opt''4 :: ErlangFun
erlang''spawn_opt''4 args = throw "unimplemented"

erlang''group_leader''0 :: ErlangFun
erlang''group_leader''0 args = throw "unimplemented"

erlang''op_greater :: ErlangFun
erlang''op_greater args = throw "unimplemented"

erlang''dt_put_tag''1 :: ErlangFun
erlang''dt_put_tag''1 args = throw "unimplemented"

erlang''phash2''1 :: ErlangFun
erlang''phash2''1 args = throw "unimplemented"

erlang''spawn''4 :: ErlangFun
erlang''spawn''4 args = throw "unimplemented"

erlang''crc32''1 :: ErlangFun
erlang''crc32''1 args = throw "unimplemented"

erlang''band''2 :: ErlangFun
erlang''band''2 args = throw "unimplemented"

erlang''send''3 :: ErlangFun
erlang''send''3 args = throw "unimplemented"

erlang''is_function''2 :: ErlangFun
erlang''is_function''2 args = throw "unimplemented"

erlang''system_monitor''2 :: ErlangFun
erlang''system_monitor''2 args = throw "unimplemented"

erlang''spawn_monitor''3 :: ErlangFun
erlang''spawn_monitor''3 args = throw "unimplemented"

erlang''list_to_pid''1 :: ErlangFun
erlang''list_to_pid''1 args = throw "unimplemented"

erlang''get_keys''1 :: ErlangFun
erlang''get_keys''1 args = throw "unimplemented"

erlang''demonitor''2 :: ErlangFun
erlang''demonitor''2 args = throw "unimplemented"

erlang''raise''3 :: ErlangFun
erlang''raise''3 args = throw "unimplemented"

erlang''hibernate''3 :: ErlangFun
erlang''hibernate''3 args = throw "unimplemented"

erlang''adler32_combine''3 :: ErlangFun
erlang''adler32_combine''3 args = throw "unimplemented"

erlang''node''1 :: ErlangFun
erlang''node''1 args = throw "unimplemented"

erlang''monitor''2 :: ErlangFun
erlang''monitor''2 args = throw "unimplemented"

erlang''start_timer''4 :: ErlangFun
erlang''start_timer''4 args = throw "unimplemented"

erlang''bit_size''1 :: ErlangFun
erlang''bit_size''1 args = throw "unimplemented"

erlang''call_on_load_function''1 :: ErlangFun
erlang''call_on_load_function''1 args = throw "unimplemented"

erlang''processes''0 :: ErlangFun
erlang''processes''0 args = throw "unimplemented"

erlang''error''2 :: ErlangFun
erlang''error''2 args = throw "unimplemented"

erlang''loaded''0 :: ErlangFun
erlang''loaded''0 args = throw "unimplemented"

erlang''bump_reductions''1 :: ErlangFun
erlang''bump_reductions''1 args = throw "unimplemented"

erlang''send_nosuspend''3 :: ErlangFun
erlang''send_nosuspend''3 args = throw "unimplemented"

erlang''op_unAppend :: ErlangFun
erlang''op_unAppend args = throw "unimplemented"

erlang''make_tuple''3 :: ErlangFun
erlang''make_tuple''3 args = throw "unimplemented"

erlang''unlink''1 :: ErlangFun
erlang''unlink''1 args = throw "unimplemented"

erlang''demonitor''1 :: ErlangFun
erlang''demonitor''1 args = throw "unimplemented"

erlang''trace_info''2 :: ErlangFun
erlang''trace_info''2 args = throw "unimplemented"

erlang''delete_module''1 :: ErlangFun
erlang''delete_module''1 args = throw "unimplemented"

erlang''garbage_collect''1 :: ErlangFun
erlang''garbage_collect''1 args = throw "unimplemented"

erlang''check_old_code''1 :: ErlangFun
erlang''check_old_code''1 args = throw "unimplemented"

erlang''spawn_monitor''1 :: ErlangFun
erlang''spawn_monitor''1 args = throw "unimplemented"

erlang''min''2 :: ErlangFun
erlang''min''2 args = throw "unimplemented"

erlang''op_append :: ErlangFun
erlang''op_append args = throw "unimplemented"

erlang''float_to_binary''2 :: ErlangFun
erlang''float_to_binary''2 args = throw "unimplemented"

erlang''system_profile''2 :: ErlangFun
erlang''system_profile''2 args = throw "unimplemented"

erlang''error''1 :: ErlangFun
erlang''error''1 args = throw "unimplemented"

erlang''delay_trap''2 :: ErlangFun
erlang''delay_trap''2 args = throw "unimplemented"

erlang''spawn_link''4 :: ErlangFun
erlang''spawn_link''4 args = throw "unimplemented"

erlang''is_record''3 :: ErlangFun
erlang''is_record''3 args = throw "unimplemented"

erlang''memory''0 :: ErlangFun
erlang''memory''0 args = throw "unimplemented"

erlang''halt''0 :: ErlangFun
erlang''halt''0 args = throw "unimplemented"

erlang''process_flag''3 :: ErlangFun
erlang''process_flag''3 args = throw "unimplemented"

erlang''set_cpu_topology''1 :: ErlangFun
erlang''set_cpu_topology''1 args = throw "unimplemented"

erlang''list_to_tuple''1 :: ErlangFun
erlang''list_to_tuple''1 args = throw "unimplemented"

erlang''universaltime''0 :: ErlangFun
erlang''universaltime''0 args = throw "unimplemented"

erlang''op_neq :: ErlangFun
erlang''op_neq args = throw "unimplemented"

erlang''bitstring_to_list''1 :: ErlangFun
erlang''bitstring_to_list''1 args = throw "unimplemented"
