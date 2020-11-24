
-module(processes).
-erlscripten_output(autogenerated_transpilation_tests).
-compile({parse_transform, erlps_parse_transform}).
-compile(nowarn).
-compile(export_all).

-export([]).

test_spawn() ->
  Pid = spawn(fun() -> ok end),
  true = is_pid(Pid),
  true = is_process_alive(Pid),
  ok.

test_get_self() ->
  self().

test_simple_receive_primop() ->
  test = self() ! test,
  received = prim_eval:'receive'(fun(test) -> received end, infinity),
  ok.