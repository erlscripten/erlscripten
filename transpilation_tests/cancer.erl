
-module(cancer).
-erlscripten_output(autogenerated_transpilation_tests).
-compile({parse_transform, erlps_parse_transform}).
-compile(nowarn).
-compile(export_all).

%% API
-export([]).

test_wtf() ->
  io:format("Hello World!\n"),
  io:format(user, "Hello from Erlang!\n"),
  io:format(user, "Term from erlang: ~p\n", [{1,33,7}]),
  ok.
