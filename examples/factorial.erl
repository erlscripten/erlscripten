-module(factorial).

-export([factorial/1]).

factorial(N) ->
    factorial(N, 1).

factorial(0, Acc) ->
    Acc;
factorial(N, Acc) ->
    factorial(N - 1, Acc * N).
