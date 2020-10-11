-module(erlscripten).
-author("gorbak25").

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    io:format(user, "~p\n", [Forms]),
    Forms.
