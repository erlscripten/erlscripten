-module(erlscripten_logger).
-author("gorbak25").

%% API
-export([die/2, debug/1, debug/2, info/1, info/2]).

die(File, Reason) ->
    io:format(user, "Erlscripten encountered a fatal error while compiling ~s\n~s\n", [File, iolist_to_binary(Reason)]),
    error(die).

debug(Format) ->
    info(Format, []).
debug(Format, Args) ->
    print_with_tag("DEBUG", Format, Args).

info(Format) ->
    info(Format, []).
info(Format, Args) ->
    print_with_tag("INFO", Format, Args).

print_with_tag(Tag, Format, Args) ->
    io:format(user, "[~s] ~s\n", [Tag, iolist_to_binary(io_lib:format(Format, Args))]).
