-module(erlps_logger).

-author("gorbak25").

%% API
-export([die/2, debug/1, debug/2, info/1, info/2]).

die(File, Reason) ->
    io:format(user,
              "\e[1;31m[ERROR]\e[1;37m While compiling\e[0m ~s\e[1;37m:\e[0m\n~s\n",
              [File, iolist_to_binary(Reason)]),
    error(die).

debug(Format) ->
    debug(Format, []).

debug(Format, Args) ->
    print_with_tag("\e[1;33m[DEBUG]\e[0m", Format, Args).

info(Format) ->
    info(Format, []).

info(Format, Args) ->
    print_with_tag("\e[1;36m[INFO]\e[0m", Format, Args).

print_with_tag(Tag, Format, Args) ->
    io:format(user, "~s ~s\n", [Tag, iolist_to_binary(io_lib:format(Format, Args))]).
