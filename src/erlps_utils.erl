%%%-------------------------------------------------------------------
%%% @author gorbak25
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Nov 2020 12:45
%%%-------------------------------------------------------------------
-module(erlps_utils).

-author("gorbak25").

%% API
-export([generate_template/1]).

generate_template(DestDir) ->
    SupportDir =
        filename:join(
            code:priv_dir(erlscripten), "support"),
    copy_recursive(SupportDir, DestDir).

copy_recursive(Source, Dest) ->
    case filelib:is_dir(Source) of
        true ->
            case filelib:is_dir(Dest) of
                false ->
                    file:make_dir(Dest);
                true ->
                    ok
            end,
            {ok, Names} = file:list_dir(Source),
            [copy_recursive(filename:join(Source, Name), filename:join(Dest, Name))
             || Name <- Names];
        false ->
            file:copy(Source, Dest)
    end.
