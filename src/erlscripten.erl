-module(erlscripten).

-export([main/1]).

-define(OPT_SPEC,
    [ {help, $h, "help", undefined, "Show this message"}
    , {version, undefined, "version", undefined, "Transpiler version"}
    , {transpile_single, $s, "transpile_single", string, "Transpile a single beam file - used for bootstraping erlscripten"}
    , {output, $o, "output", string, "Output file or directory"} ]).

usage() ->
    getopt:usage(?OPT_SPEC, "erlscripten"),
    timer:sleep(10),
    io:format("EXAMPLES:\n"
              "[version] :\n"
              "  erlscripten --version\n"),
    error.

main(Args) ->
    case main1(Args) of
        ok -> ok;
        _  -> erlang:halt(1)
    end.

main1(Args) ->
    case getopt:parse(?OPT_SPEC, Args) of
        {ok, {Opts, []}} ->
            IsHelp            = proplists:get_value(help, Opts, false),
            IsVersion         = proplists:get_value(version, Opts, false),
            TranspileSingle = proplists:get_value(transpile_single, Opts, undefined),
            Output            = proplists:get_value(output, Opts, undefined),
            if  IsHelp ->
                    usage();
                IsVersion ->
                    io:format("Erlscripten version ~s\n", [erlps_transpiler:version()]);
                TranspileSingle =/= undefined ->
                    case Output of
                      undefined ->
                        io:format("Please provide the output file where the Purescript module will be generated\n");
                      _ ->
                        do_transpile_single(TranspileSingle, Output)
                    end;
                true ->
                  usage()
            end;

        {ok, {_, NonOpts}} ->
            io:format("Can't understand ~p\n\n", [NonOpts]),
            usage();

        {error, {Reason, Data}} ->
            io:format("Error: ~s ~p\n\n", [Reason, Data]),
            usage()
    end.

do_transpile_single(TranspileSingle, Output) ->
  case beam_lib:chunks(TranspileSingle, [abstract_code]) of
    {ok,{_,[{_,{_,Forms}}]}} ->
      try
        file:delete(Output),
        {ok, Handle} = file:open(Output, [write]),

        PSAst = erlps_transpiler:transpile_erlang_module(Forms),
        TxtModule = erlps_purescript_pretty:format_module(PSAst),
        file:write(Handle, TxtModule),
        erlps_logger:info("File ~s compiled successfully\n", [TranspileSingle])
      catch Error:Reason:StackTrace ->
        erlps_logger:die(TranspileSingle,
            io_lib:format("Error: ~s\nReason: ~p\nStacktrace: ~p\n", [atom_to_list(Error), Reason, StackTrace])
        )
      end;
    {error,beam_lib,{file_error, _, enoent}} ->
      io:format("File: ~p does not exist\n", [TranspileSingle]);
    Error ->
      io:format("Failed to open beam file: ~p\n", [Error])
  end.
