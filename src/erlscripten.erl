-module(erlscripten).

-export([main/1]).

-define(OPT_SPEC,
    [ {help, $h, "help", undefined, "Show this message"}
    , {version, undefined, "version", undefined, "Transpiler version"}
    , {transpile_single, $s, "transpile_single", string, "Transpile a single beam file - used for bootstraping erlscripten"}
    , {transpile_dir, $d, "transpile_dir", string, "Transpile directory"}
    , {transpile_project, $p, "transpile_project", string, "Transpile rebar project"}
    , {omit_files, $O, "omit", string, "Files to omit"}
    , {skip_tests, undefined, "skip-tests", boolean, "Skip tests during project transpilation"}
    , {split_clauses, $S, "split-clauses", string, "Split multiple clauses into separate functions in order to ease pattern resolving for Purescript. The format is <chunk_size>:<module_name>:<erl_function_name>. Note that it will prevent tail calls."}
    , {output, $o, "output", string, "Output file or directory"} ]).

usage() ->
    getopt:usage(?OPT_SPEC, "erlscripten"),
    timer:sleep(10),
    io:format("EXAMPLES:\n"
              "[version] :\n"
              "  erlscripten --version\n"),
    error.

parse_args(Args) ->
    case getopt:parse(?OPT_SPEC, Args) of
        {ok, {Opts, []}} ->
            IsHelp            = proplists:get_value(help, Opts, false),
            IsVersion         = proplists:get_value(version, Opts, false),
            TranspileSingle   = proplists:get_value(transpile_single, Opts, undefined),
            TranspileProject  = proplists:get_value(transpile_project, Opts, undefined),
            TranspileDir      = proplists:get_value(transpile_dir, Opts, undefined),
            OmitFiles         = proplists:get_all_values(omit_files, Opts),
            SkipTests         = proplists:get_value(skip_tests, Opts, false),
            Output            = proplists:get_value(output, Opts, undefined),
            SplitClauses      = proplists:get_all_values(split_clauses, Opts),

            SplitClausesFormatted =
                try lists:foldr(
                      fun(Str, Acc) ->
                              try string:split(Str, ":", all) of
                                  [SizeStr, Mod, Fun] ->
                                      [{list_to_integer(SizeStr), Mod, Fun}|Acc];
                                  _ -> throw(Str)
                              catch error:badarg -> throw(Str)
                              end
                      end,
                      [], SplitClauses)
                catch S ->
                        io:format("Bad format for split-clauses: ~s", [S]),
                        usage()
                end,

            #{ is_help => IsHelp
             , is_version => IsVersion
             , transpile_single => TranspileSingle
             , transpile_project => TranspileProject
             , transpile_dir => TranspileDir
             , omit_files => OmitFiles
             , skip_tests => SkipTests
             , output => Output
             , split_clauses => SplitClausesFormatted
            };
        {ok, {_, NonOpts}} ->
            io:format("Can't understand ~p\n\n", [NonOpts]),
            usage();
        {error, {Reason, Data}} ->
            io:format("Error: ~s ~p\n\n", [Reason, Data]),
            usage()
    end.


main(Args) ->
    case main1(Args) of
        ok -> ok;
        _  -> erlang:halt(1)
    end.

main1(Args) ->
    case parse_args(Args) of
        error ->
            error;
        Config =
        #{ is_help := IsHelp
        , is_version := IsVersion
        , transpile_single := TranspileSingle
        , transpile_project := TranspileProject
        , transpile_dir := TranspileDir
        , output := Output
        } ->
            if IsHelp ->
                    usage();
               IsVersion ->
                    io:format("Erlscripten version ~s\n", [erlps_transpiler:version()]);
               TranspileSingle =/= undefined ->
                    case Output of
                        undefined ->
                            io:format("Please provide the output file where the Purescript module will be generated\n");
                        _ ->
                            do_transpile_single(TranspileSingle, Output, Config)
                    end;
               TranspileProject =/= undefined ->
                    do_transpile_project(TranspileProject, Output, Config);
               TranspileDir =/= undefined ->
                    do_transpile_dir(TranspileDir, Output, Config);
               true ->
                    usage()
            end
    end.

do_transpile_single(TranspileSingle, Output, Config) ->
    case beam_lib:chunks(TranspileSingle, [abstract_code]) of
    {ok,{_,[{_,{_,Forms}}]}} ->
      try
        file:delete(Output),
        {ok, Handle} = file:open(Output, [write]),

        PSAst = erlps_transpiler:transpile_erlang_module(Forms, Config),
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


do_transpile_dir(SrcDir, OutDir, Config = #{omit_files := OmitFiles}) ->
    erlps_logger:info("Transpiling directory ~s", [SrcDir]),
    [ begin
          OutFile = erlps_transpiler:erlang_module_to_purs_file(filename:basename(File, ".beam")),
          do_transpile_single(
            filename:append(SrcDir, File),
            filename:append(OutDir, OutFile),
            Config
           )
      end
      || File <- list_beams(SrcDir),
         not lists:any(
               fun(Omit) ->
                       lists:prefix(
                         filename:split(Omit),
                         filename:split(filename:append(SrcDir, File)))
               end, OmitFiles)
    ],
    ok.

do_transpile_project(Project, Output, Config = #{skip_tests := SkipTests}) ->
    erlps_logger:info("Transpiling project ~s", [Project]),
    DefaultDir = filename:append(Project, "_build/default/lib"),
    TestDir = filename:append(Project, "_build/test/lib"),

    SrcOf = fun(Module) ->
                    ModuleDir = filename:append(DefaultDir, Module),
                    filename:append(ModuleDir, "ebin")
            end,
    TestOf = fun(Module) ->
                    ModuleDir = filename:append(TestDir, Module),
                    filename:append(ModuleDir, "test")
            end,

    OutSrc = filename:append(Output, "src"),
    OutTest = filename:append(Output, "test"),

    LibModules = case file:list_dir(DefaultDir) of
                     {ok, FuckTheLeakingScopesInErlang} -> FuckTheLeakingScopesInErlang;
                     {error, enoent} -> erlps_logger:die(DefaultDir, "Source folder not found")
                 end,
    TestModules = case file:list_dir(TestDir) of
                      {ok, OkT} -> OkT;
                      {error, enoent} -> erlps_logger:die(TestDir, "Test folder not found")
                  end,

    [ do_transpile_dir(SrcOf(LibModule), OutSrc, Config)
     || LibModule <- LibModules
    ],

    if not SkipTests ->
            erlps_logger:info("Transpiling tests", []),
            [ do_transpile_dir(TestOf(TestModule), OutTest, Config)
              || TestModule <- TestModules
            ];
       true -> erlps_logger:info("Skipping tests as requested", [])
    end,

    ok.

list_beams(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            [ File
              || File <- Files, filename:extension(File) =:= ".beam"
            ];
        _ ->
            []
    end.
