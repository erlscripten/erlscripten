%%%-------------------------------------------------------------------
%%% @author gorbak25, radrow
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Nov 2020 12:47
%%%-------------------------------------------------------------------
-module(erlps_transpiler).
-author("gorbak25").

%% API
-export([ version/0
        , transpile_erlang_module/1
        , transpile_erlang_module/2
        , filter_module_attributes/1
        , erlang_module_to_purs_file/1 ]).

-include("erlps_purescript.hrl").
-include("erlps_utils.hrl").

%% The environment containing most of the configurations and meta information
-record(env,
        { current_module :: string()
        , records :: map()
        , local_imports :: #{{string(), non_neg_integer()} => string()}
        , no_auto_import :: [{atom(), non_neg_integer()}]
        , raw_functions :: #{string() => string()}
        , in_guard = false :: boolean()
        , overriden_pattern_vars = sets:new() :: sets:set(string())
        , split_clauses :: [{integer(), string(), string()}]
        }).

version() -> "v0.2.0".

%% Transforms Erlang AST into PureScript
transpile_erlang_module(Forms) ->
    transpile_erlang_module(Forms, #{}).
transpile_erlang_module(Forms, Config) ->
    Attributes = filter_module_attributes(Forms),
    ModuleName = proplists:get_value(module, Attributes),
    erlps_logger:info("Transpiling ~s", [ModuleName]),

    %% Ok now let's do some preliminary work before starting the conversion
    %% Gather record types
    Records = maps:from_list(
                [ {Record, lists:map(fun record_fields/1, Fields)}
                 || {Record, Fields} <- proplists:get_all_values(record, Attributes)]
               ),

    %% Ok It's time for some canonical imports which will be used a lot :)
    DefaultImports =
        [ #import{path = ["Prelude"]}
        , #import{path = ["Data", "BigInt"], alias = "DBI"}
        , #import{path = ["Data", "Array"], alias = "DA"}
        , #import{path = ["Data", "Maybe"], alias = "DM"}
        , #import{path = ["Data", "Map"], alias = "Map"}
        , #import{path = ["Data", "Tuple"], alias = "DT"}
        , #import{path = ["Erlang", "Builtins"], alias = "BIF"}
        , #import{path = ["Erlang", "Binary"], alias = "BIN"}
        , #import{path = ["Erlang", "Helpers"]}
        , #import{path = ["Erlang", "Exception"], alias = "EXC"}
        , #import{path = ["Erlang", "Type"]}
        , #import{path = ["Partial", "Unsafe"], explicit = ["unsafePartial"]}
        ],
    state_clear_import_requests(),
    LocalImports = maps:from_list([ {{Name, Arity}, Module}
                       || {import, {Module, ImportList}} <- Attributes,
                          {Name, Arity} <- ImportList
                      ]),
    NoAutoImport =
        [ {atom_to_list(Fun), Arity}
          || {compile, {no_auto_import, List}} <- Attributes,
             {Fun, Arity} <- List
        ],
    Env = #env{
             current_module = atom_to_list(ModuleName),
             records = Records,
             local_imports = LocalImports,
             no_auto_import = NoAutoImport,
             raw_functions = #{},
             split_clauses = maps:get(split_clauses, Config, [])
            },
    %% Now do the dirty work - transpile every function OwO
    Decls = [Fun ||
                Function = {function, _, FunName, Arity, _} <- Forms,
                case check_builtin(ModuleName, FunName, Arity, Env) of
                    local -> true;
                    _     -> false
                end,
                Fun <- transpile_function(Function, Env)
            ],
    Imports = lists:map(fun erlang_module_to_qualified_import/1, state_get_import_request()),

    Exports =
        case [all || {compile, CompileOpt} <- Attributes,
                     CompileOpt =:= export_all
                         orelse (is_list(CompileOpt) andalso lists:member(export_all, CompileOpt))
             ] of
            [] ->
                [ transpile_fun_name(Export, Arity)
                  || {export, ExportList} <- Attributes,
                     {Export, Arity} <- ExportList
                ];
            _ -> all
        end,
    OnLoad = [R || {on_load, R} <- Attributes],
    ExtraDecl =
        case OnLoad of
            [{Callback, 0}] ->
                [#valdecl{
                    name = "onload",
                    type = #type_var{name = "ErlangFun"},
                    clauses =
                        [#clause{
                            args = [pat_wildcard],
                            value =
                                #expr_app{
                                   function =
                                       #expr_var{
                                          name = transpile_fun_name(Callback, 0)},
                                   args = [#expr_array{value = []}]}}]}];
                  _ ->
                    []
                end,
    Exports1 = case Exports of
      all ->
        all;
      _ ->
        case ExtraDecl of
          [_] ->
            ["onload"|Exports];
          _ ->
            Exports
        end
    end,
    %% Dispatchers = [#top_clause{clause = Disp}
    %% || Disp <- make_dispatchers(FunctionForms)],
    #module{
       name = erlang_module_to_purs_module(ModuleName),
       exports = Exports1,
       imports = DefaultImports ++ Imports,
       decls = ExtraDecl ++ Decls %++ Dispatchers
      }.

filter_module_attributes(Forms) ->
    [ begin
          [attribute, _Ann, What | Rest] = tuple_to_list(Attr),
          list_to_tuple([What | Rest])
      end
      || Attr <- Forms, element(1, Attr) =:= attribute
    ].

record_fields({record_field, _, {atom, N, FieldName}}) -> {FieldName, {atom, N, undefined}};
record_fields({record_field, _, {atom, _, FieldName}, Default}) -> {FieldName, Default};
record_fields({typed_record_field, RecordField, _}) -> record_fields(RecordField).

-spec erlang_module_to_purs_module(string() | atom()) -> string().
erlang_module_to_purs_module(Name) when is_atom(Name) ->
    erlang_module_to_purs_module(atom_to_list(Name));
erlang_module_to_purs_module(Name) ->
    string:join(lists:map(fun string:titlecase/1, string:split(Name, "_", all)), ".").

-spec erlang_module_to_qualified_import(string() | atom()) -> purs_import().
erlang_module_to_qualified_import(Name) when is_atom(Name) ->
    erlang_module_to_qualified_import(atom_to_list(Name));
erlang_module_to_qualified_import(Name) ->
    Path = lists:map(fun string:titlecase/1, string:split(Name, "_", all)),
    Qualify = string:join(Path, "."),
    #import{path = Path, alias = Qualify}.

-spec erlang_module_to_purs_file(string() | atom()) -> string().
erlang_module_to_purs_file(Name) when is_atom(Name) ->
    erlang_module_to_purs_file(atom_to_list(Name));
erlang_module_to_purs_file(Name) ->
    string:join(lists:map(fun string:titlecase/1, string:split(Name, "_", all)), "") ++ ".purs".

transpile_function({function, _, FunName, Arity, Clauses},
                   Env = #env{split_clauses = Splits, current_module = Module}) ->
    Type = #type_var{name = "ErlangFun"},
    PSFunName = transpile_fun_name(FunName, Arity),
    PSClauses = [transpile_function_clause(Clause, Env) || Clause <- Clauses],
    CatchVars = [state_create_fresh_var("arg") || _ <- lists:seq(1, Arity)],
    FunctionClause =
        #clause{args = [#pat_array{
                           value = [#pat_var{name = Var} || Var <- CatchVars]}],
                value = ?function_clause
               },
    BadArity =
        #clause{
           args = [#pat_var{name = "args"}],
           value = ?badarity( ?make_expr_fun(Arity, #expr_var{name = PSFunName})
                            , #expr_var{name = "args"})
          },
    case lists:search(
           fun({_, Module1, FunName1}) ->
                   (FunName1 == atom_to_list(FunName)) and (Module1 == Module)
           end, Splits) of
        false ->
            AllPSClauses = PSClauses ++
                case Arity == 0 of  %% TODO some better heuristics heura
                    true -> []; _ -> [FunctionClause] end ++
                [BadArity],
            [#valdecl{name = PSFunName, clauses = AllPSClauses, type = Type}];
        {value, {Split, _, _}} ->
            MakeIdName = fun(I) ->
                                 case I of
                                     0 -> PSFunName;
                                     N -> PSFunName ++ "__p" ++ integer_to_list(N)
                                 end
                         end,
            NClauses = length(PSClauses),
            Indices = [I div Split || I <- lists:seq(0, NClauses - 1)],
            Indexed = lists:zip(Indices, PSClauses),
            LastId = lists:last(Indices),
            [ begin
                  GroupClauses = proplists:get_all_values(I, Indexed),
                  NewClauses = GroupClauses ++
                      case I == LastId of
                          true ->
                              case Arity == 0 of  %% TODO some better heuristics heura
                                  true -> []; _ -> [FunctionClause] end ++
                                  [BadArity];
                          false ->
                              [#clause{args = [#pat_var{name = "args"}],
                                       value =
                                           #expr_app{
                                              function = #expr_var{name = MakeIdName(I + 1)},
                                              args = [#expr_var{name = "args"}]
                                             }
                                      }]
                      end,
                  #valdecl{name = MakeIdName(I),
                           clauses = NewClauses,
                           type = Type}
              end
              || I <- lists:usort(Indices)
            ]
    end.

transpile_fun_name(Name, Arity) when is_atom(Name) ->
    transpile_fun_name(atom_to_list(Name), Arity);
transpile_fun_name(Name, Arity) when is_binary(Name) ->
    transpile_fun_name(binary_to_list(Name), Arity);
transpile_fun_name(Name, Arity) ->
    lists:flatten(io_lib:format("erlps__~s__~p", [Name, Arity])).


special_guard_bifs() ->
    maps:from_list(lists:concat(
      [ [  {{"erlang", Fun, 1}, {"erlang", "is_" ++ Fun, 1}}
           || Fun <-
                  [ "integer", "float", "list", "number", "atom"
                  , "tuple", "pid", "port", "function", "binary"
                  , "reference"
                  ]
        ]
      ])).

-spec builtins_calc() -> #{{string(), string(), non_neg_integer()} => string()}.
builtins_calc() ->
    Operators = [ {"+",   "op_plus"}
                , {"-",   "op_minus"}
                , {"*",   "op_mult"}
                , {"/",   "op_div"}
                , {"div", "op_div_strict"}
                , {"%",   "op_rem"}
                , {"rem", "op_rem_strict"}
                , {"/=",  "op_neq"}
                , {"=/=", "op_exactNeq"}
                , {"==",  "op_eq"}
                , {"=:=", "op_exactEq"}
                , {">",   "op_greater"}
                , {"<",   "op_lesser"}
                , {">=",  "op_greaterEq"}
                , {"=<",  "op_lesserEq"}
                , {"++",  "op_append"}
                , {"--",  "op_unAppend"}
                , {"and", "op_and"}
                , {"or",  "op_or"}
                , {"!",   "send__2"}
                ],
    maps:from_list(lists:concat([
        [ {{"erlang", "not", 1}, "erlang__op_not"}
        , {{"erlang", "-", 1}, "erlang__op_neg"}
        , {{"erlang", "+", 1}, "erlang__op_unary_plus"}
        ],
        [ {{"erlang", Op, 2}, lists:flatten(io_lib:format("erlang__~s", [Fun]))}
          || {Op, Fun} <- Operators],
        [ {{Module, Fun, Arity},
           lists:flatten(io_lib:format("~s__~s__~p", [Module, Fun, Arity]))}
          || {Module, Fun, Arity} <-
                 lists:concat(
                   [ [ {"math", "pi", 0}
                     , {"math", "sin", 1}
                     , {"math", "cos", 1}
                     , {"math", "tan", 1}
                     , {"math", "asin", 1}
                     , {"math", "acos", 1}
                     , {"math", "atan", 1}
                     , {"math", "atan2", 2}
                     , {"math", "sinh", 1}
                     , {"math", "cosh", 1}
                     , {"math", "tanh", 1}
                     , {"math", "asinh", 1}
                     , {"math", "acosh", 1}
                     , {"math", "atanh", 1}
                     , {"math", "exp", 1}
                     , {"math", "log", 1}
                     , {"math", "log2", 1}
                     , {"math", "log10", 1}
                     , {"math", "pow", 2}
                     , {"math", "sqrt", 1}
                     , {"math", "erf", 1}
                     , {"math", "erfc", 1}
                     , {"math", "ceil", 1}
                     , {"math", "floor", 1}
                     , {"math", "fmod", 2}
                     ]
                   , [ {"erts_internal", "map_next", 3}
                     , {"erts_debug", "same", 2}
                     ]
                   , [ {"prim_eval", "receive", 2}
                     ]
                   , [ {"binary", "at", 2}
                     , {"binary", "bin_to_list", 1}
                     , {"binary", "bin_to_list", 2}
                     , {"binary", "bin_to_list", 3}
                     , {"binary", "compile_pattern", 1}
                     , {"binary", "copy", 1}
                     , {"binary", "copy", 2}
                     , {"binary", "decode_unsigned", 1}
                     , {"binary", "decode_unsigned", 2}
                     , {"binary", "encode_unsigned", 1}
                     , {"binary", "encode_unsigned", 2}
                     , {"binary", "first", 1}
                     , {"binary", "last", 1}
                     , {"binary", "list_to_bin", 1}
                     , {"binary", "longest_common_prefix", 1}
                     , {"binary", "longest_common_suffix", 1}
                     , {"binary", "match", 2}
                     , {"binary", "match", 3}
                     , {"binary", "matches", 2}
                     , {"binary", "matches", 3}
                     , {"binary", "part", 2}
                     , {"binary", "part", 3}
                     , {"binary", "referenced_byte_size", 1}
                     , {"binary", "split", 2}
                     , {"binary", "split", 3}
                     ]
                   , [ {"lists", "keyfind", 3}
                     , {"lists", "keymember", 3}
                     , {"lists", "keysearch", 3}
                     , {"lists", "member", 2}
                     , {"lists", "reverse", 2}
                     ]
                   , [ {"maps", "get", 2}
                     , {"maps", "find", 2}
                     , {"maps", "from_list", 1}
                     , {"maps", "is_key", 2}
                     , {"maps", "keys", 1}
                     , {"maps", "merge", 2}
                     , {"maps", "put", 3}
                     , {"maps", "remove", 2}
                     , {"maps", "take", 2}
                     , {"maps", "to_list", 1}
                     , {"maps", "update", 3}
                     , {"maps", "values", 1}
                     ]
                   , [ {"code", "ensure_loaded", 1}
                     , {"code", "ensure_modules_loaded", 1}
                     ]
                   , [ {"erlang", atom_to_list(BIF), Arity}
                      || {BIF, Arity} <- erlang:module_info(exports),
                         proplists:get_value(atom_to_list(BIF), Operators, none) =:= none
                     ]
                   , maps:values(special_guard_bifs())
                   ]
                  )
        ]])).

builtins() ->
    case persistent_term:get({?MODULE, builtins}, error) of
      error ->
          R = builtins_calc(),
          persistent_term:put({?MODULE, builtins}, R),
          R;
      R ->
          R
    end.

-spec check_builtin(string(), string(), non_neg_integer(), #env{}) -> local | {builtin, string()}.
check_builtin(Module, Name, Arity, #env{in_guard = InGuard}) ->
    Key = {Module, Name, Arity},
    Key1 = case {InGuard, special_guard_bifs()} of
               {true, #{Key := Key_}} -> Key_;
               _ -> Key
           end,
    case builtins() of
        #{Key1 := Builtin} -> {builtin, Builtin};
        _ -> local
    end.

auto_imported(Name, Arity, #env{no_auto_import = NoAutoImport}) ->
    case lists:member({Name, Arity}, NoAutoImport) of
        true -> false;
        false ->
            AName = list_to_atom(Name),
            erl_internal:bif(AName, Arity) orelse
                lists:member(
                  {AName, Arity},
                  [ {integer, 1}, {pid, 1}, {atom, 1}, {tuple, 1}
                  , {number, 1}, {port, 1}, {binary, 1}, {list, 1}
                  , {reference, 1}, {function, 1}
                  ])
    end.

-spec transpile_fun_ref(string() | atom(), non_neg_integer(), #env{}) ->
          {direct, purs_expr()} | {code_server, string(), string()}.
transpile_fun_ref(Name, Arity, Env = #env{current_module = Module}) ->
    transpile_fun_ref(Module, Name, Arity, false, Env).
transpile_fun_ref(Module, Name, Arity, Env) ->
    transpile_fun_ref(Module, Name, Arity, true, Env).
-spec transpile_fun_ref(string() | atom(), string() | atom(), non_neg_integer(), boolean(), #env{}) ->
          {direct, purs_expr()} | {code_server, string(), string()}.
transpile_fun_ref(Module, Name, Arity, IsRemote, Env) when is_atom(Name) ->
    transpile_fun_ref(Module, atom_to_list(Name), Arity, IsRemote, Env);
transpile_fun_ref(Module, Name, Arity, IsRemote, Env) when is_atom(Module) ->
    transpile_fun_ref(atom_to_list(Module), Name, Arity, IsRemote, Env);
transpile_fun_ref(Module, Name, Arity, IsRemote,
                  #env{current_module = CurModule, local_imports = Imported} = Env) ->
    case check_builtin(Module, Name, Arity, Env) of
        {builtin, Builtin} ->
          {direct, #expr_var{name = "BIF." ++ Builtin}};
        local ->
            IsAutoImported = auto_imported(Name, Arity, Env),
            case check_builtin("erlang", Name, Arity, Env) of
                {builtin, BuiltinAnyway} when IsAutoImported, IsRemote =:= false ->
                  {direct, #expr_var{name = "BIF." ++ BuiltinAnyway}};
                _ -> if CurModule == Module ->
                            %% Check if the local module call is in fact a call to an imported function
                            Key = {list_to_atom(Name), Arity},
                            case Imported of
                                #{Key := M} ->
                                    transpile_fun_ref(M, Name, Arity, Env);
                                _ ->
                                  {direct, #expr_var{name = transpile_fun_name(Name, Arity)}}
                            end;
                        true ->
                          {code_server, erlang_module_to_purs_module(Module), transpile_fun_name(Name, Arity)}
                     end
            end
    end.

transpile_function_clause({clause, _, Args, Guards, Body}, Env) ->
    state_reset_supply_id(),
    state_clear_vars(),
    state_clear_var_stack(),
    {PsArgs, PsGuards} = transpile_pattern_sequence(Args, Env),
    PSBody = (transpile_body(Body, Env)),
    Clause = #clause{
       args = [#pat_array{value = PsArgs}],
       guards = PsGuards ++ transpile_boolean_guards(Guards, Env),
       value = PSBody
    },
    erlps_optimize:optimize_clause(Clause).

falsify_error_guard(Expr) ->
    #expr_app{
       function = #expr_var{name = "falsifyErrors"},
       args = [#expr_lambda{args = [pat_wildcard], body = Expr}]
      }.

combine_guards([#guard_expr{guard = First}|Guards], Op) ->
    combine_guards(Guards, First, Guards, Op);
combine_guards(Guards, _Op) ->
    Guards.
combine_guards([], Acc, _, _) ->
    #guard_expr{guard = Acc};
combine_guards([#guard_expr{guard = G}|Rest], Acc, Init, Op) ->
    combine_guards(Rest, #expr_binop{name = Op, lop = Acc, rop = G}, Init, Op);
combine_guards(_, _, Init, _) ->
    Init.

transpile_boolean_guards([], _Env) -> [];
transpile_boolean_guards(Guards, Env) ->
    Env1 = Env#env{in_guard = true},
    Conjs = [begin
        GSeq = [transpile_boolean_guards_singleton(E, Env1) || E <- SingleConj],
        %% If all of them compiled to guards then we won :)
        case lists:filter(fun(error) -> true; (_) -> false end, GSeq) of
          [] -> combine_guards(lists:flatten(GSeq), "&&");
          _ -> %% Fallback for some of them
            [GF] = transpile_boolean_guards_fallback([SingleConj], Env#env{in_guard = true}),
            GF
        end
      end || SingleConj <- Guards],
    case lists:filter(fun(error) -> true; (_) -> false end, Conjs) of
      [] -> [combine_guards(Conjs, "||")]
    end.

transpile_boolean_guards_singleton({call,_,{atom,_,float},[{var,_,Var}]}, _Env) ->
    [#guard_expr{guard = #expr_app{function = ?make_expr_var("isEFloat"), args =
                                       [?make_expr_var(state_get_var(Var))]}}];
transpile_boolean_guards_singleton({call,_,{atom,_,integer},[{var,_,Var}]}, _Env) ->
    [#guard_expr{guard = #expr_app{function = ?make_expr_var("isEInt"), args =
                                       [?make_expr_var(state_get_var(Var))]}}];
transpile_boolean_guards_singleton({call,_,{atom,_,number},[{var,_,Var}]}, _Env) ->
    [#guard_expr{guard = #expr_app{function = ?make_expr_var("isENum"), args =
                                       [?make_expr_var(state_get_var(Var))]}}];
transpile_boolean_guards_singleton({call,_,{atom,_,pid},[{var,_,Var}]}, _Env) ->
    [#guard_expr{guard = #expr_app{function = ?make_expr_var("isEPid"), args =
                                       [?make_expr_var(state_get_var(Var))]}}];
transpile_boolean_guards_singleton({call,_,{atom,_,tuple},[{var,_,Var}]}, _Env) ->
    [#guard_expr{guard = #expr_app{function = ?make_expr_var("isETuple"), args =
                                       [?make_expr_var(state_get_var(Var))]}}];
transpile_boolean_guards_singleton({call,_,{atom,_,atom},[{var,_,Var}]}, _Env) ->
    [#guard_expr{guard = #expr_app{function = ?make_expr_var("isEAtom"), args =
                                       [?make_expr_var(state_get_var(Var))]}}];
transpile_boolean_guards_singleton({call,_,{atom,_,function},[{var,_,Var}]}, _Env) ->
    [#guard_expr{guard = #expr_app{function = ?make_expr_var("isEFun"), args =
                                       [?make_expr_var(state_get_var(Var))]}}];
transpile_boolean_guards_singleton({call,_,{atom,_,list},[{var,_,Var}]}, _Env) ->
    [#guard_expr{guard = #expr_app{function = ?make_expr_var("isEList"), args =
                                       [?make_expr_var(state_get_var(Var))]}}];
transpile_boolean_guards_singleton({call,_,{atom,_,binary},[{var,_,Var}]}, _Env) ->
    [#guard_expr{guard = #expr_app{function = ?make_expr_var("isEBinary"), args =
                                       [?make_expr_var(state_get_var(Var))]}}];
transpile_boolean_guards_singleton({call,_,{atom,_,is_list},[{var,_,Var}]}, _Env) ->
  [#guard_expr{guard = #expr_app{function = ?make_expr_var("isEList"), args =
                                     [?make_expr_var(state_get_var(Var))]}}];
transpile_boolean_guards_singleton({call,_,{atom,_,is_binary},[{var,_,Var}]}, _Env) ->
  [#guard_expr{guard = #expr_app{function = ?make_expr_var("isEBinary"), args =
                                     [?make_expr_var(state_get_var(Var))]}}];
transpile_boolean_guards_singleton({call,_,{atom,_,is_tuple},[{var,_,Var}]}, _Env) ->
  [#guard_expr{guard = #expr_app{function = ?make_expr_var("isETuple"), args =
                                     [?make_expr_var(state_get_var(Var))]}}];
transpile_boolean_guards_singleton({call,_,{atom,_,is_number},[{var,_,Var}]}, _Env) ->
  [#guard_expr{guard = #expr_app{function = ?make_expr_var("isENum"), args =
                                     [?make_expr_var(state_get_var(Var))]}}];
transpile_boolean_guards_singleton({call,_,{atom,_,is_integer},[{var,_,Var}]}, _Env) ->
    [#guard_expr{guard = #expr_app{function = ?make_expr_var("isEInt"), args =
                                       [?make_expr_var(state_get_var(Var))]}}];
transpile_boolean_guards_singleton({call,_,{atom,_,is_float},[{var,_,Var}]}, _Env) ->
    [#guard_expr{guard = #expr_app{function = ?make_expr_var("isEFloat"), args =
                                       [?make_expr_var(state_get_var(Var))]}}];
transpile_boolean_guards_singleton({call,_,{atom,_,is_atom},[{var,_,Var}]}, _Env) ->
  [#guard_expr{guard = #expr_app{function = ?make_expr_var("isEAtom"), args =
                                     [?make_expr_var(state_get_var(Var))]}}];
transpile_boolean_guards_singleton({call,_,{atom,_,is_map},[{var,_,Var}]}, _Env) ->
  [#guard_expr{guard = #expr_app{function = ?make_expr_var("isEMap"), args =
                                     [?make_expr_var(state_get_var(Var))]}}];
transpile_boolean_guards_singleton({call,_,{atom,_,is_function},[{var,_,Var}]}, _Env) ->
  [#guard_expr{guard = #expr_app{function = ?make_expr_var("isEFun"), args =
                                     [?make_expr_var(state_get_var(Var))]}}];
transpile_boolean_guards_singleton({call,_,{atom,_,is_function},[{var,_,Var},{integer,_,Arity}]}, _Env) ->
  [#guard_expr{guard = #expr_app{function = ?make_expr_var("isEFunA"), args =
                                     [?make_expr_var(state_get_var(Var)),
                                      ?make_expr_int(Arity)
                                     ]}}];
transpile_boolean_guards_singleton({call,_,{atom,_,element},[A1,A2]}, Env) ->
  E1 = guard_trivial_expr(A1, Env),
  E2 = guard_trivial_expr(A2, Env),
  case {E1, E2} of
    {error, _} -> error;
    {_, error} -> error;
    _ ->
      [#guard_expr{guard = #expr_app{function = ?make_expr_var("onElement"), args =
                                     [E1,
                                      E2,
                                      ?make_expr_var("(==)"),
                                      ?make_expr_atom(true)
                                     ]}}]
  end;
transpile_boolean_guards_singleton({op, _, Op0, {call,_,{atom,_,element},[A1,A2]}, Rop}, Env) ->
  E1 = guard_trivial_expr(A1, Env),
  E2 = guard_trivial_expr(A2, Env),
  E3 = guard_trivial_expr(Rop, Env),
  Op = case Op0 of _ when is_atom(Op0) -> atom_to_list(Op0); _ -> Op0 end,
  F = fun(N) ->
            [#guard_expr{
                guard =
                    #expr_app{function = ?make_expr_var("onElement"), args =
                                     [E1,
                                      E2,
                                      ?make_expr_var(N),
                                      E3
                                     ]}
                      }
               ] end,
  case {E1, E2, E3} of
    {error, _, _} -> error;
    {_, error, _} -> error;
    {_, _, error} -> error;
    _ ->
      case Op of
          "==" ->
            F("weakEq");
          "/=" ->
            F("weakNEq");
          "=:=" ->
            F("(==)");
          "=/=" ->
            F("(/=)");
          "=<" ->
            F("weakLeq");
          "<" ->
            F("weakLt");
          ">=" ->
            F("weakGeq");
          ">" ->
            F("weakGt");
          _ ->
            erlps_logger:debug("Unhandled guard op ~p", [Op]),
            error
      end
  end;
transpile_boolean_guards_singleton({var, _, Var}, _Env) ->
  [#guard_expr{guard = #expr_app{ function = ?make_expr_var("(==)")
                                , args = [?make_expr_atom(true), ?make_expr_var(state_get_var(Var))]}}];
transpile_boolean_guards_singleton({op, _, Andalso, L, R}, Env)
  when Andalso =:= 'andalso'; Andalso =:= "andalso" ->
  case {transpile_boolean_guards_singleton(L, Env),
        transpile_boolean_guards_singleton(R, Env)} of
    {error, _} -> error;
    {_, error} -> error;
    {LG, RG} -> [LG, RG]
  end;
transpile_boolean_guards_singleton({op, _, Op0, Lop, Rop}, Env) ->
    LE = guard_trivial_expr(Lop, Env),
    RE = guard_trivial_expr(Rop, Env),
    Op = case Op0 of _ when is_atom(Op0) -> atom_to_list(Op0); _ -> Op0 end,
    F = fun(N) ->
                [#guard_expr{
                    guard =
                        #expr_app{
                           function = #expr_var{name = N},
                           args = [LE, RE]
                          }
                   }] end,
    case {LE, RE} of
        {error, _} -> error;
        {_, error} -> error;
        _ ->
            case Op of
                "==" ->
                  F("weakEq");
                "/=" ->
                  F("weakNEq");
                "=:=" ->
                  F("(==)");
                "=/=" ->
                  F("(/=)");
                "=<" ->
                  F("weakLeq");
                "<" ->
                  F("weakLt");
                ">=" ->
                  F("weakGeq");
                ">" ->
                  F("weakGt");
                _ ->
                  erlps_logger:debug("Unhandled guard op ~p", [Op]),
                  error
            end
    end;
transpile_boolean_guards_singleton(_Expr, _Env) -> error.

guard_trivial_expr({var, _, V}, _Env) ->
    #expr_var{name = state_get_var(V)};
guard_trivial_expr({nil, _}, _Env) ->
    ?make_expr_empty_list;
guard_trivial_expr({atom, _, Atom}, _Env) ->
    ?make_expr_atom(Atom);
guard_trivial_expr({integer, _, Num}, _Env) ->
    ?make_expr_int(Num);
guard_trivial_expr({char, Ann, Num}, Env) ->
    guard_trivial_expr({integer, Ann, Num}, Env);
guard_trivial_expr(_Expr, _Env) ->
    error.

transpile_boolean_guards_fallback(Guards, Env) ->
    Alts = [
      lists:foldl(fun(G, AccConjs) -> {op, any, "andalso", AccConjs, G} end,
        hd(Alt), tl(Alt)) || Alt <- Guards ],
    E = lists:foldl(
      fun(Alt, AccAlts) -> {op, any, "orelse", AccAlts, Alt} end,
      hd(Alts), tl(Alts)),
    [#guard_expr{
        guard =
            #expr_binop{
               name = "==",
               lop = ?make_expr_atom(true),
               rop = falsify_error_guard(transpile_expr(E, Env#env{in_guard = true}))
              }
    }].

transpile_pattern_sequence(PatternSequence, Env) ->
    state_push_var_stack(), %% Push fully bound variables
    S = [transpile_pattern(Pattern, Env) || Pattern <- PatternSequence, is_tuple(Pattern)],
    PSArgs = [A || {A, _, _} <- S],
    %% First the guards which will create the variable bindings
    %% Then the guards which will ensure term equality
    PsGuards = lists:flatten([G || {_, G, _} <- S]) ++ lists:flatten([G || {_, _, G} <- S]),
    {PSArgs, PsGuards}.

%% An erlang pattern can always be compiled to a purescript pattern and a list of guards
%% Returns {match, g_match, values_eq}
%% match is an purescript pattern, g_match are the pattern guards which will bring the
%% necessary bindings to the appropriate scope, values_eq ensure erlang term equality
%% for cosmetic reasons values_eq are aggregated and evaluated only after all g_match got executed
%% https://erlang.org/doc/apps/erts/absform.html#patterns
%% Atomic Literals
transpile_pattern({atom, _, Atom}, _) ->
    {?make_pat_atom(Atom), [], []};
transpile_pattern({char, Ann, Char}, Env) ->
     transpile_pattern({integer, Ann, Char}, Env);
transpile_pattern({float, _, Float}, _) ->
     {?make_pat_float(Float), [], []};
transpile_pattern({integer, _, Num}, _) ->
    Var = state_create_fresh_var("num"),
    {#pat_constr{constr = "ErlangInt",
                 args = [#pat_var{name = Var}]
                },
     [#guard_expr{guard = #expr_binop{
                             name = "==",
                             lop = #expr_app{function = #expr_var{name = "ErlangInt"},
                                             args = [#expr_var{name = Var}]},
                             rop = ?make_expr_int(Num)
                            }}],
     []};
transpile_pattern({op, _, "-", {integer, Ann, Num}}, Env) ->
    transpile_pattern({integer, Ann, -Num}, Env);
transpile_pattern({string, Ann, String}, Env) ->
    transpile_pattern(lists:foldr(
       fun (Char, Acc) ->
          {cons, Ann, {integer, Ann, Char}, Acc}
       end, {nil, Ann}, String), Env);

%% Bitstring pattern TODO: bits
transpile_pattern({bin, _, []}, _) ->
    Var = state_create_fresh_var("binEnd"),
    {#pat_constr{constr = "ErlangBinary", args = [#pat_var{name = Var}]}, [
        #guard_expr{guard =
        #expr_app{
            function = #expr_var{name = "BIN.empty"},
            args = [#expr_var{name = Var}]}}
    ], []};
transpile_pattern({bin, _, Segments}, Env) ->
    Var = state_create_fresh_var("binSeg"),
    {G, V} = transpile_binary_pattern_segments(Var, Segments, Env),
    {#pat_constr{constr = "ErlangBinary", args = [#pat_var{name = Var}]}, G, V};
%% Compound pattern
transpile_pattern({match, _, {var, _, _Var} = V, P}, Env) ->
    {H, G, V1} = transpile_pattern(P, Env),
    case transpile_pattern(V, Env) of
      pat_wildcard ->
        {H, G, V1};
      {#pat_var{name = N}, [], V2} ->
        {#pat_as{name = N, pattern = H}, G, V1++V2}
    end;
transpile_pattern({match, Ann, P, {var, _, _} = V}, Env) ->
    transpile_pattern({match, Ann, V, P}, Env);
transpile_pattern({match, _, P1, P2}, Env) ->
    {H1, G1, V1} = transpile_pattern(P1, Env),
    {H2, G2, V2} = transpile_pattern(P2, Env),
    Var = state_create_fresh_var("matchPat"),
    {#pat_as{name = Var, pattern = H2},
        [#guard_assg{lvalue = H1, rvalue = #expr_var{name = Var}} | G1 ++ G2], V1 ++ V2};

%% Cons pattern
transpile_pattern({cons, _, Head, Tail}, Env) ->
    {H, GH, VH} = transpile_pattern(Head, Env),
    {T, GT, VT} = transpile_pattern(Tail, Env),
    {?make_pat_cons(H, T), GH ++ GT, VH ++ VT};

%% Map pattern
transpile_pattern({map, _, Associations}, Env) ->
    MapVar = state_create_fresh_var("map"),
    {G, V} =
        lists:foldl(fun({map_field_exact, _, Key, Value}, {Gs, Vs}) ->
            begin
                {ValPat, GV, VV} = transpile_pattern(Value, Env),
                KeyExpr = transpile_expr(Key, Env),
                QueryGuard = #guard_assg{
                    lvalue = #pat_constr{constr = "DM.Just", args = [ValPat]},
                    rvalue = #expr_app{
                        function = #expr_var{name = "Map.lookup"},
                        args = [KeyExpr, #expr_var{name = MapVar}]}},
                {[QueryGuard | GV ++ Gs], VV ++ Vs}
            end end, {[], []}, Associations),
    {?make_pat_map(#pat_var{name = MapVar}), G, V};

%% Nil pattern
transpile_pattern({nil, _}, _Env) ->
    {?make_pat_empty_list, [], []};

%% Operator pattern
transpile_pattern({op, _, '++', {nil, _}, P2}, Env) ->
    transpile_pattern(P2, Env);
transpile_pattern({op, Ann, '++', {cons, AnnC, H, T}, P2}, Env) ->
    transpile_pattern({cons, AnnC, H, {op, Ann, '++', T, P2}}, Env);
transpile_pattern({op, Ann, '++', {string, AnnS, String}, Right}, Env) ->
    P = lists:foldr(fun (Char, Acc) -> {cons, AnnS, {integer, AnnS, Char}, Acc}
                 end, {nil, AnnS}, String),
    transpile_pattern({op, Ann, '++', P, Right}, Env);
transpile_pattern(P, Env) when element(1, P) =:= op ->
    case compute_constexpr(P) of
        {ok, Num} when is_integer(Num) ->
            transpile_pattern({integer, element(2, P), Num}, Env);
        {ok, Float} when is_float(Float) ->
            transpile_pattern({float, element(2, P), Float}, Env);
        {ok, Res} -> {Res, [], []};
        error -> error({illegal_operator_pattern, P})
    end;

%% Record index pattern
transpile_pattern(Expr = {record_index, _, _RecordName, _Field}, _Env) ->
    error({unimplemented_expr, Expr});

%% Record pattern
transpile_pattern({record, Ann, RecordName, RecordFields}, Env) ->
    %% Convert this to a tuple
    Matches = [record_fields(X) || X <- RecordFields],
    Fields = [{atom, Ann, RecordName}] ++
        [proplists:get_value(FieldName, Matches, {var, Ann, "_"}) ||
            {FieldName, _} <- maps:get(RecordName, Env#env.records)],
    transpile_pattern({tuple, Ann, Fields}, Env);

%% Tuple pattern
transpile_pattern({tuple, _, Args}, Env) ->
    S = [transpile_pattern(Arg, Env) || Arg <- Args, is_tuple(Arg)],
    PSArgs = [A || {A, _, _} <- S],
    PsVarGuards = lists:flatten([G || {_, G, _} <- S]),
    PsValGuards = lists:flatten([G || {_, _, G} <- S]),
    {?make_pat_tuple(PSArgs), PsVarGuards, PsValGuards};

%% Universal pattern
transpile_pattern({var, _, [$_|_]}, _) ->
    {pat_wildcard, [], []};
transpile_pattern({var, _, '_'}, _) ->
    {pat_wildcard, [], []};

%% Variable pattern
transpile_pattern({var, _, ErlangVar}, Env) ->
    Var = string:to_lower(lists:flatten(io_lib:format("~s_~p", [ErlangVar, state_new_id()]))),

    case state_is_used(ErlangVar) andalso
        not sets:is_element(ErlangVar, Env#env.overriden_pattern_vars) of
        false ->
            state_put_var(ErlangVar, Var),
            {#pat_var{name = Var}, [], []};
        true ->
            case Env of
                #env{raw_functions = #{ErlangVar := {Name, Arity}}} ->
                    {#pat_constr{
                       constr = "ErlangFun",
                       args = [#pat_num{value = Arity}, #pat_var{name = Name}]
                      }, [], []};
                _ -> {#pat_var{name = Var},
                      [],
                      [#guard_expr{
                          guard =
                              #expr_binop{
                                 name = "==",
                                 lop = #expr_var{name = Var},
                                 rop = #expr_var{name = state_get_var(ErlangVar)} }}]}
            end
    end;


transpile_pattern(Arg, _Env) ->
    error({unimplemented_pattern, Arg}).

parse_bin_segment_spec(Element, default) ->
    parse_bin_segment_spec(Element, []);
parse_bin_segment_spec(_Element, Spec) ->
    F = fun(Keys, Default) -> [Res|_] = [Y || X <- Spec, Y <- Keys, X =:= Y] ++ [Default], Res end,
    Type = F([integer, float, binary, bitstring, bits], integer),
    Sign = F([unsigned, signed], unsigned),
    Endian = F([little, big], big), %% native :P
    Unit = proplists:get_value(
             unit, Spec, case Type of
                             integer -> 1;
                             float -> 1;
                             bits -> 1;
                             binary -> 8;
                             bitstring -> 1;
                             _ -> error({todo, Type, i_was_to_lazy_to_check_this_case})
                         end),

    #{type => Type, sign => Sign, endian => Endian, unit => Unit}.

-define(BinCall(X, Rest, Expr),
        #guard_assg{
           lvalue = #pat_constr{
                       constr = "BIN.Ok",
                       args = [X, #pat_var{name = Rest}]
                      },
           rvalue = Expr
          }
       ).

transpile_binary_pattern_segments(UnboxedVar, Segments, Env) ->
    transpile_binary_pattern_segments(UnboxedVar, Segments, [], [], Env).
transpile_binary_pattern_segments(UnboxedVar, [], Guards, VarUni, _Env) ->
    Fin = #guard_expr{
             guard = #expr_app{
                        function = #expr_var{name = "BIN.empty"},
                        args = [#expr_var{name = UnboxedVar}]
                       }
            },
    {lists:reverse([Fin|Guards]), lists:reverse(VarUni)};
transpile_binary_pattern_segments(
 _UnboxedVar, [{bin_element, _, {var,_,'_'}, default, [binary]}|_], Guards, VarUni, _) ->
  %% Matches binaries but not bitstrings :)
  %% Emit alignment check
  {lists:reverse(Guards), lists:reverse(VarUni)};
transpile_binary_pattern_segments(
 _UnboxedVar, [{bin_element, _, {var,_,'_'}, default, [bitstring]}|_], Guards, VarUni, _) ->
  %% Matches everything
  {lists:reverse(Guards), lists:reverse(VarUni)};
transpile_binary_pattern_segments(
 UnboxedVar, [{bin_element, Ann, Element, Size, Spec}|Rest], Guards, VarUni, Env) ->
    %%io:format(user, "~p ~p ~p\n", [Element, Size, Spec]),
    case {Element, Size, parse_bin_segment_spec(Element, Spec)} of

        {{string, AnnS, S}, _, _} ->
            transpile_binary_pattern_segments(
              UnboxedVar,
              [{bin_element, Ann, {integer, AnnS, I}, Size, Spec} || I <- S] ++ Rest,
              Guards, VarUni, Env);

        {_, _, #{type := Type, unit := Unit, sign := Sign, endian := Endian}} ->
            SizeVar = state_create_fresh_var("size"),
            RestBinVar = state_create_fresh_var("bin"),
            SizeExpr = case Size of
                           default ->
                               case Type of
                                   integer -> ?make_expr_int(8);
                                   float -> ?make_expr_int(64);
                                   B0 when B0 =:= binary orelse B0 =:= bitstring ->
                                       #expr_app{
                                          function = #expr_var{name = "BIN.size"},
                                          args = [#expr_var{name = UnboxedVar}]
                                         }
                               end;
                           _ -> transpile_expr(Size, Env)
                       end,
            {P, G, V} = transpile_pattern(Element, Env),

            SizeGuard =
                #guard_assg{
                   lvalue = #pat_constr{constr = "ErlangInt", args = [#pat_var{name = SizeVar}]},
                   rvalue = SizeExpr
                  },

            GetGuard =
                case Type of
                    integer ->
                        ?BinCall(
                           P, RestBinVar,
                           #expr_app{
                              function = #expr_var{name = "BIN.chopInt"},
                              args =
                                  [ #expr_var{name = UnboxedVar}
                                  , #expr_var{name = SizeVar}
                                  , #expr_num{value = Unit}
                                  , #expr_var{name = case Endian of
                                                         big -> "BIN.Big";
                                                         little -> "BIN.Little"
                                                     end
                                             }
                                  , #expr_var{name = case Sign of
                                                         signed -> "BIN.Signed";
                                                         unsigned -> "BIN.Unsigned"
                                                     end
                                             }
                                  ]
                             }
                          );
                    float ->
                        ?BinCall(
                           P, RestBinVar,
                           #expr_app{
                              function = #expr_var{name = "BIN.chopFloat"},
                              args =
                                  [ #expr_var{name = UnboxedVar}
                                  , #expr_var{name = SizeVar}
                                  , #expr_num{value = Unit}
                                  , #expr_var{name = case Endian of
                                                         big -> "BIN.Big";
                                                         little -> "BIN.Little"
                                                     end
                                             }
                                  ]
                             }
                          );
                    B when B =:= binary orelse B =:= bitstring ->
                        ?BinCall(
                           P, RestBinVar,
                           #expr_app{
                              function = #expr_var{name = "BIN.chopBin"},
                              args =
                                  [ #expr_var{name = UnboxedVar}
                                  , #expr_var{name = SizeVar}
                                  , #expr_num{value = Unit}
                                  ]
                             }
                          )
                end,

            transpile_binary_pattern_segments(
              RestBinVar, Rest, G ++ [GetGuard, SizeGuard | Guards], V ++ VarUni, Env);

        {B, _, _} -> error({unsupported_bin_element, B})
    end.

transpile_body(Body, Env) ->
    {PSBody0, LetDefs} = transpile_body(Body, [], Env),
    PSBody1 =
        case LetDefs of
            [] -> PSBody0;
            _ -> apply_assignments(LetDefs, PSBody0)
        end,
    PSBody2 = catch_partial_lets(PSBody1),
    PSBody2.
transpile_body([], _, _) ->
    error(empty_body);
%% We can't just return a let expression
transpile_body([{match, Ann, Pat, Expr}], Acc, Env) ->
    %% do NOT register it at this point
    Var = "match_final_" ++ integer_to_list(state_new_id()),
    transpile_body([{match, Ann, {var, Ann, Var}, Expr},
                    {match, Ann, Pat, {var, Ann, Var}},
                    {var, Ann, Var}],
                   Acc,
                   Env);
transpile_body([Expr], Acc, Env) ->
    {transpile_expr(Expr, Env), Acc};
transpile_body([Expr|Rest], Acc, Env) ->
    {PSExpr, Acc1} = transpile_expr(Expr, Acc, Env),
    transpile_body(Rest, [#letval{lvalue = pat_wildcard, rvalue = PSExpr}|Acc1], Env).

catch_partial_lets(L) when is_list(L) ->
    catch_partial_lets(L, []);
catch_partial_lets(Expr = #expr_binop{lop = L, rop = R}) ->
    Expr#expr_binop{lop = catch_partial_lets(L), rop = catch_partial_lets(R)};
catch_partial_lets(#expr_app{function = F, args = Args}) ->
    #expr_app{function = catch_partial_lets(F),
              args = lists:map(fun catch_partial_lets/1, Args)};
catch_partial_lets(#expr_array{value = Arr}) ->
    #expr_array{value = lists:map(fun catch_partial_lets/1, Arr)};
catch_partial_lets(#expr_case{expr = Ex, cases = Cases}) ->
    #expr_case{
       expr = catch_partial_lets(Ex),
       cases =
           [ {Pat, catch_partial_lets(Guards), catch_partial_lets(Cont)}
             || {Pat, Guards, Cont} <- Cases
           ]
      };
catch_partial_lets(Expr = #expr_lambda{body = Body}) ->
    Expr#expr_lambda{body = catch_partial_lets(Body)};
catch_partial_lets(#expr_if{condition = C, then = T, else = E}) ->
    #expr_if{
       condition = catch_partial_lets(C),
       then = catch_partial_lets(T),
       else = catch_partial_lets(E)
     };
catch_partial_lets(#expr_let{letdefs = LDs, in = In}) ->
    catch_partial_lets_in_letdefs(LDs, catch_partial_lets(In));
catch_partial_lets(#expr_do{statements = Stm, return = Ret}) ->
    catch_partial_lets_in_statements(Stm, catch_partial_lets(Ret));
catch_partial_lets(#expr_record{fields = Fields}) ->
    #expr_record{fields = [{Name, catch_partial_lets(Value)}|| {Name, Value} <- Fields]};
catch_partial_lets(#guard_expr{guard = Guard}) ->
    #guard_expr{guard = catch_partial_lets(Guard)};
catch_partial_lets(G = #guard_assg{rvalue = R}) ->
    G#guard_assg{rvalue = catch_partial_lets(R)};
catch_partial_lets(Leaf) ->
    Leaf.

catch_partial_lets([], Acc) ->
    lists:reverse(Acc);
catch_partial_lets([H|T], Acc) ->
    catch_partial_lets(T, [catch_partial_lets(H)|Acc]).

catch_partial_lets_in_statements(Stmts, Ret) ->
    catch_partial_lets_in_statements(Stmts, [], Ret).
catch_partial_lets_in_statements([], Acc, Ret) ->
    #expr_do{statements = lists:reverse(Acc), return = Ret};
catch_partial_lets_in_statements(
  [#do_let{lvalue = LV, rvalue = RV, guards = Guards}|Rest], Acc, Ret)
  when (element(1, LV) =/= pat_var andalso LV =/= pat_wildcard) orelse (Guards =/= []) ->
    FixedRV = catch_partial_lets(RV),
    #expr_do{
       statements =
           lists:reverse(Acc),
       return =
           #expr_case{
              expr = FixedRV,
              cases =
                  [ {LV, Guards, catch_partial_lets_in_statements(Rest, Ret)}
                  , {pat_wildcard, [], ?badmatch(FixedRV)}
                  ]
             }
      };
catch_partial_lets_in_statements(
  [Stmt = #do_bind{rvalue = RV}|Rest], Acc, Ret) ->
    catch_partial_lets_in_statements(
      Rest, [Stmt#do_bind{rvalue = catch_partial_lets(RV)}|Acc], Ret);
catch_partial_lets_in_statements(
  [Stmt = #do_let{rvalue = RV, guards = Guards}|Rest], Acc, Ret) ->
    catch_partial_lets_in_statements(
      Rest, [Stmt#do_let{rvalue = catch_partial_lets(RV), guards = Guards}|Acc], Ret);
catch_partial_lets_in_statements(
  [#do_expr{expr = Expr}|Rest], Acc, Ret) ->
    catch_partial_lets_in_statements(
      Rest, [#do_bind{lvalue = pat_wildcard, rvalue = catch_partial_lets(Expr)}|Acc], Ret).

catch_partial_lets_in_letdefs(Stmts, Ret) ->
    catch_partial_lets_in_letdefs(Stmts, [], Ret).
catch_partial_lets_in_letdefs([], Acc, Ret) ->
    #expr_let{letdefs = lists:reverse(Acc), in = Ret};
catch_partial_lets_in_letdefs(
  [#letval{lvalue = LV, guards = Guards, rvalue = RV}|Rest], Acc, Ret)
  when (element(1, LV) =/= pat_var andalso LV =/= pat_wildcard) orelse Guards =/= [] ->
    FixedRV = catch_partial_lets(RV),
    Cont = #expr_case{
              expr = FixedRV,
              cases =
                  [ {LV, catch_partial_lets(Guards), catch_partial_lets_in_letdefs(Rest, Ret)}
                  , {pat_wildcard, [], ?badmatch(FixedRV)}
                  ]
             },
    case Acc of
        [] -> Cont;
        _ -> #expr_let{
                letdefs = lists:reverse(Acc),
                in = Cont
               }
    end;
catch_partial_lets_in_letdefs(
  [LV = #letval{rvalue = RV}|Rest], Acc, Ret) ->
    catch_partial_lets_in_letdefs(
      Rest, [LV#letval{rvalue = catch_partial_lets(RV)}|Acc], Ret);
catch_partial_lets_in_letdefs(
  [LF = #letfun{body = Body, guards = Guards}|Rest], Acc, Ret) ->
    catch_partial_lets_in_letdefs(
      Rest, [LF#letfun{body = catch_partial_lets(Body),
                       guards = catch_partial_lets(Guards)
                      }|Acc], Ret).

transpile_expr(Expr, Env) ->
    {PSExpr, LetDefs} = transpile_expr(Expr, [], Env),
    case LetDefs of
        [] -> PSExpr;
        _ ->
            apply_assignments(LetDefs, PSExpr)
    end.


transpile_expr({atom, _, Atom}, LetDefs, _Env) ->
    {?make_expr_atom(Atom), LetDefs};
transpile_expr({var, _, Var}, LetDefs, Env) ->
    { case Env of
          #env{raw_functions = #{Var := {Name, Arity}}} ->
              #expr_app{
                 function = #expr_var{name = "ErlangFun"},
                 args = [#expr_num{value = Arity}, #expr_var{name = Name}]
                };
          _ -> #expr_var{name = state_get_var(Var)}
      end
    , LetDefs};

transpile_expr({integer, _, Int}, LetDefs, _Env) ->
    {?make_expr_int(Int), LetDefs};
transpile_expr({float, _, Float}, LetDefs, _Env) ->
    {?make_expr_float(Float), LetDefs};
transpile_expr({char, Ann, Int}, LetDefs, Env) ->
    transpile_expr({integer, Ann, Int}, LetDefs, Env);
transpile_expr({string, _, String}, LetDefs, _Env) ->
    {#expr_app{
        function = #expr_var{name = "toErl"},
        args = [#expr_string{value = String}]},
     LetDefs
    };

transpile_expr({bin, _, Bin}, LetDefs, Env) ->
    transpile_binary_expression_segments(Bin, LetDefs, Env);
transpile_expr({block, _, Body}, LetDefs, Env) ->
    transpile_body(Body, LetDefs, Env);
transpile_expr({match, _, {var, _, [$_ | _]}, Expr}, LetDefs, Env) ->
    %% When matching to a wildcard pattern we may skip the case statement
    transpile_expr(Expr, LetDefs, Env);
transpile_expr({match, _, Pat, Val}, LetDefs0, Env) ->
    {ValueExpr, LetDefs1} = transpile_expr(Val, LetDefs0, Env),
    {PSPats, PSGuards} = transpile_pattern_sequence([Pat], Env),
    state_pop_discard_var_stack(), %% This permanently commits the bindings to the local scope
    case {PSPats, PSGuards} of
        {[#pat_var{name = Var}], []} ->
            {#expr_var{name = Var},
             [#letval{lvalue = #pat_var{name = Var}, rvalue = ValueExpr} | LetDefs1]};
        {[PSPat], _} ->
            Var = state_create_fresh_var("matchExpr"),
            { #expr_var{name = Var},
              [ #letval{lvalue = PSPat, guards = PSGuards, rvalue = #expr_var{name = Var}}
              , #letval{lvalue = #pat_var{name = Var}, rvalue = ValueExpr}
              | LetDefs1]}
    end;
%% TODO Scope leaking!
transpile_expr({'if', _, Clauses}, LetDefs, Env) ->
    PSCases =
        [begin
             state_push_var_stack(),
             Guards = case GuardSequence of
                          [[{atom, _, true}]] ->
                              [];
                          _ ->
                              transpile_boolean_guards(GuardSequence, Env)
                      end,
             R = {pat_wildcard,
                  Guards,
                  transpile_body(Body, Env)},
                   state_pop_var_stack(),
             R
         end || {clause, _, [], GuardSequence, Body} <- Clauses],
    {#expr_case{
        expr = ?make_expr_atom(true),
        cases = PSCases ++
            case [found || {#pat_constr{constr = "ErlangAtom",
                                        args = #pat_string{value = "true"}
                                       }
                           , [], _} <- PSCases ] of
                [] -> [{pat_wildcard, [], ?if_clause}];
                _ -> []
            end
       }, LetDefs};
%% TODO Scope leaking!
transpile_expr({'case', _, Expr, Clauses}, LetDefs0, Env) ->
    {ExprVar, LetDefs1} = bind_expr("case", Expr, LetDefs0, Env),
    UserCases = [ begin
                      {[PSPat], PSGuards} = transpile_pattern_sequence(Pat, Env),
                      R = {PSPat, PSGuards ++ transpile_boolean_guards(Guards, Env),
                           transpile_body(Cont, Env)},
                      state_pop_var_stack(),
                      R
                  end
            || {clause, _, Pat, Guards, Cont} <- Clauses
           ],
    Cases = case UserCases of
              [] ->
                    [{#pat_var{name = "something_else"}, [],
                      ?case_clause(#expr_var{name = "something_else"})}];
              _ ->
                case lists:last(UserCases) of
                    %%{#pat_var{} =V, [], _} ->
                    %%    UserCases;
                    _ ->
                        UserCases ++
                            [{#pat_var{name = "something_else"}, [],
                              ?case_clause(#expr_var{name = "something_else"})}]
                end
            end,
    Case = #expr_case{
       expr = #expr_var{name = ExprVar},
       cases = Cases
      },
    {Case, LetDefs1};


transpile_expr({op, _, Andalso, L, R}, LetDefs0, Env)
  when Andalso =:= 'andalso'; Andalso =:= "andalso" ->
    {LVar, LetDefs1} = bind_expr("lop", L, LetDefs0, Env),
    Expr =
        #expr_case{
           expr = #expr_var{name = LVar},
           cases =
               [ {?make_pat_atom(false), [], ?make_expr_atom(false)}
               , {?make_pat_atom(true), [], transpile_expr(R, Env)} % TODO SCOPE LEAKING :)))
               , {pat_wildcard, [], ?badarg(#expr_var{name = LVar})}
               ]
          },
    {Expr, LetDefs1};
transpile_expr({op, _, Orelse, L, R}, LetDefs0, Env)
  when Orelse =:= 'orelse'; Orelse =:= "orelse" ->
    {LVar, LetDefs1} = bind_expr("lop", L, LetDefs0, Env),
    Expr =
        #expr_case{
           expr = #expr_var{name = LVar},
           cases =
               [ {?make_pat_atom(true), [], ?make_expr_atom(true)}
               , {?make_pat_atom(false), [], transpile_expr(R, Env)} % TODO SCOPE LEAKING :)))
               , {pat_wildcard, [], ?badarg(#expr_var{name = LVar})}
               ]
          },
    {Expr, LetDefs1};
transpile_expr({op, _, Op, L, R}, LetDefs0, Env) ->
    {direct, OpFun} = transpile_fun_ref("erlang", Op, 2, Env),
    {LVar, LetDefs1} = bind_expr("lop", L, LetDefs0, Env),
    {RVar, LetDefs2} = bind_expr("rop", R, LetDefs1, Env),
    {#expr_app{
        function = OpFun,
        args = [#expr_array{
                   value =
                       [#expr_var{name = LVar},
                        #expr_var{name = RVar}
                       ]}]},
     LetDefs2
    };
transpile_expr({op, _, Op, Arg}, LetDefs0, Env) ->
  {direct, OpFun} = transpile_fun_ref("erlang", Op, 1, Env),
    {ArgVar, LetDefs1} = bind_expr("op_arg", Arg, LetDefs0, Env),
    {#expr_app{
        function = OpFun,
        args = [#expr_array{value = [#expr_var{name = ArgVar}]}]},
     LetDefs1
    };

transpile_expr({call, Ann, {remote, _, {atom, _, erlang}, {atom, AnnA, is_record}},
                Args = [_, {atom, _, _}]}, LetDefs, Env) ->
    transpile_expr({call, Ann, {atom, AnnA, is_record}, Args}, LetDefs, Env);
transpile_expr({call, _, {atom, _, is_record}, [Arg1, {atom, _, Record}]},
               LetDefs0, Env = #env{records = Records}) ->
    {ArgVar, LetDefs1} = bind_expr("perhaps_a_record", Arg1, LetDefs0, Env),
    case maps:get(Record, Records, undefined) of
        undefined -> error({undefined_record, Record});
        Fields ->
            Arity = length(Fields),
            { #expr_app{
                 function = #expr_var{name = "BIF.erlang__is_record__3"},
                 args =
                     [#expr_array{
                        value =
                            [ #expr_var{name = ArgVar}
                            , ?make_expr_atom(Record)
                            , ?make_expr_int(Arity + 1)
                            ]
                       }]
                }
            , LetDefs1
            }
    end;
transpile_expr({call, _, {atom, _, record_info}, [{atom, _, size}, {atom, _, Record}]},
               LetDefs0, #env{records = Records}) ->
    case maps:get(Record, Records, undefined) of
        undefined -> error({undefined_record, Record});
        Fields ->
            {?make_expr_int(length(Fields) + 1), LetDefs0}
    end;
transpile_expr({call, _, {atom, _, record_info}, [{atom, _, fields}, {atom, _, Record}]},
               LetDefs0, #env{records = Records}) ->
    case maps:get(Record, Records, undefined) of
        undefined -> error({undefined_record, Record});
        Fields ->
            {make_expr_list([?make_expr_atom(Field) || {Field, _} <- Fields]), LetDefs0}
    end;
transpile_expr({call, _, {atom, _, Fun}, Args}, LetDefs0, Env) ->
    {ArgsVars, LetDefs1} = bind_exprs("arg", Args, LetDefs0, Env),
    case transpile_fun_ref(Fun, length(Args), Env) of
      {direct, PSFun} ->
        { #expr_app{
             function = PSFun,
             args = [#expr_array{value = [#expr_var{name = ArgVar} || ArgVar <- ArgsVars]}]}
        , LetDefs1
        };
      {code_server, MName, FName} ->
        { #expr_app{
               function = #expr_var{name = "BIF.do_remote_fun_call"},
               args = [ #expr_string{value = MName}
                      , #expr_string{value = FName}
                      , #expr_array{value = [#expr_var{name = ArgVar} || ArgVar <- ArgsVars]}
                      ]}
          , LetDefs1
          }
      end;
transpile_expr({call, _, {remote, _, {atom, _, Module0}, {atom, _, Fun}}, Args},
               LetDefs0, Env) ->
    Module1 = case Module0 of
                'io' -> "erlang_io";
                %'io_lib' -> "erlang_iolib";
                'unicode' -> "erlang_unicode";
                _ -> Module0
              end,
    {ArgsVars, LetDefs1} = bind_exprs("arg", Args, LetDefs0, Env),
    case transpile_fun_ref(Module1, Fun, length(Args), Env) of
      {direct, PSFun} ->
          { #expr_app{
               function = PSFun,
               args = [#expr_array{value = [#expr_var{name = ArgVar} || ArgVar <- ArgsVars]}]}
          , LetDefs1
          };
      {code_server, MName, FName} ->
          { #expr_app{
               function = #expr_var{name = "BIF.do_remote_fun_call"},
               args = [ #expr_string{value = MName}
                      , #expr_string{value = FName}
                      , #expr_array{value = [#expr_var{name = ArgVar} || ArgVar <- ArgsVars]}
                      ]}
          , LetDefs1
          }
    end;
transpile_expr({call, Ann, {remote, _, MVar, FVar}, Args0},
               LetDefs, Env) ->
    Args1 = lists:foldr(fun (Arg, Acc) -> {cons, Ann, Arg, Acc}
                 end, {nil, Ann}, Args0),
    transpile_expr(
      {call, Ann, {remote, Ann, {atom, Ann, erlang}, {atom, Ann, apply}}, [MVar, FVar, Args1]},
      LetDefs, Env);
transpile_expr({call, _, Fun, Args}, LetDefs0, Env) ->
    {ArgsVars, LetDefs1} = bind_exprs("arg", Args, LetDefs0, Env),
    {FunVar, LetDefs2} = bind_expr("fun", Fun, LetDefs1, Env),
    IfNotVar =
      #expr_app{
         function = #expr_var{name = "BIF.erlang__apply__2"},
         args =
             [#expr_array{
                 value =
                     [
                      #expr_var{name = FunVar},
                      lists:foldr(fun (ArgVar, Acc) -> ?make_expr_cons(?make_expr_var(ArgVar), Acc)
                                  end, ?make_expr_empty_list, ArgsVars)
                     ]
                }]},
    case Fun of
        {var, _, Var} ->
            case Env of
                #env{raw_functions = #{Var := {Name, _}}} ->
                    {#expr_app{function = #expr_var{name = Name},
                               args = [#expr_array{
                                          value = [#expr_var{name = ArgVar} || ArgVar <- ArgsVars]}]
                              }, LetDefs1};
                _ -> {IfNotVar, LetDefs2}
            end;
        _ -> {IfNotVar, LetDefs2}
    end;

transpile_expr({nil, _}, LetDefs, _) ->
    {?make_expr_empty_list, LetDefs};
transpile_expr({cons, _, H, T}, LetDefs0, Env) ->
    {VarH, LetDefs1} = bind_expr("head", H, LetDefs0, Env),
    {VarT, LetDefs2} = bind_expr("tail", T, LetDefs1, Env),
    { ?make_expr_cons(#expr_var{name = VarH}, #expr_var{name = VarT})
    , LetDefs2
    };

transpile_expr({'fun', _, {function, Fun, Arity}}, LetDefs, Env) when is_atom(Fun) ->
    {direct, FRef} = transpile_fun_ref(Fun, Arity, Env),
    {#expr_app{function = #expr_var{name = "ErlangFun"},
               args = [#expr_num{value = Arity},
                       FRef]},
     LetDefs
    };
transpile_expr({'fun', Ann, {function, Module, Fun, Arity}},
               LetDefs, Env) ->
    transpile_expr(
      {call, Ann, {remote, Ann, {atom, Ann, erlang}, {atom, Ann, make_fun}}, [Module, Fun, Arity]},
      LetDefs, Env);
transpile_expr({'fun', _, {clauses, Clauses = [{clause, _, SomeArgs, _, _}|_]}}, LetDefs, Env0) ->
    FunVar = state_create_fresh_var("lambda"),
    Arity = length(SomeArgs),
    CatchVars = [state_create_fresh_var("arg") || _ <- lists:seq(1, Arity)],
    FunctionClause =
        #letfun{
           name = FunVar,
           args = [#pat_array{
                      value = [#pat_var{name = Var} || Var <- CatchVars]}],
           body = ?function_clause
          },
    BadArity =
        #letfun{
           name = FunVar,
           args = [#pat_var{name = "args"}],
           body = ?badarity
                     ( ?make_expr_fun(Arity, #expr_var{name = FunVar})
                     , #expr_var{name = "args"})
          },
    Lambda =
        #expr_app{
           function = #expr_var{name = "ErlangFun"},
           args =
               [ #expr_num{value = Arity}
               , #expr_let{
                    letdefs =
                        [ begin
                              PrevVars = state_get_vars(),
                              {PSArgs, PSGuards0} =
                                  transpile_pattern_sequence(
                                    Args,
                                    Env0#env{
                                      raw_functions = #{},
                                      overriden_pattern_vars =
                                          sets:from_list(maps:keys(PrevVars))
                                     }),
                              BodyEnv =
                                  Env0#env {
                                    raw_functions =
                                        maps:filter(
                                          fun(N, _ ) ->
                                                  maps:get(N, PrevVars, undefined)
                                                      == maps:get(N, state_get_vars(), undefined)
                                          end,
                                          Env0#env.raw_functions
                                         )
                                   },
                              PSGuards1 = PSGuards0 ++ transpile_boolean_guards(Guards, Env0),
                              PSBody = transpile_body(Cont, BodyEnv),
                              state_pop_var_stack(),
                              #letfun{ name = FunVar
                                     , args = [#pat_array{value = PSArgs}]
                                     , guards = PSGuards1
                                     , body = PSBody
                                     }
                          end
                          || {clause, _, Args, Guards, Cont} <- Clauses
                        ] ++ [FunctionClause, BadArity],
                    in = #expr_var{name = FunVar}
                   }
               ]},
    {Lambda, LetDefs
    };
transpile_expr({'named_fun', _, Name, Clauses = [{clause, _, SomeArgs, _, _}|_]}, LetDefs, Env0) ->
    FunVar = state_create_fresh_var(string:to_lower(lists:flatten(io_lib:format("~s", [Name])))),
    Arity = length(SomeArgs),
    CatchVars = [state_create_fresh_var("arg") || _ <- lists:seq(1, Arity)],
    FunctionClause =
        #letfun{
           name = FunVar,
           args = [#pat_array{
                      value = [#pat_var{name = Var} || Var <- CatchVars]}],
           body = ?function_clause
          },
    BadArity =
        #letfun{
           name = FunVar,
           args = [#pat_var{name = "args"}],
           body = ?badarity
                     ( ?make_expr_fun(Arity, #expr_var{name = FunVar})
                     , #expr_var{name = "args"})
          },
    Lambda =
        #expr_app{
           function = #expr_var{name = "ErlangFun"},
           args =
               [ #expr_num{value = Arity}
               , #expr_let{
                    letdefs =
                        [ begin
                              PrevVars = state_get_vars(),
                              {PSArgs, PSGuards0} =
                                  transpile_pattern_sequence(
                                    Args, Env0#env{raw_functions = #{},
                                                   overriden_pattern_vars =
                                                       sets:from_list(maps:keys(PrevVars))
                                                  }),
                              NameWasShadowed =
                                  fun(N) -> maps:get(N, PrevVars, undefined)
                                                =/= maps:get(N, state_get_vars(), undefined)
                                  end,
                              Env1 =
                                  Env0#env {
                                    raw_functions =
                                        maps:filter(
                                          fun(N, _ ) -> not NameWasShadowed(N) end,
                                          Env0#env.raw_functions
                                         )
                                   },
                              BodyEnv =
                                  case NameWasShadowed(Name) of
                                      false ->
                                          state_put_var(Name, FunVar), %% recursion
                                          Env1#env{
                                            raw_functions = (Env1#env.raw_functions)#{Name => {FunVar, Arity}}
                                           };
                                      true -> Env1
                                  end,
                              PSGuards1 = PSGuards0 ++ transpile_boolean_guards(Guards, Env0),
                              PSBody = transpile_body(Cont, BodyEnv),
                              state_pop_var_stack(),
                              #letfun{ name = FunVar
                                     , args = [#pat_array{value = PSArgs}]
                                     , guards = PSGuards1
                                     , body = PSBody
                                     }
                          end
                          || {clause, _, Args, Guards, Cont} <- Clauses
                        ] ++ [FunctionClause, BadArity],
                    in = #expr_var{name = FunVar}
                   }
               ]},
    {Lambda, LetDefs};

transpile_expr({tuple, _, Exprs}, LetDefs0, Env) ->
    {ExprsVars, LetDefs1} = bind_exprs("tup_el", Exprs, LetDefs0, Env),
    {#expr_app{
        function = #expr_var{name = "ErlangTuple"},
        args = [#expr_array{value = [#expr_var{name = ExprVar} || ExprVar <- ExprsVars]}]}
    , LetDefs1
    };

transpile_expr({record, Ann, RecordName, RecordFields}, LetDefs, Env) ->
    %% Convert this to a tuple
    Values = [record_fields(X) || X <- RecordFields],
    Fields = [{atom, Ann, RecordName}] ++
        [proplists:get_value(FieldName, Values, Default) ||
            {FieldName, Default} <- maps:get(RecordName, Env#env.records)],
    transpile_expr({tuple, Ann, Fields}, LetDefs, Env);
transpile_expr({record, _, Expr, RecordName, RecordFields}, LetDefs0, Env) ->
    RecordFieldsProplist =
        [ {atom_to_list(Field), Val}
          || {record_field, _, {atom, _, Field}, Val} <- RecordFields
        ],
    {ExprVar, LetDefs1} = bind_expr("record", Expr, LetDefs0, Env),
    {FieldValueVars, LetDefs2} =
        bind_exprs("record_updt", [Val || {_, Val} <- RecordFieldsProplist], LetDefs1, Env),
    UpdatesProplist =
        lists:zip(
          [Field || {Field, _} <- RecordFieldsProplist],
          FieldValueVars
         ),

    AllRecordFields =
        [atom_to_list(Field) || {Field, _} <- maps:get(RecordName, Env#env.records)],

    FieldPats =
        [ #pat_var{name = state_create_fresh_var(FieldName)}
         || FieldName <- AllRecordFields],
    {#expr_case{
        expr = #expr_var{name = ExprVar},
        cases =
            [ { ?make_pat_tuple([?make_pat_atom(RecordName)|FieldPats])
              , []
              , ?make_expr_tuple(
                   [ ?make_expr_atom(RecordName)
                   | [ case proplists:get_value(FieldName, UpdatesProplist) of
                           undefined -> #expr_var{name = Var};
                           NewVar -> #expr_var{name = NewVar}
                       end
                       || {#pat_var{name = Var}, FieldName}
                              <- lists:zip(FieldPats, AllRecordFields)
                     ]
                   ]
                  )
              }
            , {pat_wildcard, [], ?badrecord(?make_expr_atom(RecordName))}
            ]
       },
     LetDefs2
    };
transpile_expr({record_field, _, Expr, Record, {atom, _, Field}}, LetDefs0, Env) ->
    {ExprVar, LetDefs1} = bind_expr("record", Expr, LetDefs0, Env),

    AllRecordFields =
        [FieldName || {FieldName, _} <- maps:get(Record, Env#env.records)],
    FindIndex =
        fun R([FieldName|_], N) when FieldName == Field ->
                N;
            R([_|Rest], N) ->
                R(Rest, N + 1);
            R([], _) ->
                error({field_not_in_record, Record, Field})
        end,
    Index = FindIndex(AllRecordFields, 1),

    FieldVar = state_create_fresh_var("field"),
    ArrVar = state_create_fresh_var("arr"),
    {#expr_case{
        expr = #expr_var{name = ExprVar},
        cases =
            [ {#pat_constr{constr = "ErlangTuple", args = [#pat_var{name = ArrVar}]}
              , [#guard_assg{
                    lvalue = #pat_constr{
                                constr = "DM.Just",
                                args = [#pat_var{name = FieldVar}]
                               },
                    rvalue = #expr_binop{
                                name = "DA.!!",
                                lop = #expr_var{name = ArrVar},
                                rop = #expr_num{value = Index}
                               }
                   }]
              , #expr_var{name = FieldVar}
              }
            , {pat_wildcard, [], ?badrecord(?make_expr_atom(Record))}
            ]
       },
     LetDefs1
    };
transpile_expr({record_index, _, Record, {atom, _, Field}}, LetDefs, Env) ->
    AllRecordFields =
        [FieldName || {FieldName, _} <- maps:get(Record, Env#env.records)],
    FindIndex =
        fun R([FieldName|_], N) when FieldName == Field ->
                N;
            R([_|Rest], N) ->
                R(Rest, N + 1);
            R([], _) ->
                error({field_not_in_record, Record, Field})
        end,
    %% It counts tuple elements from 1
    {?make_expr_int(FindIndex(AllRecordFields, 1) + 1),
     LetDefs
    };

transpile_expr({lc, _, Ret, []}, LetDefs0, Env) ->
    {RetVar, LetDefs1} = bind_expr("lcRet", Ret, LetDefs0, Env),
    {make_expr_list([#expr_var{name = RetVar}]),
     LetDefs1
    };
transpile_expr({lc, _, Ret, [{generate, Ann, Pat, Source}|Rest]}, LetDefs0, Env) ->
    {SourceVar, LetDefs1} = bind_expr("lcSrc", Source, LetDefs0, Env),
    PrevVars = state_get_vars(),
    {[PSPat], Guards} = transpile_pattern_sequence(
                          [Pat], Env#env{
                                   overriden_pattern_vars =
                                       sets:from_list(maps:keys(PrevVars))
                                  }),
    Var = state_create_fresh_var("lc"),
    Gen =
        #expr_app{
           function = #expr_var{name = "flmap"},
           args =
               [#expr_lambda{
                   args = [#pat_var{name = Var}],
                   body =
                       #expr_case{
                          expr = #expr_var{name = Var},
                          cases =
                              [ {PSPat, Guards, transpile_expr({lc, Ann, Ret, Rest}, Env)}
                              , {pat_wildcard, [], ?make_expr_empty_list}
                              ]
                         }
                  }
               ,  #expr_var{name = SourceVar}
               ]
          },
    state_pop_var_stack(),
    {Gen, LetDefs1};
transpile_expr({lc, Ann, Ret, [Expr|Rest]}, LetDefs0, Env) ->
    {Var, LetDefs1} = bind_expr("cond", Expr, LetDefs0, Env),
    {#expr_case{
        expr = #expr_var{name = Var},
        cases = [ {?make_pat_atom(true), [], transpile_expr({lc, Ann, Ret, Rest}, Env)}
                , {pat_wildcard, [], ?make_expr_empty_list}
                ]
       },
     LetDefs1
    };

transpile_expr({bc, Ann, Ret, Generators}, LetDefs0, Env) ->
    {LCExpr, LetDefs1} = transpile_expr({lc, Ann, Ret, Generators}, LetDefs0, Env),
    { #expr_app{function = #expr_var{name = "BIN.concatErl"},
                args = [LCExpr]
               }
    , LetDefs1
    };

transpile_expr({map, _, Associations}, LetDefs0, Env) ->
    {Keys, Vals} = lists:unzip([{Key, Val} || {_, _, Key, Val} <- Associations]),
    {KeysVars, LetDefs1} = bind_exprs("key", Keys, LetDefs0, Env),
    {ValsVars, LetDefs2} = bind_exprs("val", Vals, LetDefs1, Env),
    Map = #expr_app{
             function = #expr_var{name = "Map.fromFoldable"},
             args =
                 [#expr_array{
                     value =
                         [ #expr_app{function = #expr_var{name = "DT.Tuple"},
                                     args = [#expr_var{name = KeyVar},
                                             #expr_var{name = ValVar}]
                                    }
                           || {KeyVar, ValVar} <- lists:zip(KeysVars, ValsVars)
                         ]
                    }
                 ]},
    {?make_expr_map(Map),
     LetDefs2
    };
transpile_expr({map, Ann, Map, Associations}, LetDefs0, Env) ->
    {MapVar, LetDefs1} = bind_expr("map", Map, LetDefs0, Env),
    {Ext, LetDefs2} = transpile_expr({map, Ann, Associations}, LetDefs1, Env),
    ExtVar = state_create_fresh_var("mapExt"),
    Exacts = [Key || {map_field_exact, _, Key, _} <- Associations],
    {ExactsVars, LetDefs3} = bind_exprs("exact_key", Exacts, LetDefs2, Env),
    {direct, FRef} = transpile_fun_ref(maps, merge, 2, Env),
    MergeExpr =
        #expr_app{
           function = FRef,
           args = [#expr_array{value = [#expr_var{name = MapVar},
                                        #expr_var{name = ExtVar}]}]},
    MissingKeyVar = state_create_fresh_var("missing"),
    {case ExactsVars of
         [] -> MergeExpr;
         _ ->
             #expr_case{
                expr =
                    #expr_app{
                       function = #expr_var{name = "findMissingKey"},
                       args = [#expr_var{name = MapVar},
                               #expr_array{value = [#expr_var{name = V} || V <- ExactsVars]}
                              ]
                      },
                cases =
                    [ {#pat_constr{constr = "DM.Nothing"}, [],
                       MergeExpr
                      }
                    , {#pat_constr{constr = "DM.Just",
                                   args = [#pat_var{name = MissingKeyVar}]}, [],
                       ?badkey(#expr_var{name = MissingKeyVar})
                      }
                    ]
               }
     end,
     [ #letval{lvalue = #pat_var{name = ExtVar}, rvalue = Ext}
     | LetDefs3]
    };


transpile_expr({'try', _, ExprBlock, Clauses, Catches, After}, LetDefs, Env) ->
    PSExpr = transpile_body(ExprBlock, Env),
    Defer = fun(E) -> #expr_lambda{args = [pat_wildcard], body = E} end,
    OfVar = state_create_fresh_var("of"),
    ExVar1 = state_create_fresh_var("ex"),
    ExVar2 = state_create_fresh_var("ex"),
    OfHandler =
        case Clauses of
            [] -> no_of;
            _ -> #expr_lambda
                     { args = [#pat_var{name = OfVar}]
                     , body = #expr_case
                       { expr = #expr_var{name = OfVar}
                       , cases =
                             [ begin
                                   {[PSPat], PSGuards} = transpile_pattern_sequence(Pat, Env),
                                   R = {PSPat, PSGuards ++ transpile_boolean_guards(Guards, Env),
                                        transpile_body(Cont, Env)},
                                   state_pop_var_stack(),
                                   R
                               end
                              || {clause, _, Pat, Guards, Cont} <- Clauses
                             ] ++ [{#pat_var{name = "something_else"}, [],
                                    ?try_clause(#expr_var{name = "something_else"})}]
                       }
                     }
        end,
    ExHandler =
        #expr_lambda
        { args = [#pat_var{name = ExVar1}]
        , body = #expr_case
          { expr = #expr_var{name = ExVar1}
          , cases =
                [ begin
                      {[PSPat], PSGuards0} = transpile_pattern_sequence(Pat, Env),
                      PSGuards1 = transpile_boolean_guards(Guards, Env),
                      PSCont = transpile_body(Cont, Env),
                      state_pop_var_stack(),
                      { PSPat
                      , PSGuards0 ++ PSGuards1
                      , PSCont
                      }
                  end
                  || {clause, _, Pat, Guards, Cont} <- Catches
                ] ++ [{#pat_var{name = ExVar2}, [],
                      #expr_app{
                         function = #expr_var{name = "EXC.raise"},
                         args = [#expr_var{name = ExVar2}]
                        }
                     }]
          }
        },
    Res =
        case {After, OfHandler} of
            {[], no_of} ->
                #expr_app{
                   function = #expr_var{name = "EXC.tryCatch"},
                   args = [Defer(PSExpr), ExHandler]
                  };
            {[], _} ->
                #expr_app{
                   function = #expr_var{name = "EXC.tryOfCatch"},
                   args = [Defer(PSExpr), OfHandler, ExHandler]
                  };
            {_, no_of} ->
                #expr_app{
                   function = #expr_var{name = "EXC.tryCatchFinally"},
                   args = [Defer(PSExpr), ExHandler, Defer(transpile_body(After, Env))]
                  };
            {_, _} ->
                #expr_app{
                   function = #expr_var{name = "EXC.tryOfCatchFinally"},
                   args = [Defer(PSExpr), OfHandler, ExHandler, Defer(transpile_body(After, Env))]
                  }
        end,
    {Res, LetDefs};
transpile_expr({'catch', Ann, Expr}, LetDefs, Env) ->
    transpile_expr(
      { 'try', Ann
      , [Expr]
      , []
      , [ { clause, Ann
          , [{tuple, Ann, [{atom, Ann, throw}, {var, Ann, 'payload'}, {var, Ann, '_'}]}]
          , [], [{var, Ann, 'payload'}]
          }
        , { clause, Ann
          , [{tuple, Ann, [{atom, Ann, error}, {var, Ann, 'payload'}, {var, Ann, 'stack'}]}]
          , [], [{tuple, Ann, [{atom, Ann, 'EXIT'},
                              {tuple, Ann, [{var, Ann, 'payload'}, {var, Ann, 'stack'}]}]}]
          }
        , { clause, Ann
          , [{tuple, Ann, [{atom, Ann, exit}, {var, Ann, 'payload'}, {var, Ann, '_'}]}]
          , [], [{tuple, Ann, [{atom, Ann, 'EXIT'}, {var, Ann, 'payload'}]}]
          }
        ]
      , []
      },
      LetDefs, Env
     );

transpile_expr(X, _LetDefs, _Env) ->
    error({unimplemented_expr, X}).


transpile_binary_expression_segments(Bin, LetDefs, Env) ->
    transpile_binary_expression_segments(Bin, [], LetDefs, Env).
transpile_binary_expression_segments([], Acc, LetDefs, _) ->
    { ?make_expr_bin(#expr_app{
         function = #expr_var{name = "BIN.concat"},
         args = [#expr_array{ value = lists:reverse(Acc)}]
        })
    , LetDefs};
transpile_binary_expression_segments(
  [{bin_element, _, Element, Size, Spec}|Rest], Acc, LetDefs0, Env) ->
    case {Element, Size, parse_bin_segment_spec(Element, Spec)} of

        {{string, _, S}, _, #{unit := Unit, endian := Endian}} ->
            {SizeExpr, LetDefs1} =
                case Size of
                    default -> {?make_expr_int(8), LetDefs0};
                    _ -> transpile_expr(Size, LetDefs0, Env)
                end,
            transpile_binary_expression_segments(
              Rest,
              [ #expr_app{
                   function = ?make_expr_var("BIN.fromInts"),
                   args =
                       [ #expr_app{
                            function = #expr_var{name = "toErl"},
                            args = [#expr_string{value = S}]
                           }
                       , SizeExpr
                       , #expr_num{value = Unit}
                       , #expr_var{name = case Endian of
                                              big -> "BIN.Big";
                                              little -> "BIN.Little" end
                                  }
                       ]
                  }
              | Acc],
              LetDefs1, Env);

        {_, _, #{type := Type, unit := Unit, endian := Endian}} ->
            {ExprVar, LetDefs1} = bind_expr("bin_el", Element, LetDefs0, Env),
            {SizeExpr, LetDefs2} =
                case Size of
                    default ->
                        case Type of
                            integer -> {?make_expr_int(8), LetDefs1};
                            float -> {?make_expr_int(64), LetDefs1};
                            B0 when B0 =:= binary orelse B0 =:= bitstring ->
                                {#expr_app{function = #expr_var{name = "BIN.packedSize"},
                                           args = [#expr_var{name = ExprVar}]
                                          },
                                 LetDefs1}
                        end;
                    _ -> transpile_expr(Size, LetDefs1, Env)
                end,
            Chunk =
                case Type of
                    integer ->
                        #expr_app{
                           function = #expr_var{name = "BIN.fromInt"},
                           args =
                               [ #expr_var{name = ExprVar}
                               , SizeExpr
                               , #expr_num{value = Unit}
                               , #expr_var{name = case Endian of
                                                      big -> "BIN.Big";
                                                      little -> "BIN.Little" end
                                          }
                                  ]
                          };
                    float ->
                        #expr_app{
                           function = #expr_var{name = "BIN.fromFloat"},
                           args =
                               [ #expr_var{name = ExprVar}
                               , SizeExpr
                               , #expr_num{value = Unit}
                               , #expr_var{name = case Endian of
                                                      big -> "BIN.Big";
                                                      little -> "BIN.Little" end
                                          }
                               ]
                          };
                    B when B =:= binary orelse B =:= bitstring ->
                        #expr_app{
                           function = #expr_var{name = "BIN.binPrefix"},
                           args =
                               [ #expr_var{name = ExprVar}
                               , SizeExpr
                               , #expr_num{value = Unit}
                               ]
                          }
                end,
            transpile_binary_expression_segments(Rest, [Chunk|Acc], LetDefs2, Env)
    end;
transpile_binary_expression_segments([B|_], _, _, _) ->
    error({unsupported_bin_element, B}).


bind_expr(Name, Expr, LetDefs0, Env) when is_atom(Name) ->
    bind_expr(atom_to_list(Name), Expr, LetDefs0, Env);
bind_expr(Name, Expr, LetDefs0, Env) ->
    Var = state_create_fresh_var(Name),
    {PSExpr, LetDefs1} = transpile_expr(Expr, LetDefs0, Env),
    {Var,
     [ #letval{lvalue = #pat_var{name = Var}, rvalue = PSExpr}
     | LetDefs1
     ]}.
bind_exprs(Name, Exprs, LetDefs, Env) ->
    bind_exprs(Name, Exprs, [], LetDefs, Env).
bind_exprs(_, [], Acc, LetDefs, _) ->
    {lists:reverse(Acc), LetDefs};
bind_exprs(Name, [Expr|Rest], Acc, LetDefs0, Env) ->
    {Var, LetDefs1} = bind_expr(Name, Expr, LetDefs0, Env),
    bind_exprs(Name, Rest, [Var|Acc], LetDefs1, Env).



compute_constexpr({string, _, Str}) ->
    {ok, Str};
compute_constexpr({op, _, Op, E}) ->
    case compute_constexpr(E) of
        {ok, V} when is_number(V) -> {ok, (fun erlang:Op/1)(V)};
        _ -> error
    end;
compute_constexpr({op, _, Op, L, R}) -> %% FIXME: float handling needs to be fixed
    case {compute_constexpr(L), compute_constexpr(R)} of
        {{ok, LV}, {ok, RV}}
          when is_number(LV) andalso is_number(RV) andalso
               (Op =:= '+' orelse Op =:= '-' orelse Op =:= '*' orelse Op =:= '/'
                orelse Op =:= 'bsl'
               )
               -> {ok, (fun erlang:Op/2)(LV, RV)};
        _ -> error
    end;
compute_constexpr({integer, _, Num}) ->
    {ok, Num};
compute_constexpr({float, _, Num}) ->
    {ok, Num}.

apply_assignments([], Expr) ->
    Expr;
apply_assignments([Letdef|Rest], Expr) ->
    apply_assignments(Rest, #expr_let{letdefs = [Letdef], in = Expr}).


%% Hacky emulation of a state monad using the process dictionary :P
-define(BINDINGS, var_bindings).
-define(BINDINGS_STACK, var_bindings_stack).
%% Id supply (basically a counter, todo: implement as a petri net)
state_reset_supply_id() ->
    persistent_term:put({?MODULE, ctr}, counters:new(1, [])).
state_new_id() ->
    Ctr = persistent_term:get({?MODULE, ctr}),
    Id = counters:get(Ctr, 1),
    counters:add(Ctr, 1, 1),
    Id.
%% Variable bindings
state_clear_vars() ->
    put(?BINDINGS, #{}).
state_get_vars() ->
    get(?BINDINGS).
state_put_var(ErlangVar, PsVar) ->
    put(?BINDINGS, maps:put(ErlangVar, PsVar, state_get_vars())).
state_create_fresh_var(Name) ->
    lists:flatten(io_lib:format("~s_~p", [Name, state_new_id()])).
state_is_used(ErlangVar) ->
    maps:is_key(ErlangVar, state_get_vars()).
state_get_var(ErlangVar) ->
    try maps:get(ErlangVar, state_get_vars())
    catch error:{badkey, _}:S -> error({undefined_var, ErlangVar, S})
    end.
%% Bindings stack
state_clear_var_stack() ->
    put(?BINDINGS_STACK, []).
state_push_var_stack() ->
    put(?BINDINGS_STACK, [state_get_vars() | get(?BINDINGS_STACK)]).
state_pop_discard_var_stack() ->
    O = hd(get(?BINDINGS_STACK)),
    put(?BINDINGS_STACK, tl(get(?BINDINGS_STACK))),
    O.
state_pop_var_stack() ->
    O = hd(get(?BINDINGS_STACK)),
    put(?BINDINGS_STACK, tl(get(?BINDINGS_STACK))),
    put(?BINDINGS, O),
    O.
-define(IMPORT_REQUESTS, import_requests).
state_clear_import_requests() ->
    put(?IMPORT_REQUESTS, sets:new()).
state_get_import_request() ->
    sets:to_list(get(?IMPORT_REQUESTS)).

