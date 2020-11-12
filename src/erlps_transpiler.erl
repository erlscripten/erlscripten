%%%-------------------------------------------------------------------
%%% @author gorbak25
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
        , filter_module_attributes/1
        , erlang_module_to_purs_file/1 ]).

-include("erlps_purescript.hrl").
-include("erlps_utils.hrl").

-record(env,
        { current_module :: string()
        , records :: map()
        }).

version() -> "v0.0.1".

transpile_erlang_module(Forms) ->
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
        , #import{path = ["Data", "List"], alias = "DL"}
        , #import{path = ["Data", "Maybe"], alias = "DM"}
        , #import{path = ["Data", "Map"], alias = "Map"}
        , #import{path = ["Data", "Tuple"], alias = "Tup"}
        , #import{path = ["Data", "Traversable"], explicit = ["sequence"]}
        , #import{path = ["Erlang", "Builtins"], alias = "BIF"}
        , #import{path = ["Erlang", "Helpers"]}
        , #import{path = ["Erlang", "Type"], explicit = ["ErlangFun", "ErlangTerm(..)"]}
        , #import{path = ["Effect"], explicit = ["Effect"]}
        , #import{path = ["Effect", "Unsafe"], explicit = ["unsafePerformEffect"]}
        , #import{path = ["Effect", "Exception"], explicit = ["throw"]}
        , #import{path = ["Control", "Applicative"]}
        , #import{path = ["Control", "Monad"]}
        ],
    state_clear_import_requests(),
    Env = #env{
             current_module = atom_to_list(ModuleName),
             records = Records
            },
    %% Now do the dirty work - transpile every function OwO
    Decls = [transpile_function(Function, Env) ||
                Function = {function, _, FunName, Arity, _} <- Forms,
                case check_builtin(ModuleName, FunName, Arity) of
                    local -> true;
                    _     -> false
                end
            ],
    Imports = lists:map(fun erlang_module_to_qualified_import/1, state_get_import_request()),

    Exports = case lists:member({compile, export_all}, Attributes) of
                  true -> all;
                  false ->
                      [ transpile_fun_name(Export, Arity)
                       || {export, ExportList} <- Attributes,
                          {Export, Arity} <- ExportList
                      ]
              end,

    %% Dispatchers = [#top_clause{clause = Disp}
    %% || Disp <- make_dispatchers(FunctionForms)],
    #module{
       name = erlang_module_to_purs_module(ModuleName),
       exports = Exports,
       imports = DefaultImports ++ Imports,
       decls = Decls %++ Dispatchers
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

transpile_function({function, _, FunName, Arity, Clauses}, Env) ->
    Type = #type_var{name = "ErlangFun"},
    PSClauses = [transpile_function_clause(Clause, Env) ||
                    Clause <- Clauses],
    CatchClause = #clause{args = [pat_wildcard], value = ?function_clause},
    #valdecl{
       name = transpile_fun_name(FunName, Arity),
       clauses = PSClauses ++ [CatchClause],
       type = Type
      }.

transpile_fun_name(Name, Arity) when is_atom(Name) ->
    transpile_fun_name(atom_to_list(Name), Arity);
transpile_fun_name(Name, Arity) when is_binary(Name) ->
    transpile_fun_name(binary_to_list(Name), Arity);
transpile_fun_name(Name, Arity) ->
    io_lib:format("erlps__~s__~p", [Name, Arity]).

-spec builtins() -> #{{string(), string(), non_neg_integer()} => string()}.
builtins() ->
    Operators = [ {"+",   "op_plus"}
                , {"-",   "op_minus"}
                , {"*",   "op_mult"}
                , {"/",   "op_div"}
                , {"div", "op_div"}
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
                , {"!",   "send"}
                , {"andalso", "op_andalso"}
                , {"orelse",  "op_orelse"}
                ],
    maps:from_list(lists:concat([
        [ {{"erlang", "not", 1}, "erlang__op_not"}
        , {{"erlang", "-", 1}, "erlang__op_neg"}
        ],
        [ {{"erlang", Op, 2}, io_lib:format("erlang__~s", [Fun])}
          || {Op, Fun} <- Operators],
        [ {{Module, Fun, Arity},
           io_lib:format("~s__~s__~p", [Module, Fun, Arity])}
          || {Module, Fun, Arity} <-
                 lists:concat(
                   [ [ {"lists", "keyfind", 3}
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
                   , [ {"erlang", atom_to_list(BIF), Arity}
                       || {BIF, Arity} <- erlang:module_info(exports),
                          proplists:get_value(atom_to_list(BIF), Operators, none) =:= none
                     ]
                   ]
                  )
        ]])).

-spec check_builtin(string(), string(), non_neg_integer()) -> local | {builtin, string()}.
check_builtin(Module, Name, Arity) ->
    Key = {Module, Name, Arity},
    case builtins() of
        #{Key := Builtin} -> {builtin, Builtin};
        _ -> local
    end.


-spec transpile_fun_ref(string() | atom(), non_neg_integer(), #env{}) -> purs_expr().
transpile_fun_ref(Name, Arity, Env = #env{current_module = Module}) ->
    transpile_fun_ref(Module, Name, Arity, Env).
-spec transpile_fun_ref(string() | atom(), string() | atom(), non_neg_integer(), #env{}) -> purs_expr().
transpile_fun_ref(Module, Name, Arity, Env) when is_atom(Name) ->
    transpile_fun_ref(Module, atom_to_list(Name), Arity, Env);
transpile_fun_ref(Module, Name, Arity, Env) when is_atom(Module) ->
    transpile_fun_ref(atom_to_list(Module), Name, Arity, Env);
transpile_fun_ref(Module, Name, Arity, #env{current_module = CurModule}) ->
    case check_builtin(Module, Name, Arity) of
        {builtin, Builtin} ->
            #expr_var{name = "BIF." ++ Builtin};
        local ->
            %% TODO: import resolving
            %% assuming nobody overwrites autoimported stuff
            case check_builtin("erlang", Name, Arity) of
                {builtin, BuiltinAnyway} ->
                    #expr_var{name = "BIF." ++ BuiltinAnyway};
                local -> if CurModule == Module -> #expr_var{name = transpile_fun_name(Name, Arity)};
                        true -> #expr_var{name = erlang_module_to_purs_module(Module) ++ "."
                                          ++ transpile_fun_name(Name, Arity)}
                     end
            end
    end.

transpile_function_clause({clause, _, Args, Guards, Body}, Env) ->
    %% Ok this will be slightly tricky, some patterns commonly used in erlang function clauses
    %% cannot be expressed as matches in Purescipt BUT we may emulate them in guards!
    %% Guards in purescript are really powerful - using patten guards we will emulate what we want
    %% Now we need to determine 2 important things:
    %% 1) What to match in the function head
    %% 2) What to assert in the guard
    %% We also need to emulate erlang's semantics of matching by value
    %% Important:
    %% 1) Standalone variables are the simplest case to consider
    %% 2) Literal atoms/strings/tuples are easy
    %% 3) Binaries are a PITA
    %% Keeping track of variable bindings in a pure functional way will be a PITA
    %% Let's use the process dictionary to emulate a state monad and then gather results at the end
    %% Only after we emitted the guards which will bring the referenced variables in scope
    %% we may add the guards from erlang :)
    %% When matching by value always create a new variable and AFTER all the guards which
    %% will bring the vars in the proper scope we will assert equality
    %% What essentially we want to generate:
    %% fun_name match1 match2 ... matchN | g_match1, g_match2, ..., g_matchN, ERLANG_MATCH_BY_VALUE, user erlang guard expressions
    %% Some guards which we may emit are in the state monad
    %% To deal with it just emit "unsafePerformEffect" and add guards which will ensure that
    %% The effectful computation won't throw an exception
    %% For instance: when dealing with <<X>> first emit a check for the length and
    %% only then unsafely access the specified byte in the binary
    state_clear_vars(),
    state_clear_var_stack(),
    {PsArgs, PsGuards} = transpile_pattern_sequence(Args, Env),
    PSBody = erlps_optimize:optimize_expr(transpile_body(Body, Env)),
    #clause{
       args = [#pat_array{value = PsArgs}],
       guards = erlps_optimize:optimize_expr(PsGuards ++ transpile_boolean_guards(Guards, Env)),
       value = PSBody
    }.


transpile_boolean_guards([], _Env) -> [];
transpile_boolean_guards([SingleConj], Env) ->
    GSeq = [transpile_boolean_guards_singleton(E, Env) || E <- SingleConj],
    %% If all of them compiled to guards then we won :)
    case lists:filter(fun(error) -> true; (_) -> false end, GSeq) of
      [] ->
          lists:flatten(GSeq);
      _ ->
          %% Well - just fallback to the general case :P
          transpile_boolean_guards_fallback([SingleConj], Env)
    end;
transpile_boolean_guards(Guards, Env) ->
    %% Well alternatives are hard and we don't worry about them right now :P
    transpile_boolean_guards_fallback(Guards, Env).

transpile_boolean_guards_singleton({call,_,{atom,_,is_list},[{var,_,Var}]}, _Env) ->
  [#guard_expr{guard = #expr_app{function = ?make_expr_var("isEL"), args = [?make_expr_var(state_get_var(Var))]}}];
transpile_boolean_guards_singleton({call,_,{atom,_,is_tuple},[{var,_,Var}]}, _Env) ->
  Pat = #pat_constr{constr = "ErlangTuple", args = [pat_wildcard]},
  [#guard_assg{lvalue = Pat, rvalue = ?make_expr_var(state_get_var(Var))}];
transpile_boolean_guards_singleton({call,_,{atom,_,is_integer},[{var,_,Var}]}, _Env) ->
  Pat = #pat_constr{constr = "ErlangNum", args = [pat_wildcard]},
  [#guard_assg{lvalue = Pat, rvalue = ?make_expr_var(state_get_var(Var))}];
transpile_boolean_guards_singleton({call,_,{atom,_,is_atom},[{var,_,Var}]}, _Env) ->
  Pat = #pat_constr{constr = "ErlangAtom", args = [pat_wildcard]},
  [#guard_assg{lvalue = Pat, rvalue = ?make_expr_var(state_get_var(Var))}];
transpile_boolean_guards_singleton({call,_,{atom,_,is_function},[{var,_,Var},{integer,_,Arity}]}, _Env) ->
  Pat = #pat_constr{constr = "ErlangFun", args = [#pat_num{value = Arity}, pat_wildcard]},
  [#guard_assg{lvalue = Pat, rvalue = ?make_expr_var(state_get_var(Var))}];
transpile_boolean_guards_singleton({op, _, Op0, Lop, Rop}, Env) ->
    LE = guard_trivial_expr(Lop, Env),
    RE = guard_trivial_expr(Rop, Env),
    Op = case Op0 of _ when is_atom(Op0) -> atom_to_list(Op0); _ -> Op0 end,
    F = fun(N) -> [#guard_expr{guard = #expr_binop{name = N, lop = LE, rop = RE}}] end,
    case {LE, RE} of
        {error, _} -> error;
        {_, error} -> error;
        _ ->
            case Op of
                "==" ->
                  %% TODO: special case for floats ;P
                  F("==");
                "=:=" ->
                  %% TODO: special case for floats ;P
                  F("==");
                "=<" ->
                  F("<=");
                "<" ->
                  F("<");
                ">=" ->
                  F(">=");
                ">" ->
                  F(">");
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
guard_trivial_expr(_Expr, _Env) ->
    error.

transpile_boolean_guards_fallback(Guards, Env) ->
    %% erlps_logger:info("Erlscripten was unable to remove effects in guard sequence - Falling back to inefficient code in\n~p ", [Guards]),
    Alts = [
      lists:foldl(fun(G, AccConjs) -> {op, any, "andalso", AccConjs, G} end,
        hd(Alt), tl(Alt)) || Alt <- Guards ],
    E = lists:foldl(
      fun(Alt, AccAlts) -> {op, any, "orelse", AccAlts, Alt} end,
      hd(Alts), tl(Alts)),
    {TruePat, [], []} = transpile_pattern({atom, any, true}, Env),
    [#guard_assg{
       lvalue = TruePat,
       rvalue = escape_effect_guard(transpile_expr(E, Env))
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
%% transpile_pattern({float, _, Float}, _) ->
%%     error(todo);
%% transpile_pattern({integer, _, Num}, _) when Num =< 9007199254740000, Num >= -9007199254740000 ->
%%     error({todo, too_big_int}); TODO
transpile_pattern({integer, _, Num}, _) ->
    {?make_pat_int(Num), [], []};
transpile_pattern({op, _, "-", {integer, Ann, Num}}, Env) ->
    transpile_pattern({integer, Ann, -Num}, Env);

%% Bitstring pattern
transpile_pattern({bin, _, []}, _) ->
    %% The easy case – <<>>
    %% Empty binary - guard for size eq 0
    Var = state_create_fresh_var("bin_e"),
    {{#pat_constr{constr = "ErlangBinary", args = [#pat_var{name = Var}]}}, [
        #guard_expr{guard =
        #expr_binop{
            name = "==",
            lop = #expr_num{value = 0},
            rop = #expr_app{function = #expr_var{name = "ErlangBinary.unboxed_byte_size"}, args = [#expr_var{name = Var}]}}}
    ], []};
transpile_pattern({bin, _, [{bin_element, _, {string, _, Str}, default, default}]}, _) ->
    %% Binary string literal – <<"erlang">>
    %% Assert buffer length and then compare with str
    Var = state_create_fresh_var("bin_s"),
    {{#pat_constr{constr = "ErlangBinary", args = [#pat_var{name = Var}]}}, [
        #guard_expr{guard = #expr_binop{
            name = "==",
            lop = #expr_num{value = length(Str)},
                               rop = #expr_app{
                                        function = #expr_var{name =  "ErlangBinary.unboxed_byte_size"},
                                        args = [#expr_var{name = Var}]}}},
        #guard_expr{guard = #expr_binop{
            name = "==",
            lop = #expr_num{value = length(Str)},
            rop = #expr_app{
                     function = #expr_var{name = "ErlangBinary.unboxed_strcmp"},
                     args = [#expr_var{name = Var}, #expr_string{value = Str}]}}}
    ], []};
transpile_pattern({bin, _, Segments}, _) ->
    %% Ok the general hard part...
    %% Unfortunately we need to keep track of bindings created in this match
    %% Variables in the size guard can only reference variables from the enclosing scope
    %% present on the variable stack OR variables created during this binding
    %% Fortunately patterns can only be literals or variables
    %% Size specs are guard expressions
    Var = state_create_fresh_var("bin_c"),
    {G, V, _} = transpile_binary_pattern_segments(Var, Segments, #{}),
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
    Var = state_create_fresh_var("match_pat"),
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
                KeyExpr = case transpile_expr(Key, Env) of
                              #expr_app{function = #expr_var{name = "pure"}, args = [Ex]} -> Ex;
                              Effectful -> escape_effect(Effectful)
                          end,
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
transpile_pattern(P, _Env) when element(1, P) =:= op ->
    case compute_constexpr(P) of
        {ok, Res} -> Res;
        error -> error({illegal_operator_pattern, P})
    end;

%% Record index pattern
transpile_pattern({record_index, _, _RecordName, _Field}, _Env) ->
    error(todo);

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
transpile_pattern({var, _, [$_ | _]}, _) ->
    pat_wildcard;

%% Variable pattern
transpile_pattern({var, _, ErlangVar}, _) ->
    Var = string:to_lower(io_lib:format("~s", [ErlangVar])) ++ state_get_fresh_id(),
    case state_is_used(ErlangVar) of
        false ->
            state_put_var(ErlangVar, Var),
            {#pat_var{name = Var}, [], []};
        true ->
            %% Variable was used before so emit an extra guard
            state_put_var(Var, Var),
            {#pat_var{name = Var},
                [],
                [#guard_expr{guard = #expr_binop{
                    name = "==",
                    lop = #expr_var{name = Var},
                    rop = #expr_var{name = state_get_var(ErlangVar)}}}]}
    end;


transpile_pattern(Arg, _Env) ->
    error({unimplemented_pattern, Arg}).


pattern_vars(Pat) ->
    pattern_vars(Pat, []).
pattern_vars([], Acc) ->
    Acc;
pattern_vars([Pat|Rest], Acc) ->
    Acc1 = pattern_vars(Pat, Acc),
    pattern_vars(Rest, Acc1);
pattern_vars(#pat_string{}, Acc) ->
    Acc;
pattern_vars(pat_wildcard, Acc) ->
    Acc;
pattern_vars(#pat_num{}, Acc) ->
    Acc;
pattern_vars(#pat_var{name = Var}, Acc) ->
    [Var|Acc];
pattern_vars(#pat_constr{args = Pats}, Acc) ->
    pattern_vars(Pats, Acc);
pattern_vars(#pat_array{value = Pats}, Acc) ->
    pattern_vars(Pats, Acc);
pattern_vars(#pat_as{name = Name, pattern = Pat}, Acc) ->
    pattern_vars(Pat, [Name|Acc]);
pattern_vars(#pat_record{fields = Fields}, Acc) ->
    pattern_vars([Pat || {_Field, Pat} <- Fields], Acc).


%% When resolving variables in the size spec look to:
%% 1) The outer scope on the stack
%% 2) The newBindings scope
%% 3) If var is present in both scopes then insert a value guard
transpile_binary_pattern_segments(UnboxedVar, [], NewBindings) ->
    ok.

escape_effect(Expr) ->
    #expr_app{
       function = #expr_var{name = "unsafePerformEffect"},
       args = [Expr]
      }.
escape_effect_guard(Expr) ->
    #expr_app{
       function = #expr_var{name = "unsafePerformEffectGuard"},
       args = [Expr]
      }.

pure(Expr) ->
    #expr_app{function = #expr_var{name = "pure"}, args = [Expr]}.

transpile_body(Body, Env) ->
    PSBody = transpile_body(Body, [], Env),
    catch_partial_lets(PSBody).
transpile_body([], _, _) ->
    error(empty_body);
transpile_body([{match, Ann, Pat, Expr}], Acc, Env) ->
    %% We can't just return a bind
    Var = "match_final_", % do NOT register it at this point
    transpile_body([{match, Ann, {var, Ann, Var}, Expr},
                    {match, Ann, Pat, {var, Ann, Var}},
                    {var, Ann, Var}],
                   Acc,
                   Env);
transpile_body([Expr], Acc, Env) ->
    #expr_do{statements = lists:reverse(Acc), return = transpile_expr(Expr, Env)};
transpile_body([Expr|Rest], Acc, Env) ->
    {PSExpr, Acc1} = transpile_expr(Expr, Acc, Env),
    transpile_body(Rest, [#do_expr{expr = PSExpr}|Acc1], Env).

catch_partial_lets(Expr = #expr_binop{lop = L, rop = R}) ->
    Expr#expr_binop{lop = catch_partial_lets(L), rop = catch_partial_lets(R)};
catch_partial_lets(#expr_app{function = F, args = Args}) ->
    #expr_app{function = catch_partial_lets(F), args = lists:map(fun catch_partial_lets/1, Args)};
catch_partial_lets(#expr_array{value = Arr}) ->
    #expr_array{value = lists:map(fun catch_partial_lets/1, Arr)};
catch_partial_lets(#expr_case{expr = Ex, cases = Cases}) ->
    #expr_case{
       expr = catch_partial_lets(Ex),
       cases =
           [ {Pat, Guards, catch_partial_lets(Cont)}
            || {Pat, Guards, Cont} <- Cases
           ]
      };
catch_partial_lets(Expr = #expr_lambda{body = Body}) ->
    Expr#expr_lambda{body = catch_partial_lets(Body)};
catch_partial_lets(#expr_do{statements = Stm, return = Ret}) ->
    catch_partial_lets_in_statements(Stm, catch_partial_lets(Ret));
catch_partial_lets(#expr_record{fields = Fields}) ->
    #expr_record{fields = [{Name, catch_partial_lets(Value)}|| {Name, Value} <- Fields]};
catch_partial_lets(Leaf) ->
    Leaf.

catch_partial_lets_in_statements(Stmts, Ret) ->
    catch_partial_lets_in_statements(Stmts, [], Ret).
catch_partial_lets_in_statements([], Acc, Ret) ->
    #expr_do{statements = lists:reverse(Acc), return = Ret};
catch_partial_lets_in_statements(
  [#do_let{lvalue = LV, rvalue = RV, guards = Guards}|Rest], Acc, Ret)
  when element(1, LV) =/= pat_var andalso LV =/= pat_wildcard ->
    #expr_do{
       statements =
           lists:reverse(Acc),
       return =
           #expr_case{
              expr = RV,
              cases =
                  [ {LV, Guards, catch_partial_lets_in_statements(Rest, Ret)}
                  , {pat_wildcard, [], ?bad_match}
                  ]
             }
      };
catch_partial_lets_in_statements(
  [Stmt = #do_bind{rvalue = RV}|Rest], Acc, Ret) ->
    catch_partial_lets_in_statements(
      Rest, [Stmt#do_bind{rvalue = catch_partial_lets(RV)}|Acc], Ret);
catch_partial_lets_in_statements(
  [Stmt = #do_let{rvalue = RV}|Rest], Acc, Ret) ->
    catch_partial_lets_in_statements(
      Rest, [Stmt#do_let{rvalue = catch_partial_lets(RV)}|Acc], Ret);
catch_partial_lets_in_statements(
  [#do_expr{expr = Expr}|Rest], Acc, Ret) ->
    catch_partial_lets_in_statements(
      Rest, [#do_bind{lvalue = pat_wildcard, rvalue = catch_partial_lets(Expr)}|Acc], Ret).


%% WIP SCOPE LEAKER

%% leak_scope(Expr = #expr_case{cases = Cases}) ->
%%     CasesResults = [ begin
%%                    {ContLeaked, ContVars} = leak_scope(Cont),
%%                    PatVars = pattern_vars(Pat),
%%                    {{Pat, Guards, ContLeaked}, PatVars ++ ContVars}
%%                      end
%%                      || {Pat, Guards, Cont} = Case <- Cases
%%                    ],
%%     Cases1 = [Case || {Case, _} <- CasesResults],
%%     Vars = [Var || {_, Vars} <- CasesResults, Var <- Vars],
%%     {Expr#expr_case{cases = Cases1}, Vars};
%% leak_scope(#expr_do{statements = Stmts, return = Ret}) ->
%%     {Stmts1, Ret1, Vars} = leak_scope_in_statements(Stmts, Ret),
%%     {#expr_do{statements = Stmts1, return = Ret1}, Vars};
%% leak_scope(NotScoped) ->
%%     {NotScoped, []}.

%% leak_scope_in_statements(Stmts, Ret) ->
%%     leak_scope_in_statements(Stmts, Ret, [], []).  %% TODO HERE RETURN THE SCOPE
%% leak_scope_in_statements([], AccStmts, Vars) ->
%%     {AccStmts, Vars};
%% leak_scope_in_statements([Stmt|Rest], AccStmts, Vars) ->
%%     case Stmt of
%%         #do_bind{lvalue = LV, rvalue = RV} ->
%%             {RV1, RVVars} = leak_scope(RV),
%%             LVVars = pattern_vars(LV),
%%             leak_scope_in_statements(
%%               Rest, [#do_bind{lvalue = LV, rvalue = RV1}|AccStmts], LVVars ++ RVVars ++ Vars);
%%         #do_let{lvalue = LV, rvalue = RV} ->
%%             {RV1, RVVars} = leak_scope(RV),
%%             LVVars = pattern_vars(LV),
%%             leak_scope_in_statements(
%%               Rest, [#do_let{lvalue = LV, rvalue = RV1}|AccStmts], LVVars ++ RVVars ++ Vars);
%%         #do_expr{expr = Expr} ->
%%             {Expr1, ExprVars} = leak_scope(Expr),
%%             leak_scope_in_statements(Rest, [#do_expr{expr = Expr1}])


transpile_expr(Expr, Env) ->
    {PSExpr, Stmts} = transpile_expr(Expr, [], Env),
    #expr_do{statements = lists:reverse(Stmts), return = PSExpr}.
transpile_expr({match, _, {var, _, [$_ | _]}, Expr}, Stmts, Env) ->
    %% When matching to a wildcard pattern we may skip the case statement
    transpile_expr(Expr, Stmts, Env);
transpile_expr({match, _, Pat, Val}, Stmts0, Env) ->
    {ValueExpr, Stmts1} = transpile_expr(Val, Stmts0, Env),
    {PSPats, PSGuards} = transpile_pattern_sequence([Pat], Env),
    state_pop_discard_var_stack(), %% This permanently commits the bindings to the local scope
    case {PSPats, PSGuards} of
        {[#pat_var{name = Var}], []} ->
            {pure(#expr_var{name = Var}),
             [#do_bind{lvalue = #pat_var{name = Var}, rvalue = ValueExpr} | Stmts1]};
        {[PSPat], _} ->
            Var = state_create_fresh_var("match_expr"),
            { pure(#expr_var{name = Var}),
              [ #do_let{lvalue = PSPat, rvalue = #expr_var{name = Var}, guards = PSGuards}
              , #do_bind{lvalue = #pat_var{name = Var}, rvalue = ValueExpr}
              | Stmts1]}
    end;
%% TODO Scope leaking!
transpile_expr({'if', _, Clauses}, Stmts, Env) ->
    {#expr_case{
        expr = ?make_expr_atom(true),
        cases = [{pat_wildcard,
                  transpile_boolean_guards(GuardSequence, Env),
                  transpile_body(Body, Env)} ||
                    {clause, _, [], GuardSequence, Body} <- Clauses]
       }, Stmts};
%% TODO Scope leaking!
transpile_expr({'case', _, Expr, Clauses}, Stmts0, Env) ->
    {ExprVar, Stmts1} = bind_expr("case", Expr, Stmts0, Env),
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
                    [{pat_wildcard, [], ?case_clause}];
              _ ->
                case lists:last(UserCases) of
                    {#pat_var{}, [], _} ->
                        UserCases;
                    _ ->
                        UserCases ++ [{pat_wildcard, [], ?case_clause}]
                end
            end,
    Case = #expr_case{
       expr = #expr_var{name = ExprVar},
       cases = Cases
      },
    {Case, Stmts1};

transpile_expr({atom, _, Atom}, Stmts, _Env) ->
    {pure(?make_expr_atom(Atom)), Stmts};
transpile_expr({var, _, Var}, Stmts, _Env) ->
    {pure(#expr_var{name = state_get_var(Var)}), Stmts};

transpile_expr({integer, _, Int}, Stmts, _Env) ->
    {pure(?make_expr_int(Int)), Stmts};
transpile_expr({char, Ann, Int}, Stmts, Env) ->
    transpile_expr({integer, Ann, Int}, Stmts, Env);
transpile_expr({string, _, String}, Stmts, _Env) ->
    {pure(#expr_app{
             function = #expr_var{name = "make_string"},
             args = [#expr_string{value = String}]}),
     Stmts
    };

transpile_expr({op, _, Op, L, R}, Stmts0, Env) ->
    OpFun = transpile_fun_ref("erlang", Op, 2, Env),
    {LVar, Stmts1} = bind_expr("lop", L, Stmts0, Env),
    {RVar, Stmts2} = bind_expr("rop", R, Stmts1, Env),
    {#expr_app{
        function = OpFun,
        args = [#expr_array{
                   value =
                       [#expr_var{name = LVar},
                        #expr_var{name = RVar}
                       ]}]},
     Stmts2
    };
transpile_expr({op, _, Op, Arg}, Stmts0, Env) ->
    OpFun = transpile_fun_ref("erlang", Op, 1, Env),
    {ArgVar, Stmts1} = bind_expr("op_arg", Arg, Stmts0, Env),
    {#expr_app{
        function = OpFun,
        args = [#expr_array{value = [#expr_var{name = ArgVar}]}]},
     Stmts1
    };

transpile_expr({call, _, {atom, _, Fun}, Args}, Stmts0, Env) ->
    {ArgsVars, Stmts1} = bind_exprs("arg", Args, Stmts0, Env),
    PSFun = transpile_fun_ref(Fun, length(Args), Env),
    { #expr_app{
         function = PSFun,
         args = [#expr_array{value = [#expr_var{name = ArgVar} || ArgVar <- ArgsVars]}]}
    , Stmts1
    };
transpile_expr({call, _, {remote, _, {atom, _, Module}, {atom, _, Fun}}, Args},
               Stmts0, Env) ->
    state_add_import_request(Module, Env),
    {ArgsVars, Stmts1} = bind_exprs("arg", Args, Stmts0, Env),
    PSFun = transpile_fun_ref(Module, Fun, length(Args), Env),
    { #expr_app{
         function = PSFun,
         args = [#expr_array{value = [#expr_var{name = ArgVar} || ArgVar <- ArgsVars]}]}
    , Stmts1
    };
transpile_expr({call, _, Fun, Args}, Stmts0, Env) ->
    {FunVar, Stmts1} = bind_expr("fun", Fun, Stmts0, Env),
    {ArgsVars, Stmts2} = bind_exprs("arg", Args, Stmts1, Env),
    { #expr_app{
         function = #expr_var{name = "applyTerm"},
         args = [#expr_var{name = FunVar},
                 #expr_array{value = [#expr_var{name = ArgVar} || ArgVar <- ArgsVars]}]}
    , Stmts2
    };

transpile_expr({nil, _}, Stmts, _) ->
    {pure(?make_expr_empty_list), Stmts};
transpile_expr({cons, _, H, T}, Stmts0, Env) ->
    {VarH, Stmts1} = bind_expr("tail", H, Stmts0, Env),
    {VarT, Stmts2} = bind_expr("head", T, Stmts1, Env),
    { pure(?make_expr_cons(#expr_var{name = VarH}, #expr_var{name = VarT}))
    , Stmts2
    };

transpile_expr({'fun', _, {function, Fun, Arity}}, Stmts, Env) when is_atom(Fun) ->
    {pure(#expr_app{function = #expr_var{name = "ErlangFun"},
                   args = [#expr_num{value = Arity},
                           transpile_fun_ref(Fun, Arity, Env)]}),
     Stmts
    };
transpile_expr({'fun', _, {function, {atom, _, Module}, {atom, _, Fun}, {integer, _, Arity}}},
               Stmts, Env) when is_atom(Fun) ->
    {pure(#expr_app{function = #expr_var{name = "ErlangFun"},
                    args = [#expr_num{value = Arity},
                            transpile_fun_ref(Module, Fun, Arity, Env)]}),
     Stmts
    };
transpile_expr({'fun', _, {clauses, Clauses = [{clause, _, SomeArgs, _, _}|_]}}, Stmts, Env) ->
    Arity = length(SomeArgs),
    ArgVars = [state_create_fresh_var("funarg") || _ <- SomeArgs],
    Case =
        #expr_case{
           expr = #expr_array{value = [#expr_var{name = ArgVar}|| ArgVar <- ArgVars]},
           cases =
               [ begin
                     {PSArgs, PSGuards} = transpile_pattern_sequence(Args, Env),
                     R = { #pat_array{value = PSArgs}
                         , PSGuards ++ transpile_boolean_guards(Guards, Env)
                         , transpile_body(Cont, Env)},
                     state_pop_var_stack(),
                     R
                 end
                || {clause, _, Args, Guards, Cont} <- Clauses
               ]
          },
    Lambda =
        #expr_app{
           function = #expr_var{name = "ErlangFun"},
           args =
               [ #expr_num{value = Arity}
               , #expr_lambda{
                    args = [#pat_array{
                              value = [#pat_var{name = ArgVar} || ArgVar <- ArgVars]
                             }],
                   body = Case}
               ]},
    {pure(Lambda),
     Stmts
    };
transpile_expr({'named_fun', _, Name, Clauses = [{clause, _, SomeArgs, _, _}|_]},
               Stmts0, Env) ->
    FunVar = state_create_fresh_var(string:to_lower(io_lib:format("~s", [Name]))),
    state_put_var(Name, FunVar),
    Arity = length(SomeArgs),
    ArgVars = [state_create_fresh_var("funarg") || _ <- SomeArgs],
    Case =
        #expr_case{
           expr = #expr_array{value = [#expr_var{name = ArgVar}|| ArgVar <- ArgVars]},
           cases =
               [ begin
                     {PSArgs, PSGuards} = transpile_pattern_sequence(Args, Env),
                     R = { #pat_array{value = PSArgs}
                         , PSGuards ++ transpile_boolean_guards(Guards, Env)
                         , transpile_body(Cont, Env)},
                     state_pop_var_stack(),
                     R
                 end
                || {clause, _, Args, Guards, Cont} <- Clauses
               ]
          },
    Lambda =
        #expr_app{
           function = #expr_var{name = "ErlangFun"},
           args =
               [ #expr_num{value = Arity}
               , #expr_lambda{
                    args = [#pat_array{
                              value = [#pat_var{name = ArgVar} || ArgVar <- ArgVars]
                             }],
                   body = Case}
               ]},
    {pure(#expr_var{name = FunVar}),
     [ #do_let{lvalue = #pat_var{name = FunVar}, rvalue = Lambda}
     | Stmts0
     ]
    };

transpile_expr({tuple, _, Exprs}, Stmts0, Env) ->
    {ExprsVars, Stmts1} = bind_exprs("tup", Exprs, Stmts0, Env),
    {pure(#expr_app{
        function = #expr_var{name = "ErlangTuple"},
        args = [#expr_array{value = [#expr_var{name = ExprVar} || ExprVar <- ExprsVars]}]})
    , Stmts1
    };

transpile_expr({record, Ann, RecordName, RecordFields}, Stmts, Env) ->
    %% Convert this to a tuple
    Values = [record_fields(X) || X <- RecordFields],
    Fields = [{atom, Ann, RecordName}] ++
        [proplists:get_value(FieldName, Values, Default) ||
            {FieldName, Default} <- maps:get(RecordName, Env#env.records)],
    transpile_expr({tuple, Ann, Fields}, Stmts, Env);

transpile_expr({lc, _, Ret, []}, Stmts0, Env) ->
    {RetVar, Stmts1} = bind_expr("lc_ret", Ret, Stmts0, Env),
    {pure(make_expr_list([#expr_var{name = RetVar}])),
     Stmts1
    };
transpile_expr({lc, _, Ret, [{generate, Ann, Pat, Source}|Rest]}, Stmts0, Env) ->
    {SourceVar, Stmts1} = bind_expr("lc_src", Source, Stmts0, Env),
    {[PSPat], Guards} = transpile_pattern_sequence([Pat], Env),
    Var = state_create_fresh_var("lc"),
    Gen =
        #expr_app{
           function = transpile_fun_ref(lists, "flatmap", 2, Env),
           args =
               [#expr_array{
                   value =
                       [ #expr_var{name = SourceVar},
                         ?make_expr_lambda(
                            [#pat_var{name = Var}],
                            #expr_case{
                               expr = #expr_var{name = Var},
                               cases =
                                   [ {PSPat, Guards, transpile_expr({lc, Ann, Ret, Rest}, Env)}
                                   , {pat_wildcard, [], pure(?make_expr_empty_list)}
                                   ]
                              }
                           )
                       ]
                  }]
          },
    state_pop_var_stack(),
    {Gen, Stmts1};
transpile_expr({lc, Ann, Ret, [Expr|Rest]}, Stmts0, Env) ->
    {Var, Stmts1} = bind_expr("cond", Expr, Stmts0, Env),
    {#expr_case{
        expr = #expr_var{name = Var},
        cases = [ {?make_pat_atom(true), [], transpile_expr({lc, Ann, Ret, Rest}, Env)}
                , {pat_wildcard, [], pure(?make_expr_empty_list)}
                ]
       },
     Stmts1
    };

transpile_expr({map, _, Associations}, Stmts0, Env) ->
    {Keys, Vals} = lists:unzip([{Key, Val} || {map_field_assoc, _, Key, Val} <- Associations]),
    {KeysVars, Stmts1} = bind_exprs("key", Keys, Stmts0, Env),
    {ValsVars, Stmts2} = bind_exprs("val", Vals, Stmts1, Env),
    Map = #expr_app{
             function = #expr_var{name = "Map.fromFoldable"},
             args =
                 [#expr_array{
                     value =
                         [ #expr_app{function = #expr_var{name = "Tup.Tuple"},
                                     args = [#expr_var{name = KeyVar},
                                             #expr_var{name = ValVar}]
                                    }
                           || {KeyVar, ValVar} <- lists:zip(KeysVars, ValsVars)
                         ]
                    }
                 ]},
    {pure(?make_expr_map(Map)),
     Stmts2
    };
transpile_expr({map, Ann, Map, Associations}, Stmts0, Env) ->
    {MapVar, Stmts1} = bind_expr("map", Map, Stmts0, Env),
    {Ext, Stmts2} = transpile_expr({map, Ann, Associations}, Stmts1, Env),
    ExtVar = state_create_fresh_var("map_ext"),
    {#expr_app{
        function = transpile_fun_ref(maps, merge, 2, Env),
        args = [#expr_array{value = [#expr_var{name = MapVar}, #expr_var{name = ExtVar}]}]},
     [ #do_bind{lvalue = #pat_var{name = ExtVar}, rvalue = Ext}
     | Stmts2]
    };

transpile_expr(X, _Stmts, _Env) ->
    error({unimplemented_expr, X}).


bind_expr(Name, Expr, Stmts0, Env) ->
    Var = state_create_fresh_var(Name),
    {PSExpr, Stmts1} = transpile_expr(Expr, Stmts0, Env),
    {Var,
     [ #do_bind{lvalue = #pat_var{name = Var}, rvalue = PSExpr}
     | Stmts1
     ]}.
bind_exprs(Name, Exprs, Stmts, Env) ->
    bind_exprs(Name, Exprs, [], Stmts, Env).
bind_exprs(_, [], Acc, Stmts, _) ->
    {lists:reverse(Acc), Stmts};
bind_exprs(Name, [Expr|Rest], Acc, Stmts0, Env) ->
    {Var, Stmts1} = bind_expr(Name, Expr, Stmts0, Env),
    bind_exprs(Name, Rest, [Var|Acc], Stmts1, Env).



compute_constexpr({op, _, Op, L, R}) -> %% FIXME: float handling needs to be fixed
    case {compute_constexpr(L), compute_constexpr(R)} of
        {{ok, LV}, {ok, RV}}
            when is_number(LV) andalso is_number(RV) andalso
            (Op =:= '+' orelse Op =:= '-' orelse Op =:= '*' orelse Op =:= '/')
            -> {ok, (fun erlang:Op/2)(LV, RV)};
        _ -> error
    end;
compute_constexpr({integer, _, Num}) ->
    {ok, Num};
compute_constexpr({float, _, Num}) ->
    {ok, Num}.

%% Hacky emulation of a state monad using the process dictionary :P
-define(BINDINGS, var_bindings).
-define(BINDINGS_STACK, var_bindings_stack).
%% Variable bindings
state_clear_vars() ->
    put(?BINDINGS, #{}).
state_get_vars() ->
    get(?BINDINGS).
state_get_fresh_id() ->
    integer_to_list(map_size(state_get_vars())).
state_put_var(ErlangVar, PsVar) ->
    put(?BINDINGS, maps:put(ErlangVar, PsVar, state_get_vars())).
state_create_fresh_var() ->
    state_create_fresh_var("v").
state_create_fresh_var(Name) ->
    Var = Name ++ "_" ++ state_get_fresh_id(),
    state_put_var(Var, Var),
    Var.
state_is_used(ErlangVar) ->
    maps:is_key(ErlangVar, state_get_vars()).
state_get_var(ErlangVar) ->
    maps:get(ErlangVar, state_get_vars()).
%% Bindings stack
state_clear_var_stack() ->
    put(?BINDINGS_STACK, []).
state_push_var_stack() ->
    put(?BINDINGS_STACK, [state_get_vars() | get(?BINDINGS_STACK)]).
state_pop_discard_var_stack() ->
    put(?BINDINGS_STACK, tl(get(?BINDINGS_STACK))).
state_pop_var_stack() ->
    O = hd(get(?BINDINGS_STACK)),
    put(?BINDINGS_STACK, tl(get(?BINDINGS_STACK))),
    put(?BINDINGS, O).

-define(IMPORT_REQUESTS, import_requests).
state_clear_import_requests() ->
    put(?IMPORT_REQUESTS, sets:new()).
state_add_import_request(Module, Env) when is_atom(Module) ->
    state_add_import_request(atom_to_list(Module), Env);
state_add_import_request("erlang", _) -> ok;
state_add_import_request(Module, #env{current_module = Module}) -> ok;
state_add_import_request(Module, _Env) ->
    put(?IMPORT_REQUESTS, sets:add_element(Module, get(?IMPORT_REQUESTS))).
state_get_import_request() ->
    sets:to_list(get(?IMPORT_REQUESTS)).

