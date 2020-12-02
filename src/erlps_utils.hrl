
-define(make_pat_var(Var),
    #pat_var{name = Var}).
-define(make_pat_float(Float),
    #pat_constr{constr = "ErlangFloat", args = [#pat_float{value = Float}]}).
-define(make_pat_atom(Atom),
    #pat_constr{constr = "ErlangAtom", args = [#pat_string{value = atom_to_list(Atom)}]}).
-define(make_pat_tuple(Stuff),
    #pat_constr{constr = "ErlangTuple", args = [#pat_array{value = Stuff}]}).
-define(make_pat_cons(H, T),
    #pat_constr{constr = "ErlangCons", args = [H, T]}).
-define(make_pat_empty_list,
    ?make_pat_var("ErlangEmptyList")).
-define(make_pat_map(Map),
        #pat_constr{constr = "ErlangMap", args = [Map]}).
make_pat_list([]) ->
    ?make_pat_empty_list;
make_pat_list([H|T]) ->
    ?make_pat_cons(H, make_pat_list(T)).


-define(make_expr_var(Var),
    #expr_var{name = Var}).
-define(make_expr_int(Int),
    #expr_app{
       function = ?make_expr_var("ErlangInt"),
       args = [ case Int < -2147483648 orelse Int > 2147483647 of
                    true ->
                        #expr_app{
                           function = ?make_expr_var("unsafePartial"),
                           args = [#expr_app{
                                      function = ?make_expr_var("DM.fromJust"),
                                      args = [#expr_app{
                                                 function = ?make_expr_var("DBI.fromString"),
                                                 args = [#expr_string{value = integer_to_list(Int)}]
                                                }]
                                     }]
                          };
                    false ->
                        #expr_app{function = ?make_expr_var("DBI.fromInt"),
                                  args = [#expr_num{value = Int}]
                                 }
                end ]}).
-define(make_expr_float(Float),
    #expr_app{function = ?make_expr_var("ErlangFloat"), args = [#expr_float{value = Float}]}).
-define(make_expr_atom(Atom),
    #expr_app{function = ?make_expr_var("ErlangAtom"), args = [#expr_string{value = atom_to_list(Atom)}]}).
-define(make_expr_tuple(Stuff),
    #expr_app{function = ?make_expr_var("ErlangTuple"), args = [#expr_array{value = Stuff}]}).
-define(make_expr_cons(H, T),
    #expr_app{function = ?make_expr_var("ErlangCons"), args = [H, T]}).
-define(make_expr_empty_list,
    ?make_expr_var("ErlangEmptyList")).
-define(make_expr_map(Map),
    #expr_app{function = ?make_expr_var("ErlangMap"), args = [Map]}).
-define(make_expr_fun(Arity, Fun),
    #expr_app{function = ?make_expr_var("ErlangFun"),
              args = [#expr_num{value = Arity}, Fun]}).
-define(make_expr_bin(Ar),
    #expr_app{function = ?make_expr_var("ErlangBinary"),
              args = [Ar]
             }).
-define(make_expr_lambda(Args, Body),
    #expr_app{function = ?make_expr_var("ErlangFun"),
              args = [#expr_num{value = length(Args)},
                      #expr_lambda{
                         args = [#pat_array{value = Args}],
                         body = Body
                        }
                     ]}).
make_expr_list([]) ->
    ?make_expr_empty_list;
make_expr_list([H|T]) ->
    ?make_expr_cons(H, make_expr_list(T)).
-define(make_expr_unit, #expr_var{name = "unit"}).

-define(common_error(Name, Args),
        #expr_app{function = #expr_var{name = "EXC." ++ atom_to_list(Name)},
                  args = Args}).


-define(function_clause, ?common_error(function_clause, [?make_expr_unit])).
-define(case_clause(T), ?common_error(case_clause, [T])).
-define(if_clause, ?common_error(if_clause, [?make_expr_unit])).
-define(try_clause(T), ?common_error(try_clause, [T])).
-define(badmatch(T), ?common_error(badmatch, [T])).
-define(badrecord(T), ?common_error(badrecord, [T])).
-define(badarity(F, Args), ?common_error(badarity, [F, Args])).
-define(badarg, ?common_error(badarg, [?make_expr_unit])).
-define(badarg(T), ?common_error(badarg1, [T])).
-define(badkey(T), ?common_error(badkey, [T])).
-define(bad_generator(T), ?common_error(bad_generator, [T])).
