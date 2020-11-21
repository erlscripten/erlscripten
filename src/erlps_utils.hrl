
-define(make_pat_var(Var),
    #pat_var{name = Var}).
-define(make_pat_int(Int),
    #pat_constr{constr = "ErlangNum", args = [#pat_num{value = Int}]}).
-define(make_pat_float(Float),
    #pat_constr{constr = "ErlangFloat", args = [#expr_float{value = Float}]}).
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
    #expr_app{function = ?make_expr_var("ErlangNum"), args = [#expr_num{value = Int}]}).
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

-define(common_error(Name, T),
        #expr_app{function = #expr_var{name = "EXC.error"},
                  args = [?make_expr_tuple([?make_expr_atom(Name), T])]}).


-define(function_clause(T), ?common_error(function_clause, T)).
-define(case_clause(T), ?common_error(case_clause, T)).
-define(if_clause(T), ?common_error(if_clause, T)).
-define(try_clause(T), ?common_error(try_clause, T)).
-define(badmatch(T), ?common_error(badmatch, T)).
-define(badrecord(T), ?common_error(badrecord, T)).
