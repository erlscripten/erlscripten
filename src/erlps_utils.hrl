
-define(make_pat_var(Var),
    #pat_var{name = Var}).
-define(make_pat_int(Int),
    #pat_constr{constr = "ErlangNum", args = [#pat_num{value = Int}]}).
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

-define(ps_error(Msg),
        #expr_app{function = #expr_var{name = "error"},
                  args = [#expr_string{value = Msg}]}).


-define(function_clause, ?ps_error("function_clause")).
-define(case_clause, ?ps_error("case_clause")).
-define(if_clause, ?ps_error("if_clause")).
-define(try_clause, ?ps_error("try_clause")).
-define(bad_match, ?ps_error("bad_match")).
