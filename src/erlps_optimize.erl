%%%-------------------------------------------------------------------
%%% @author radrow
%%% @copyright (C) 2020, aeternity
%%% @doc
%%%
%%% @end
%%% Created : 06. Nov 2020 16:31
%%%-------------------------------------------------------------------
-module(erlps_optimize).
-author("radrow").


-export([optimize_expr/1]).

-include("erlps_purescript.hrl").
-include("erlps_utils.hrl").

-type(peepholable()) :: purs_expr() | purs_guard() | purs_do_statement() | [peepholable()].

optimize_expr(Expr) ->
    peephole(first, Expr).

-spec peephole(first | second, peepholable()) -> peepholable().
peephole(Phase, L) when is_list(L) ->
    peephole_list(Phase, L, []);

%% do X --> X
peephole(_, #expr_do{statements = [], return = Expr}) ->
    peephole(first, Expr);
%% _ <- S --> S
peephole(_, #do_bind{lvalue = pat_wildcard, rvalue = Expr}) ->
    peephole(first, #do_expr{expr = Expr});
%% V <- pure X --> let V = X
peephole(_, #do_bind{lvalue = LV,
                     rvalue = #expr_app{function = #expr_var{name = "pure"},
                                        args = [Arg]
                                       }
                    }) ->
    peephole(first, #do_let{lvalue = LV, rvalue = Arg});
%% do finishing with do
peephole(_, #expr_do{statements = Stmts0, return = #expr_do{statements = Stmts1, return = Ret}}
        ) ->
    peephole(first, #expr_do{statements = Stmts0 ++ Stmts1, return = Ret});

%% sequence [pure X, pure Y, pure Z, ...] --> pure [X, Y, Z, ...]
peephole(Phase,
         #expr_app{
            function = #expr_var{name = "sequence"},
            args = [#expr_array{value = Args}]
           }) ->
    PeepholedArgs = peephole(Phase, Args),
    GetPures =
        fun Rec([], Acc) -> lists:reverse(Acc);
            Rec([#expr_app{function = #expr_var{name = "pure"},
                           args = [X]
                          } | Rest], Acc) ->
                Rec(Rest, [X | Acc]);
            Rec([#expr_app{function = #expr_var{name = "pure"},
                           args = X
                          } | Rest], Acc) when not is_list(X) ->
                Rec(Rest, [X | Acc]);
            Rec(_, _) ->
                nope
        end,
    case GetPures(PeepholedArgs, []) of
        nope -> #expr_app{
                   function = #expr_var{name = "sequence"},
                   args = [#expr_array{value = PeepholedArgs}]
                  };
        Pures -> #expr_app{function = #expr_var{name = "pure"},
                           args = [#expr_array{value = Pures}]}
    end;
%% unsafePerformEffect (pure X) --> X
peephole(_,
  #expr_app{
     function = #expr_var{name = "unsafePerformEffect"},
     args =
         [#expr_app{function = #expr_var{name = "pure"},
                    args = [Ex]
                   }
         ]}) ->
    peephole(first, Ex);
%% rbind F (pure X) --> F X
peephole(_,
  #expr_app{
     function = #expr_var{name = "rbind"},
     args =
         [ Fun
         , #expr_app{function = #expr_var{name = "pure"},
                    args = [Ex]
                   }
         ]}) ->
    peephole(first,
             #expr_app{function = Fun, args = [Ex]});
%% apply (pure F) X --> map F X
peephole(_,
  #expr_app{
     function = #expr_var{name = "apply"},
     args =
         [ #expr_app{function = #expr_var{name = "pure"},
                     args = [Fun]
                    }
         , Arg
         ]}) ->
    peephole(first,
             #expr_app{function = #expr_var{name = "map"},
                       args = [Fun, Arg]
                      });
%% map F (pure X) --> pure (F X)
peephole(_,
  #expr_app{
     function = #expr_var{name = "map"},
     args =
         [ Fun
         , #expr_app{function = #expr_var{name = "pure"},
                     args = [Ex]
                    }
         ]}) ->
    peephole(first,
             #expr_app{function = #expr_var{name = "pure"},
                       args = [#expr_app{function = Fun, args = [Ex]}]
                      });
%% case X of true -> T; false -> F end --> erlCaseIf X T F
peephole(_,
  #expr_case{
     expr = Expr,
     cases = [{#pat_constr{constr = "ErlangAtom", args = [#pat_string{value = "true"}]},
               [],
               T
              },
              {#pat_constr{constr = "ErlangAtom", args = [#pat_string{value = "false"}]},
               [],
               F
              },
              {pat_wildcard, [], ?case_clause}
             ]
    }) ->
    peephole(first,
             #expr_app{function = #expr_var{name = "erlCaseIf"},
                       args = [Expr, T, F]
                      });
%% case X of V -> V --> X
peephole(_,
  #expr_case{
     expr = Expr,
     cases = [{#pat_var{name = Var}, [], #expr_var{name = Var}}|_]
    }) ->
    peephole(first, Expr);

peephole(first, #expr_binop{name = Op, lop = Lop, rop = Rop}) ->
    peephole(second,
             #expr_binop{name = Op, lop = peephole(first, Lop), rop = peephole(first, Rop)});
peephole(first, #expr_app{function = Fun, args = Args}) ->
    peephole(second,
             #expr_app{function = peephole(first, Fun), args = peephole(first, Args)});
peephole(first, #expr_array{value = Arr}) ->
    peephole(second,
             #expr_array{value = peephole(first, Arr)});
peephole(first, #expr_case{expr = Expr, cases = Cases}) ->
    peephole(second,
             #expr_case{expr = peephole(first, Expr),
               cases = [ {Pat, peephole(first, Guards), peephole(first, Cont)}
                        || {Pat, Guards, Cont} <- Cases
                       ]
              });
peephole(first, #expr_lambda{args = Args, body = Body}) ->
    peephole(second,
             #expr_lambda{args = Args, body = peephole(first, Body)});
peephole(first, #expr_do{statements = Stmts, return = Ret0}) ->
    E = #expr_do{statements = flatten_do(peephole(first, Stmts)),
                              return = peephole(first, Ret0)},
    #expr_do{statements = Statements, return = Ret} = E,
    %% Fixup scope - WARNING DIRTY HACK OwO
    %% Find #expr_case{
    %%                expr = #expr_var{name = Var},
    %%                cases =
    %%                    [ {PSPat, PSGuards, #expr_var{name = "<<NEEDLE>>"}}
    %%                    , {pat_wildcard, [], ?bad_match}
    %%                    ]
    %%               }
  {OldScope, T} = lists:splitwith(fun(#do_expr{expr = #expr_case{ expr = #expr_var{}, cases = [{_, _, #expr_var{name = "<<SCOPE_FIXUP>>"}}, _] }}) -> false; (_) -> true end, Statements),
  E1 = case T of
      [] ->
          E;
      [#do_expr{expr = #expr_case{cases = [{Pat, Guard, _}, Case2] } = C} | NewScope] ->
        Body = #expr_do{statements = NewScope, return = Ret},
        io:format(user, "AAAA ~p\n", [T]),
        #expr_do{
          statements = OldScope,
          return = C#expr_case{cases = [{Pat, Guard, Body}, Case2]}
        }
    end,
  peephole(second, E1);

peephole(first, #do_bind{lvalue = Pat, rvalue = Expr}) ->
    peephole(second,
             #do_bind{lvalue = Pat, rvalue = peephole(first, Expr)});
peephole(first, #do_let{lvalue = Pat, rvalue = Expr}) ->
    peephole(second,
             #do_let{lvalue = Pat, rvalue = peephole(first, Expr)});
peephole(first, #do_expr{expr = Expr}) ->
    peephole(second,
             #do_expr{expr = peephole(first, Expr)});
peephole(first, #guard_expr{guard = Expr}) ->
    peephole(second,
             #guard_expr{guard = peephole(first, Expr)});
peephole(first, #guard_assg{lvalue = Pat, rvalue = Expr}) ->
    peephole(second,
             #guard_assg{lvalue = Pat, rvalue = peephole(first, Expr)});
peephole(_, NothingToDo) ->
    NothingToDo.


peephole_list(_, [], Acc) ->
    lists:reverse(Acc);
peephole_list(Phase, [H|T], Acc) ->
    peephole_list(Phase, T, [peephole(Phase, H)|Acc]).

flatten_do(Do) ->
    flatten_do(Do, []).
flatten_do([], Acc) ->
    lists:reverse(Acc);
flatten_do([#do_expr{expr = #expr_do{statements = Stmts, return = Ret}}|Rest], Acc) ->
    flatten_do(Stmts ++ [#do_expr{expr = Ret}] ++ Rest, Acc);
flatten_do([#do_bind{lvalue = LV,
                     rvalue = #expr_do{statements = Stmts, return = Ret}}|Rest],
           Acc) ->
    flatten_do(Stmts ++ [#do_bind{lvalue = LV, rvalue = Ret}] ++ Rest, Acc);
flatten_do([#do_expr{expr = #expr_app{function = #expr_var{name = "pure"}}}|Rest], Acc) ->
    flatten_do(Rest, Acc);
flatten_do([Stmt|Rest], Acc) ->
    flatten_do(Rest, [Stmt|Acc]).

