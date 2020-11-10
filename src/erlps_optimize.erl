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
    %% Expr.
    peephole(first, Expr).

-spec peephole(first | second, peepholable()) -> peepholable().
peephole(Phase, L) when is_list(L) ->
    peephole_list(Phase, L, []);

%% do X --> X
peephole(_, #expr_do{statements = [], return = Expr}) ->
    peephole(first, Expr);
%% %% _ <- S --> S
%% peephole(_, #do_bind{lvalue = pat_wildcard, rvalue = Expr}) ->
%%     peephole(first, #do_expr{expr = Expr});
peephole(_, #do_expr{expr = Expr}) ->
    peephole(first, #do_bind{lvalue = pat_wildcard, rvalue = Expr});
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
    peephole(second,
            #expr_do{statements = peephole_stmts(peephole(first, Stmts))
            , return = peephole(first, Ret0)});

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

peephole_stmts(Do) ->
    peephole_stmts(Do, []).
peephole_stmts([], Acc) ->
    lists:reverse(Acc);
peephole_stmts([#do_expr{expr = #expr_do{statements = Stmts, return = Ret}}|Rest], Acc) ->
    peephole_stmts(Stmts ++ [#do_expr{expr = Ret}] ++ Rest, Acc);
peephole_stmts([#do_bind{lvalue = LV,
                     rvalue = #expr_do{statements = Stmts, return = Ret}}|Rest],
           Acc) ->
    peephole_stmts(Stmts ++ [#do_bind{lvalue = LV, rvalue = Ret}] ++ Rest, Acc);
peephole_stmts([#do_expr{expr = #expr_app{function = #expr_var{name = "pure"}}}|Rest], Acc) ->
    peephole_stmts(Rest, Acc);
peephole_stmts([Stmt|Rest], Acc) ->
    peephole_stmts(Rest, [Stmt|Acc]).

