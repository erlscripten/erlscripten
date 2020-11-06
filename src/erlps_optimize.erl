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

-include("erlps_purescript.hrl").

-export([optimize_expr/1]).

-type(peepholable()) :: purs_expr() | purs_guard() | [peepholable()].

optimize_expr(Expr) ->
    peephole(first, Expr).

-spec peephole(first | second, peepholable()) -> purs_expr().
peephole(Phase, L) when is_list(L) ->
    peephole_list(Phase, L, []);

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
%% case X of true -> T; false -> F end --> erlIf X T F
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
              }
             ]
    }) ->
    peephole(first,
             #expr_app{function = #expr_var{name = "erlIf"},
                       args = [Expr, T, F]
                      });

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
