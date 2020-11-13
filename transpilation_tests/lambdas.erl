
-module(lambdas).
-erlscripten_output(autogenerated_transpilation_tests).
-compile({parse_transform, erlps_parse_transform}).
-compile(nowarn).
-compile(export_all).

%% API
-export([]).

test_can_be_called() ->
    A = fun(Q) -> Q end,
    A(ok).

id(X) -> X.

test_can_pass_fun() ->
    A = fun lambdas:id/1,
    A(ok).

test_match_semantics_1() ->
    X = 2,
    A = fun (X) -> ok end,
    A(3).

test_match_semantics_2() ->
    A = 2,
    B = fun (W) -> A = W end,
    B(1). %% should throw exception

test_match_semantics_3() ->
    A = 2,
    F = fun (X) when X==A -> ok; (_) -> match_failed end,
    ok = F(2),
    match_failed = F(3),
    ok.

test_scope_does_not_leak_1() ->
    (fun() -> X=2 end)(),
    X = ok.

test_scope_does_not_leak_2() ->
    (fun X() -> ok end)(),
    X = ok.

test_can_use_stdlib() ->
    R = lists:map(fun(X) -> X*2 end, lists:seq(1,10)),
    [2,4,6,8,10,12,14,16,18,20] = R,
    ok.

test_factorial_abuse_1() ->
    %% http://www.willamette.edu/~fruehr/haskell/evolution.html OwO
    A = fun FacCps(K, 0) -> K(1);
            FacCps(K, N) -> FacCps(fun(R) -> K(R*N) end, N-1) end,
    720 = A(fun(X) -> X end, 6),
    ok.

y(X) ->
   F = fun (P) -> X(fun (Arg) -> (P(P))(Arg) end) end,
   F(F).

mk_fact() ->
   F =
     fun (FA) ->
          fun (N) ->
              if (N == 0) -> 1;
                 true -> N * FA(N-1)
              end
          end
     end,
   y(F).

test_factorial_abuse_2() ->
    F = mk_fact(),
    720 = F(6),
    ok.

test_factorial_abuse_3() ->
    %% Abuse combinators xD
    %% If this monstrosity properly evaluates then lambdas are handled properly
    Y = fun(X) -> F = fun (P) -> X(fun (Arg) -> (P(P))(Arg) end) end, F(F) end,
    S = fun(F, G, X) -> F(X, G(X)) end,
    K = fun(X, Y) -> X end,
    B = fun(F, G, X) -> F(G(X)) end,
    Cond = fun(P, F, G, X) -> case P(X) of true -> F(X); false -> G(X) end end,
    Fac = Y(fun(FA) -> fun(N) -> Cond(fun(X) -> X==0 end, fun(X) -> K(1, X) end, fun(X) -> S(fun(X,Y) -> X*Y end, fun(X) -> B(FA, fun(Y) -> Y-1 end, X) end, X) end, N) end end),
    1 = Fac(1),
    2 = Fac(2),
    720 = Fac(6),
    5040 = Fac(7),
    40320 = Fac(8),
    ok.
