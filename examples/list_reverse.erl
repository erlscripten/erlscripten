-module(list_reverse).

-compile(export_all).

%% Continuation passing based list reversal.
%% It is easier to understand when you realize that
%% foldr is just a catamorphism in the F-algebra built
%% on a `list` endofunctor in the category of Erlang
%% types with arity-1 functions as morphisms.
list_reverse(L) ->
  (lists:foldr(
   fun (E, Cont) ->
       fun(X) -> Cont([E|X])
       end
   end,
   fun(X) -> X end,
   L))
  ([]).
