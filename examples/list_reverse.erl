-module(list_reverse).

-compile(export_all).

list_reverse(L) ->
  (lists:foldr(
   fun (E, Cont) ->
       fun(X) -> Cont([E|X])
       end
   end,
   fun(X) -> X end,
   L))
  ([]).
