%%%-------------------------------------------------------------------
%%% @author radrow
%%% @copyright (C) 2020, aeternity
%%% @doc
%%%
%%% @end
%%% Created : 26. Okt 2020 16:31
%%%-------------------------------------------------------------------
-module(erlps_purescript_pretty).
-author("radrow").

-include("erlps_purescript.hrl").

-import(prettypr, [text/1, sep/1, above/2, beside/2, nest/2, empty/0, follow/2]).

%% API
-export([
    format_module/1,
    with_options/2
]).

-export([
    pp_expr/1,
    pp_pat/1,
    pp_guard/1,
    pp_clause/2,
    pp_type/1,
    pp_import/1,
    pp_valdecl/1,
    pp_module/1
]).


-type doc() :: prettypr:document().
-type options() :: [{indent, non_neg_integer()} | show_generated].


format_module(Module) ->
    Retarded = prettypr:format(pp_module(Module)),
    NotRetarded = string:replace(Retarded, "\t", "        ", all),
    NotRetarded.

%% OPTIONS

-define(erlps_pretty_opts, erlps_pretty_opts).

-spec options() -> options().
options() ->
    case get(?erlps_pretty_opts) of
        undefined -> [];
        Opts      -> Opts
    end.

-spec option(atom(), any()) -> any().
option(Key, Default) ->
    proplists:get_value(Key, options(), Default).

-spec with_options(options(), fun(() -> A)) -> A.
with_options(Options, Fun) ->
    put(?erlps_pretty_opts, Options),
    Res = Fun(),
    erase(?erlps_pretty_opts),
    Res.

-spec indent() -> non_neg_integer().
indent() -> option(indent, 2).

%% HELPERS

-spec par([doc()]) -> doc().
par(Ds) -> par(Ds, indent()).

-spec par([doc()], non_neg_integer()) -> doc().
par([], _) -> empty();
par(Ds, N) -> prettypr:par(Ds, N).

-spec above([doc()]) -> doc().
above([])       -> empty();
above([D])      -> D;
above([D | Ds]) -> lists:foldl(fun(X, Y) -> above(Y, X) end, D, Ds).

-spec beside([doc()]) -> doc().
beside([])       -> empty();
beside([D])      -> D;
beside([D | Ds]) -> lists:foldl(fun(X, Y) -> beside(Y, X) end, D, Ds).

-spec hsep([doc()]) -> doc().
hsep(Ds) -> beside(punctuate(text(" "), [ D || D <- Ds, D /= empty() ])).

-spec hsep(doc(), doc()) -> doc().
hsep(D1, D2) -> hsep([D1, D2]).

-spec punctuate(doc(), [doc()]) -> [doc()].
punctuate(_Sep, [])      -> [];
punctuate(_Sep, [D])     -> [D];
punctuate(Sep, [D | Ds]) -> [beside(D, Sep) | punctuate(Sep, Ds)].

-spec paren(doc()) -> doc().
paren(D) -> beside([text("("), D, text(")")]).

-spec indent(doc()) -> doc().
indent(D) -> nest(indent(), D).

%% block(Header, Body) ->
%%  Header
%%      Body
-spec block(doc(), doc()) -> doc().
block(Header, Body) ->
    sep([ Header, indent(Body) ]).

-spec comma_brackets(string(), string(), [doc()]) -> doc().
comma_brackets(Open, Close, Ds) ->
    beside([text(Open), par(punctuate(text(","), Ds), 0), text(Close)]).



%% PURESCRIPT

-spec pp_expr_arg(purs_expr()) -> doc().
pp_expr_arg(Expr) ->
    case lists:member(
	   element(1, Expr),
	   [expr_app, expr_if, expr_binop, expr_lambda, expr_do, expr_let, expr_case]
	  ) of
	true -> paren(pp_expr(Expr));
	false -> pp_expr(Expr)
    end.

-spec pp_expr(purs_expr()) -> doc().
pp_expr(#expr_binop{name = O, lop = L, rop = R}) ->
    block(hsep(pp_expr_arg(L), text(O)), pp_expr_arg(R));
pp_expr(#expr_num{value = Val}) ->
    if Val >= 0 -> text(integer_to_list(Val));
       true     -> paren(text(integer_to_list(Val)))
    end;
pp_expr(#expr_float{value = Val}) ->
    Doc = text(string:replace(string:replace(float_to_list(Val), "e+0", "e+"), "e-0", "e-")),
    if Val >= 0 -> Doc;
       true     -> paren(Doc)
    end;
pp_expr(#expr_string{value = Val}) ->
    text(escape_string(Val));
pp_expr(#expr_app{function = F, args = Args}) ->
    par([pp_expr(F) | lists:map(fun pp_expr_arg/1, Args)]);
pp_expr(#expr_var{name = Var}) ->
    text(Var);
pp_expr(#expr_array{value = Arr}) ->
    comma_brackets("[", "]", lists:map(fun pp_expr/1, Arr));
pp_expr(#expr_case{expr = Ex, cases = Cases}) ->
    block(hsep([text("case"), pp_expr(Ex), text("of")]),
          above([ block(hsep([pp_pat(Pat), pp_guards(Guards), text("->")]),
                        pp_expr(Expr)
                       )
                 || {Pat, Guards, Expr} <- Cases
                ])
         );
pp_expr(#expr_if{condition = C0, then = T0, else = E0 = #expr_if{}}) ->
    GetGroups = fun GetGroups(#expr_if{condition = C, then = T, else = E}, Acc) ->
                        GetGroups(E, [{C, T}|Acc]);
                    GetGroups(NotIf, Acc) ->
                        {lists:reverse(Acc), NotIf}
                end,
    {Groups, FinalElse} = GetGroups(E0, []),

    above(
      [ above(
          block(text("if"), pp_expr(C0)),
          block(text("then"), pp_expr(T0))
         ) ] ++
      [ above(block(text("else if"), pp_expr(C)),
              block(text("then"), pp_expr(T))
             )
        || {C, T} <- Groups
      ] ++
      [ block(text("else"), pp_expr(FinalElse)) ]
      );
pp_expr(#expr_if{condition = C, then = T, else = E}) ->
    above(
      [ block(text("if"), pp_expr(C))
      , block(text("then"), pp_expr(T))
      , block(text("else"), pp_expr(E))
      ]);
pp_expr(#expr_lambda{args = Args, body = Body}) ->
    block(
      hsep(lists:flatten([text("\\"),
			  [pp_pat(Arg) || Arg <- Args],
			  text("->")])),
      pp_expr(Body));
pp_expr(#expr_do{statements = Stm, return = Ret}) ->
    block(text("do"), above([pp_do_statement(E) || E <- Stm] ++ [pp_expr(Ret)]));
pp_expr(#expr_let{letdefs = LDs0, in = In0 = #expr_let{}}) ->
    GetGroups = fun GetGroups(#expr_let{letdefs = LDs, in = In}, Acc) ->
                        GetGroups(In, [LDs|Acc]);
                    GetGroups(NotLet, Acc) ->
                        {lists:reverse(Acc), NotLet}
                end,
    {Groups, FinalIn} = GetGroups(In0, []),

    above([ block(text("let   "), above([pp_letdef(LD) || LD <- LDs0])) ] ++
          [ block(text("in let"), above([pp_letdef(LD) || LD <- LDs]))
            || LDs <- Groups
          ] ++
          [ block(text("in"), pp_expr(FinalIn)) ]
         );
pp_expr(#expr_let{letdefs = LDs, in = In}) ->
    above(
      block(text("let"), above([pp_letdef(LD) || LD <- LDs])),
      block(text("in"), pp_expr(In))
     );
pp_expr(#expr_record{fields = Fields}) ->
    comma_brackets(
      "{", "}",
      [hsep(beside(text(Name), text(":")), pp_expr(Value))|| {Name, Value} <- Fields]).

escape_string(S) -> escape_string(lists:flatten(S), [$"]).

escape_string([], A) -> lists:reverse([$"|A]);
escape_string([$\n|S], A) -> escape_string(S, [$n,$\\|A]);
escape_string([$\r|S], A) -> escape_string(S, [$r,$\\|A]);
escape_string([$\t|S], A) -> escape_string(S, [$t,$\\|A]);
escape_string([$\\|S], A) -> escape_string(S, [$\\,$\\|A]);
escape_string([$"|S], A) -> escape_string(S, [$",$\\|A]);
escape_string([$'|S], A) -> escape_string(S, [$',$\\|A]);
escape_string([C|S], A) when C >= $a, C =< $z -> escape_string(S, [C|A]);
escape_string([C|S], A) when C >= $0, C =< $9 -> escape_string(S, [C|A]);
escape_string([C|S], A) when C >= $A, C =< $Z -> escape_string(S, [C|A]);
escape_string([C|S], A) ->
  %% Last chance before hex escaping...
  case lists:member(C, "!#$%&\()*+,-./:;<=>?@[\\]^_`{|}~ ") of
    true -> escape_string(S, [C|A]);
    false when C < 256 ->
      %% Normal string
      escape_string(S, [hexify(C rem 16),hexify(C div 16),$x,$\\|A]);
    false when C < 65536 ->
      %% Unicode...
      A1 = C rem 16, C1 = C div 16,
      A2 = C1 rem 16, C2 = C1 div 16,
      A3 = C2 rem 16,
      A4 = C2 div 16,
      escape_string(S, [hexify(A1),hexify(A2),hexify(A3),hexify(A4),$x,$\\|A])
  end.

hexify(X) when X >= 0, X =< 9 -> X + $0;
hexify(10) -> $A;
hexify(11) -> $B;
hexify(12) -> $C;
hexify(13) -> $D;
hexify(14) -> $E;
hexify(15) -> $F.

-spec pp_letdef(purs_letdef()) -> doc().
pp_letdef(#letval{lvalue = LV, guards = [], rvalue = RV}) ->
    block(
      hsep(pp_pat(LV), text("=")),
      pp_expr(RV)
     );
pp_letdef(LV = #letval{}) ->
    error({unsolved_guards_in_letval, LV});
pp_letdef(#letfun{name = F, args = Args, guards = Guards, body = Body}) ->
    pp_clause(F, #clause{args = Args, guards = Guards, value = Body}).

-spec pp_do_statement(purs_do_statement()) -> doc().
pp_do_statement(#do_bind{lvalue = LV, rvalue = RV}) ->
    block(hsep(pp_pat(LV), text("<-")), pp_expr(RV));
pp_do_statement(#do_let{lvalue = LV, rvalue = RV, guards = []}) ->
    hsep(text("let"), block(hsep(pp_pat(LV), text("=")), pp_expr(RV)));
pp_do_statement(#do_let{lvalue = LV, rvalue = RV, guards = Guard}) ->
    erlps_logger:die(
      "-.-",
      io_lib:format("~s", [io_lib:format("Guards in let statement made in to the pretty printer ~p ~p ~p", [LV, RV, Guard])])
    );
pp_do_statement(#do_expr{expr = Expr}) ->
    pp_expr(Expr).

-spec pp_pat(purs_pat()) -> doc().
pp_pat(pat_wildcard) ->
    text("_");
pp_pat(#pat_num{value = Val}) ->
    if Val >= 0 -> text(integer_to_list(Val));
       true     -> paren(text(integer_to_list(Val)))
    end;
pp_pat(#pat_float{value = Val}) ->
    Doc = text(string:replace(string:replace(float_to_list(Val), "e+0", "e+"), "e-0", "e-")),
    if Val >= 0 -> Doc;
       true     -> paren(Doc)
    end;
pp_pat(#pat_string{value = Val}) ->
    text(escape_string(Val));
pp_pat(#pat_var{name = Var}) ->
    text(Var);
pp_pat(#pat_array{value = Arr}) ->
    comma_brackets("[", "]", [pp_pat(E) || E <- Arr]);
pp_pat(#pat_as{name = Name, pattern = Pat}) ->
    beside([text(Name), text("@"), pp_pat(Pat)]);
pp_pat(#pat_constr{constr = Constr, args = Args}) ->
    paren(hsep([pp_pat(P) || P <- [#pat_var{name = Constr} | Args]]));
pp_pat(#pat_record{fields = Fields}) ->
    comma_brackets(
      "{", "}",
      [hsep(beside(text(Name), text(":")), pp_pat(Value))|| {Name, Value} <- Fields]).


-spec pp_guard(purs_guard()) -> doc().
pp_guard(#guard_expr{guard = Guard}) ->
    pp_expr(Guard);
pp_guard(#guard_assg{lvalue = LV, rvalue = RV}) ->
    block(hsep(pp_pat(LV), text("<-")), paren(pp_expr(RV))).

-spec pp_guards([purs_guard()]) -> doc().
pp_guards([]) -> empty();
pp_guards([G1|Guards]) ->
    above([hsep(text("|"), pp_guard(G1)) | lists:map(fun (G) -> hsep(text(","), pp_guard(G)) end, Guards)]).

-spec pp_clause(Name :: string(), purs_clause()) -> doc().
pp_clause(Name, #clause{
    args = Args,
    guards = Guards,
    value = Value}) ->
    block(
      block(
        hsep(text(Name), par(lists:map(fun pp_pat/1, Args))),
        hsep(pp_guards(Guards), text("="))),
      pp_expr(Value)).

-spec pp_type(purs_type()) -> doc().
pp_type(#type_var{name = Name}) ->
    text(Name);
pp_type(#type_app{typeconstr = Constr, args = Args}) ->
    paren(hsep([pp_type(Constr) | lists:map(fun pp_type/1, Args)]));
pp_type(#type_fun{arg = Arg, ret = Ret}) ->
    hsep([pp_type(Arg), text("->"), pp_type(Ret)]).

-spec pp_import(purs_import()) -> doc().
pp_import(#import{ path = Path
                 , alias = Alias
                 , qualify = Qualify
                 , hiding = Hiding
                 , explicit = Explicit
                 }) ->
    hsep([
        text("import"),
        if Qualify -> text("qualified"); true -> empty() end,
        beside(punctuate(text("."), lists:map(fun prettypr:text/1, Path))),
        case Alias of
            false -> empty();
            _     -> hsep(text("as"), text(Alias))
        end,
        if Hiding -> text("hiding"); true -> empty() end,
        case Explicit of
            [] -> empty();
            _  -> comma_brackets("(", ")", lists:map(fun prettypr:text/1, Explicit))
        end
    ]).

-spec pp_valdecl(purs_valdecl()) -> doc().
pp_valdecl(#valdecl{
              name = Name,
              type = Type,
              clauses = Clauses
             }) ->
    above([ case Type of
                no_type -> empty();
                _ActualType ->
                    hsep([text(Name), text("::"), pp_type(Type)])
            end
          | [ pp_clause(Name, Clause) || Clause <- Clauses ]
          ]).

-spec pp_top_decl(purs_top_decl()) -> doc().
pp_top_decl(TD = #valdecl{}) ->
    pp_valdecl(TD).

-spec pp_module(purs_module()) -> doc().
pp_module(#module{name = Name, exports = Exports, imports = Imports, decls = Decls}) ->
    Comment =
        above(lists:map(
                fun prettypr:text/1,
                [ "{-"
                , "This file has been autogenerated"
                , "DO NOT EDIT - Your changes WILL be overwritten"
                , "Use this code at your own risk - the authors are just a mischievous raccoon and a haskell devote"
                , io_lib:format("Erlscripten ~s", [erlps_transpiler:version()])
                , "-}\n"])),
    above(
      [ hsep(
          [text("module"),
           beside(text(Name),
                  case Exports of
                      all -> empty();
                      [] -> empty();  % if nothing is exported then everything is exported, right?
                      _ -> comma_brackets("(", ")", lists:map(fun prettypr:text/1, Exports))
                  end),
           text("where")])
      , Comment
      , above(lists:map(fun pp_import/1, Imports))
      , text("\n")
      , above(punctuate(text("\n"), lists:map(fun pp_top_decl/1, Decls)))
      ]).
