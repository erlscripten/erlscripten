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
    format_module/1
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

-spec pp_expr(purs_expr()) -> doc().
pp_expr(#expr_binop{name = O, lop = L, rop = R}) when is_list(O) ->
    paren(block(hsep(pp_expr(L), text(O)), pp_expr(R)));
pp_expr(#expr_num{value = Val}) when is_integer(Val) ->
    text(integer_to_list(Val));
pp_expr(#expr_string{value = Val}) when is_list(Val) ->
    text(io_lib:format("~p", [lists:flatten(Val)]));
pp_expr(#expr_app{function = F, args = Args}) ->
    paren(par([pp_expr(F) | lists:map(fun pp_expr/1, Args)]));
pp_expr(#expr_var{name = Var}) when is_list(Var) ->
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
pp_expr(#expr_lambda{args = Args, body = Body}) ->
    block(hsep(lists:flatten([text("\\"), [pp_pat(Arg) || Arg <- Args], text("->")])),
          pp_expr(Body));
pp_expr(#expr_do{statements = Stm, return = Ret}) ->
    paren(block(text("do"), above([pp_do_statement(E) || E <- Stm] ++ [pp_expr(Ret)])));
pp_expr(#expr_record{fields = Fields}) ->
    comma_brackets(
      "{", "}",
      [hsep(beside(text(Name), text(":")), pp_expr(Value))|| {Name, Value} <- Fields]).

-spec pp_do_statement(purs_do_statement()) -> doc().
pp_do_statement(#do_bind{lvalue = LV, rvalue = RV}) ->
    block(hsep(pp_pat(LV), text("<-")), pp_expr(RV));
pp_do_statement(#do_let{lvalue = LV, rvalue = RV}) ->
    block(hsep([text("let"), pp_pat(LV), text("=")]), pp_expr(RV));
pp_do_statement(#do_expr{expr = Expr}) ->
    pp_expr(Expr).

-spec pp_pat(purs_pat()) -> doc().
pp_pat(pat_wildcard) ->
    text("_");
pp_pat(#pat_num{value = Val}) ->
    text(integer_to_list(Val));
pp_pat(#pat_string{value = Val}) ->
    text(io_lib:format("~p", [Val]));
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
    block(hsep(pp_pat(LV), text("<-")), pp_expr(RV)).

-spec pp_guards([purs_guard()]) -> doc().
pp_guards([]) -> empty();
pp_guards(Guards) ->
    hsep(text("|"),
          par(punctuate(text(","), lists:map(fun pp_guard/1, Guards)))).

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
                      _ -> comma_brackets("(", ")", lists:map(fun prettypr:text/1, Exports))
                  end),
           text("where")])
      , Comment
      , above(lists:map(fun pp_import/1, Imports))
      , text("\n")
      , above(punctuate(text("\n"), lists:map(fun pp_top_decl/1, Decls)))
      ]).
