%%%-------------------------------------------------------------------
%%% @author radrow
%%% @copyright (C) 2020, aeternity
%%% @doc
%%%
%%% @end
%%% Created : 26. Okt 2020 16:31
%%%-------------------------------------------------------------------
-module(erlps_purescript).
-author("radrow").

-include("erlps_purescript.hrl").

%% API
-export([
    purs_expr_to_str/1,
    purs_pat_to_str/1,
    purs_guard_to_str/1,
    purs_clause_to_str/1,
    purs_type_to_str/1,
    purs_typedecl_to_str/1,
    purs_import_to_str/1,
    purs_top_decl_to_str/1,
    purs_module_to_str/1
]).

%% TODO: USE PRETTY PRINTER

-spec purs_expr_to_str(purs_expr()) -> string().
purs_expr_to_str(#expr_binop{name = O, lop = L, rop = R}) ->
    io_lib:format("(~s ~s ~s)", [purs_expr_to_str(L), O, purs_expr_to_str(R)]);
purs_expr_to_str(#expr_num{value = Val}) ->
    io_lib:format("~p", [Val]);
purs_expr_to_str(#expr_string{value = Val}) ->
    io_lib:format("~p", [lists:flatten(Val)]);
purs_expr_to_str(#expr_app{function = F, args = Args}) ->
    io_lib:format("(~s)", [string:join([purs_expr_to_str(P) || P <- [F | Args]], " ")]);
purs_expr_to_str(#expr_var{name = Var}) ->
    Var;
purs_expr_to_str(#expr_array{value = Arr}) ->
    io_lib:format("[~s]", [string:join([purs_expr_to_str(E) || E <- Arr], ", ")]);
purs_expr_to_str(#expr_case{expr = Ex, cases = Cases}) ->
    CasesStr = string:join(
        [ io_lib:format("~s~s -> ~s", [purs_pat_to_str(Pat), purs_guards_to_str(Guards), purs_expr_to_str(Expr)])
            || {Pat, Guards, Expr} <- Cases
        ], "; "),
    io_lib:format("case ~s of {~s}", [purs_expr_to_str(Ex), CasesStr]);
purs_expr_to_str(#expr_lambda{args = Args, body = Body}) ->
    io_lib:format("\\~s -> ~s", [string:join([purs_pat_to_str(Arg) || Arg <- Args], " "), purs_expr_to_str(Body)]).


-spec purs_pat_to_str(purs_pat()) -> string().
purs_pat_to_str(pat_wildcard) ->
    "_";
purs_pat_to_str(#pat_num{value = Val}) ->
    io_lib:format("~p", [Val]);
purs_pat_to_str(#pat_string{value = Val}) ->
    io_lib:format("~p", [Val]);
purs_pat_to_str(#pat_var{name = Var}) ->
    Var;
purs_pat_to_str(#pat_array{value = Arr}) ->
    io_lib:format("[~s]", [string:join([purs_pat_to_str(E) || E <- Arr], ", ")]);
purs_pat_to_str(#pat_as{name = Name, pattern = Pat}) ->
    io_lib:format("~s@(~s)", [Name, purs_pat_to_str(Pat)]);
purs_pat_to_str(#pat_constr{constr = Constr, args = Args}) ->
    io_lib:format("(~s)", [string:join([purs_pat_to_str(P) || P <- [#pat_var{name = Constr} | Args]], " ")]).


-spec purs_guard_to_str(purs_guard()) -> string().
purs_guard_to_str(#guard_expr{guard = Guard}) ->
    purs_expr_to_str(Guard);
purs_guard_to_str(#guard_assg{lvalue = LV, rvalue = RV}) ->
    io_lib:format("~s <- ~s", [purs_pat_to_str(LV), purs_expr_to_str(RV)]).

-spec purs_guards_to_str([purs_guard()]) -> string().
purs_guards_to_str([]) -> "";
purs_guards_to_str(Guards) ->
    GuardsStr = string:join([purs_guard_to_str(G) || G <- Guards], ", "),
    io_lib:format(" | ~s", [GuardsStr]).

-spec purs_clause_to_str(purs_clause()) -> string().
purs_clause_to_str(#clause{
    name = Name,
    args = Args,
    guards = Guards,
    value = Value}) ->
    ArgsStr = string:join([purs_pat_to_str(P) || P <- Args], " "),
    GuardsStr = purs_guards_to_str(Guards),
    ValueStr = purs_expr_to_str(Value),
    io_lib:format("~s ~s~s = ~s", [Name, ArgsStr, GuardsStr, ValueStr]).

-spec purs_type_to_str(purs_type()) -> string().
purs_type_to_str(#type_var{name = Name}) ->
    Name;
purs_type_to_str(#type_app{typeconstr = Constr, args = Args}) ->
    io_lib:format("(~s ~s)", [purs_type_to_str(Constr),
        string:join([purs_type_to_str(A) || A <- Args], " ")]);
purs_type_to_str(#type_fun{arg = Arg, ret = Ret}) ->
    io_lib:format("(~s -> ~s)", [purs_type_to_str(Arg), purs_type_to_str(Ret)]).

-spec purs_typedecl_to_str(purs_typedecl()) -> string().
purs_typedecl_to_str(#typedecl{name = Name, type = Type}) ->
    io_lib:format("~s :: Partial => ~s", [Name, purs_type_to_str(Type)]).

-spec purs_import_to_str(purs_import()) -> string().
purs_import_to_str(#import{
    path = Path,
    alias = Alias,
    qualify = Qualify,
    hiding = Hiding,
    explicit = Explicit
}) ->
    io_lib:format("import ~s ~s ~s ~s ~s", [
        if Qualify -> "qualified"; true -> "" end,
        string:join(Path, "."),
        case Alias of
            false -> "";
            _ -> "as " ++ Alias
        end,
        if Hiding -> "hiding"; true -> "" end,
        case Explicit of
            [] -> "";
            _ -> "(" ++ string:join(Explicit, ", ") ++ ")"
        end
    ]).

-spec purs_top_decl_to_str(purs_top_decl()) -> string().
purs_top_decl_to_str(#top_typedecl{typedecl = Decl}) ->
    purs_typedecl_to_str(Decl);
purs_top_decl_to_str(#top_clause{clause = Clause}) ->
    purs_clause_to_str(Clause).

-spec purs_module_to_str(purs_module()) -> string().
purs_module_to_str(#module{name = Name, imports = Imports, decls = Decls}) ->
    Format =
        "module ~s where\n"
        "{-\n"
        "This file has been autogenerated\n"
        "DO NOT EDIT - Your changes WILL be overwritten\n"
        "Use this code at your own risk - the authors are just a mischievous raccoon and a haskell devote\n"
        "Erlscripten ~s\n"
        "-}\n"
        "\n"
        "~s\n"
        "\n\n"
        "~s",
    ImportStr = string:join([purs_import_to_str(I) || I <- Imports], "\n"),
    DeclsStr = string:join([purs_top_decl_to_str(D) || D <- Decls], "\n\n"),
    io_lib:format(Format, [Name, erlscripten:version(), ImportStr, DeclsStr]).
