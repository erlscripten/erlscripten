%%%-------------------------------------------------------------------
%%% @author radrow
%%% @copyright (C) 2020, aeternity
%%% @doc
%%%
%%% @end
%%% Created : 28. Okt 2020 14:12
%%%-------------------------------------------------------------------
-author("radrow").

-record(module, {
    name :: string(),
    imports :: [purs_import()],
    decls :: [purs_top_decl()]}).
-type purs_module() :: #module{}.
-export_type([purs_module/0]).


-record(import, {
    path :: [string()],
    alias = false :: false | string(),
    qualify = false :: boolean(),
    hiding = false :: boolean(),
    explicit = [] :: [string()]
}).
-type purs_import() :: #import{}.
-export_type([purs_import/0]).


-type purs_top_decl()
:: purs_valdecl()
.
-export_type([purs_top_decl/0]).

-record(valdecl, {
    name :: string(),
    type = no_type :: no_type | purs_type(),
    clauses :: [purs_clause()]
}).
-type purs_valdecl() :: #valdecl{}.
-export_type([purs_valdecl/0]).

-record(type_var, {name :: string()}).
-record(type_app, {typeconstr :: purs_type(), args :: [purs_type()]}).
-record(type_fun, {arg :: purs_type(), ret :: purs_type()}).
-type purs_type()
:: #type_var{}
| #type_app{}
| #type_fun{}
.
-export_type([purs_type/0]).

-record(clause, {
    args :: [purs_pat()],
    guards = [] :: [purs_guard()],
    value :: purs_expr()}).
-type purs_clause() :: #clause{}.
-export_type([purs_clause/0]).


-record(expr_binop, {name :: string(), lop :: purs_expr(), rop :: purs_expr()}).
-record(expr_num, {value :: number()}).
-record(expr_string, {value :: string()}).
-record(expr_app, {function :: purs_expr(), args :: purs_expr() | [purs_expr()]}).
-record(expr_var, {name :: string()}).
-record(expr_array, {value :: [purs_expr()]}).
-record(expr_case, {expr :: purs_expr(), cases :: [{purs_pat(), [purs_guard()], purs_expr()}]}).
-record(expr_lambda, {args :: [purs_pat()], body :: purs_expr()}).
-record(expr_do, {statements :: [purs_do_statement()], return :: purs_expr()}).
-type purs_expr()
:: #expr_binop{}
| #expr_num{}
| #expr_string{}
| #expr_app{}
| #expr_var{}
| #expr_array{}
| #expr_case{}
| #expr_lambda{}
| #expr_do{}
.
-export_type([purs_expr/0]).

-record(do_bind, {lvalue :: purs_pat(), rvalue :: purs_expr()}).
-record(do_let, {lvalue :: purs_pat(), rvalue :: purs_expr()}).
-record(do_expr, {expr :: purs_expr()}).
-type purs_do_statement()
:: #do_bind{}
| #do_let{}
| #do_expr{}
.

-record(pat_num, {value :: number()}).
-record(pat_string, {value :: string()}).
-record(pat_var, {name :: string()}).
-record(pat_array, {value :: [purs_pat()]}).
-record(pat_as, {name :: string(), pattern :: purs_pat()}).
-record(pat_constr, {constr :: string(), args = [] :: [purs_pat()]}).
-type purs_pat()
:: pat_wildcard
| #pat_num{}
| #pat_string{}
| #pat_var{}
| #pat_array{}
| #pat_as{}
| #pat_constr{}
.
-export_type([purs_pat/0]).


-record(guard_expr, {guard :: purs_expr()}).
-record(guard_assg, {lvalue :: purs_pat(), rvalue :: purs_expr()}).
-type purs_guard()
:: #guard_expr{}
| #guard_assg{}
.
-export_type([purs_guard/0]).
