%% example lisp parser
%%
%% lisp_parser.erl
%%

-module(lisp_parser).
-export([parse/1]).

%% get the lexer definition
-include("../include/lexer.hrl").

special_symbol () -> parsec:one_of("~!@$%&*-+=<>/").

%% define the lisp lexer
make_lexer () ->
    #lexer{
	    comment_start=parsec:string("#|"),
	    comment_end=parsec:string("|#"),
	    comment_line=parsec:string(";"),
	    ident_start=parsec:choice([?LETTER,special_symbol()]),
	    ident_letter=parsec:choice([?LETTER,special_symbol()]),
	    op_start=pzero,
	    op_letter=pzero,
	    reserved_names=["lambda","let"],
	    reserved_ops=[]
	  }.

as_list (X) -> parsec:return({list,X}).
as_num (X) -> parsec:return({num,X}).
as_string (X) -> parsec:return({str,X}).
as_char (X) -> parsec:return({char,X}).
as_ident (X) -> parsec:return({id,X}).
as_op (X) -> parsec:return({op,X}).

%% parse a form
form (Lexer) ->
    fun (ST) ->
	    Parser=parsec:choice(
		     [parsec:bind(lexer:parens(Lexer,parsec:many(form(Lexer))),fun as_list/1),
		      parsec:bind(lexer:real_or_natural(Lexer),fun as_num/1),
		      parsec:bind(lexer:identifier(Lexer),fun as_ident/1),
		      parsec:bind(lexer:string_lit(Lexer),fun as_string/1),
		      parsec:bind(lexer:char_lit(Lexer),fun as_char/1),
		      parsec:bind(lexer:reserved(Lexer,"lambda"),fun as_op/1)
		     ]),
	    Parser(ST)
    end.

%% parse a form from a string
parse (S) ->
    Lexer=make_lexer(),
    WS=lexer:whitespace(Lexer),
    parsec:parse(S,parsec:do([WS,form(Lexer)])).
