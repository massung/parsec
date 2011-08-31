%% parsec-style token lexer
%%
%% copyright (c) 2011 by jeffrey massung
%% all rights reserved
%%
%% lexer.erl
%%

-module(lexer).

%% grab type definitions
-include("../include/lexer.hrl").

%% exposed functionality
-export([whitespace/1,
	 lexeme/2,
	 identifier/1,
	 reserved/2,
	 operator/1,
	 reserved_op/2,
	 char_lit/1,
	 string_lit/1,
	 natural/2,
	 decimal/1,
	 hexadecimal/1,
	 octal/1,
	 binary/1,
	 real/1,
	 real_or_natural/1,
	 parens/2,
	 brackets/2,
	 braces/2,
	 angles/2
	]).

%% skip a single-line comment
single_line_comment (#lexer{comment_line=P}) ->
    parsec:do([P,parsec:many_till(parsec:any_char(),?EOL)]).

%% skip a multi-line comment
block_comment (#lexer{comment_start=S,comment_end=E}) ->
    parsec:do([S,parsec:many_till(parsec:any_char(),E)]).

%% skip any comment
comment (Lexer=#lexer{}) ->
    parsec:choice([single_line_comment(Lexer),block_comment(Lexer)]).

%% skip all whitespace and comments
whitespace (Lexer=#lexer{}) ->
    parsec:skip(parsec:choice([comment(Lexer),?SPACE])).

%% parse a lexeme
lexeme (Lexer=#lexer{},P) ->
    parsec:bind(P,fun (X) -> 
			  parsec:bind_(whitespace(Lexer),parsec:return(X)) 
		  end).

%% parse an identifier
identifier (Lexer=#lexer{ident_start=S,ident_letter=L,reserved_names=NS}) ->
    Ident=parsec:bind(S,parsec:cons(parsec:many(L))),
    lexeme(Lexer,parsec:bind(Ident,fun (Id) ->
					   case lists:member(Id,NS) of
					       false -> parsec:return(Id);
					       true -> parsec:pzero()
					   end
				   end)).

%% parse an operator
operator (Lexer=#lexer{op_start=S,op_letter=L,reserved_ops=OPS}) ->
    Operator=parsec:bind(S,parsec:cons(parsec:many(L))),
    lexeme(Lexer,parsec:bind(Operator,fun (Op) ->
					      case lists:member(Op,OPS) of
						  false -> parsec:return(Op);
						  true -> parsec:pzero()
					      end
				      end)).

%% parse a reserved word
reserved (Lexer=#lexer{ident_start=S,ident_letter=L},Name) ->
    Ident=parsec:bind(S,parsec:cons(parsec:many(L))),
    lexeme(Lexer,parsec:bind(Ident,fun (Id) ->
					   case Id==Name of
					       true -> parsec:return(Name);
					       false -> parsec:pzero()
					   end
				   end)).

%% parse a reserved operator
reserved_op (Lexer=#lexer{op_start=S,op_letter=L},Name) ->
    Operator=parsec:bind(S,parsec:cons(parsec:many(L))),
    lexeme(Lexer,parsec:bind(Operator,fun (Op) ->
					      case Op==Name of
						  true -> parsec:return(Op);
						  false -> parsec:pzero()
					      end
				      end)).

%% parse an escaped character
escaped_char () ->
    fun ({_,[$\\,$\\|CS]}) -> {$\\,CS};
	({_,[$\\,$t|CS]}) -> {$\t,CS};
	({_,[$\\,$r|CS]}) -> {$\r,CS};
	({_,[$\\,$n|CS]}) -> {$\n,CS};
	({_,[$\\,C|CS]}) -> {C,CS};
	({_,[C|CS]}) -> {C,CS};
	(_) -> pzero
    end.

%% parse a character literal
char_lit (Lexer=#lexer{}) ->
    Char=fun ([]) -> pzero;
	     (CS) -> 
		 X=lists:foldl(fun (C,Acc) -> (Acc bsl 8) bor C end,0,CS),
		 parsec:return(X)
	 end,
    Quoted=parsec:do([parsec:char($'),
		      parsec:many_till(escaped_char(),parsec:char($'))
		     ]),
    lexeme(Lexer,parsec:bind(Quoted,Char)).

%% parse a string literal
string_lit (Lexer=#lexer{}) ->
    String=fun (S) -> parsec:return(S) end,
    Quoted=parsec:do([parsec:char($"),
		      parsec:many_till(escaped_char(),parsec:char($"))
		     ]),
    lexeme(Lexer,parsec:bind(Quoted,String)).

%% parse an unsigned, natural number using a given base (2-36)
natural (Lexer=#lexer{},Base) ->
    Digits=string:left("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ",Base),
    lexeme(Lexer,parsec:bind(parsec:many1(parsec:one_of(Digits)),
		      fun (N) -> 
			      parsec:return(list_to_integer(N,Base)) 
		      end)).

%% parse an unsigned, natural number for various bases
decimal (Lexer=#lexer{}) -> natural(Lexer,10).
hexadecimal (Lexer=#lexer{}) -> natural(Lexer,16).
octal (Lexer=#lexer{}) -> natural(Lexer,8).
binary (Lexer=#lexer{}) -> natural(Lexer,2).

%% parse a real number
real (Lexer=#lexer{}) ->
    Fraction=parsec:do([parsec:char($.),?DIGITS]),
    Exp=parsec:do([parsec:one_of("eE"),
		   parsec:maybe(parsec:one_of("-+")),
		   ?DIGITS
		  ]),

    %% must be in the format \d+\.\d+(eE[-+]?\d+)?
    lexeme(Lexer,parsec:bind(parsec:capture(parsec:do([?DIGITS,
						       Fraction,
						       parsec:maybe(Exp)
						      ])),
			     fun (N) -> 
				     parsec:return(list_to_float(N))
			     end)).

%% parse a float or natural number according to many languages
real_or_natural (Lexer=#lexer{}) ->
    parsec:choice([real(Lexer),
		   parsec:bind_(parsec:string("0x"),hexadecimal(Lexer)),
		   parsec:bind_(parsec:string("0o"),octal(Lexer)),
		   parsec:bind_(parsec:string("0b"),binary(Lexer)),
		   decimal(Lexer)
		  ]).

%% parse a combinator between start and end combinators
between (Lexer=#lexer{},S,E,P) ->
    parsec:bind(parsec:bind_(lexeme(Lexer,S),lexeme(Lexer,P)),
	 fun (X) ->
		 parsec:bind_(lexeme(Lexer,E),parsec:return(X))
	 end).
    
%% parse a combinator between parens
parens (Lexer=#lexer{},P) -> between(Lexer,parsec:char($(),parsec:char($)),P).
brackets (Lexer=#lexer{},P) -> between(Lexer,parsec:char($[),parsec:char($]),P).
braces (Lexer=#lexer{},P) -> between(Lexer,parsec:char(${),parsec:char($}),P).
angles (Lexer=#lexer{},P) -> between(Lexer,parsec:char($<),parsec:char($>),P).
