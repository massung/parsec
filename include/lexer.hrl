%% parsec lexer record definition
%%
%% copyright 2011 by jeffrey massung
%% all rights reserved
%%
%% lexer.hrl
%%

-include("../include/parsec.hrl").

%% the definition of a parser combinator function
-type parse_combinator(T) :: fun ((any()) -> parse_result(T)).

%% language definition
-record(lexer, {
	  comment_start :: parse_combinator(string()),
	  comment_end :: parse_combinator(string()),
	  comment_line :: parse_combinator(string()),
	  ident_start :: parse_combinator(integer()),
	  ident_letter :: parse_combinator(integer()),
	  op_start :: parse_combinator(integer()),
	  op_letter :: parse_combinator(integer()),
	  reserved_names :: list(string()),
	  reserved_ops :: list(string())
	 }).
