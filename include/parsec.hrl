%% parsec record definitions
%%
%% copyright 2011 by jeffrey massung
%% all rights reserved
%%
%% parsec.hrl
%%

%% parse state is a value and an input stream
-type parse_state(T) :: {T,string()}.

%% return type of a parse combinator
-type parse_result(T) :: parse_state(T) | pzero.

%% common combinators
-define(UPPER_LETTER,parsec:one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ")).
-define(LOWER_LETTER,parsec:one_of("abcdefghijklmnopqrstuvwxyz")).
-define(LETTER,parsec:choice([?UPPER_LETTER,?LOWER_LETTER])).
-define(LETTERS,parsec:many1(?LETTER)).
-define(DIGIT,parsec:one_of("0123456789")).
-define(DIGITS,parsec:many1(?DIGIT)).
-define(HEX_DIGIT,parsec:one_of("0123456789abcdefABCDEF")).
-define(HEX_DIGITS,parsec:many1(?HEX_DIGIT)).
-define(OCT_DIGIT,parsec:one_of("01234567")).
-define(OCT_DIGITS,parsec:many1(?OCT_DIGIT)).
-define(ALPHANUM,parsec:choice([?LETTER,?DIGIT])).
-define(PUNCTUATION,parsec:one_of("!@#$%^&*()-=+[]{}\\|;:'\",./<>?~`")).
-define(SPACE,parsec:one_of(" \t")).
-define(SPACES,parsec:many1(?SPACE)).
-define(NEWLINE,parsec:one_of("\r\n")).
-define(NEWLINES,parsec:many1(?NEWLINES)).
-define(EOL,parsec:choice([parsec:eof(),parsec:do([?NEWLINE])])).
