%% parsec-style parser for erlang
%%
%% copyright (c) 2011 by jeffrey massung
%% all rights reserved
%%
%% parsec.erl
%%

-module(parsec).

%% grab type definitions
-include("../include/parsec.hrl").

%% exposed functionality
-export([bind/2,
	 bind_/2,
	 return/1,
	 do/1,
	 parse/2,
	 pzero/0,
	 capture/1,
	 any_char/0,
	 eof/0,
	 choice/1,
	 char/1,
	 one_of/1,
	 none_of/1,
	 string/1,
	 option/2,
	 many1/1,
	 many/1,
	 maybe/1,
	 skip/1,
	 between/3,
	 cons/1,
	 sep_by1/2,
	 sep_by/2,
	 sep_end_by1/2,
	 sep_end_by/2,
	 many_till/2
	]).

%% bind from one parse combinator to the next
bind (M,F) ->
    fun (ST) ->
	    case M(ST) of
		{X,NewState} -> (F(X))({ok,NewState});
		Error -> Error
	    end
    end.

%% bind two combinators, ignoring the intermediate result
bind_ (M,P) ->
    fun (ST) ->
	    case M(ST) of
		{_,NewState} -> P({ok,NewState});
		Error -> Error
	    end
    end.

%% terminate a bind chain and return a value
return (X) ->
    fun ({_,Src}) -> {X,Src};
	(Error) -> Error
    end.

%% bind a list of combinators, ignoring all results except the last
do ([P]) -> bind(P,fun (X) -> return(X) end);
do ([P|PS]) ->
    XS=do(PS),
    bind(P,fun (_) -> XS end).

%% evaluate a parse combinator
parse (String,P) ->
    case P({undefined,String}) of
	{X,Rest} ->
	    {ok,X,Rest};
	Error ->
	    {error,Error}
    end.

%% force a parse failure
pzero () -> fun (_) -> pzero end.

%% return the entire set of characters parsed by a combinator
capture (P) ->
    fun (ST={_,Cur}) ->
	    case P(ST) of
		{_,New} ->
		    {string:left(Cur,length(Cur)-length(New)),New};
		Error ->
		    Error
	    end
    end.

%% match any character except eof
any_char () -> 
    fun ({_,[C|CS]}) -> {C,CS};
	(Error) -> Error
    end.

%% match the end of the parse stream
eof () ->
    fun ({_,[]}) -> {eof,[]};
	(_) -> pzero
    end.

%% return the successful match from a list
choice (PS) ->
    fun (ST) ->
    	    Accum=fun (_,Acc={_,_}) -> Acc;
    		      (P,_) -> P(ST)
    		  end,
    	    lists:foldl(Accum,pzero,PS)
    end.

%% match a specific character
char (C) ->
    fun ({_,[X|CS]}) ->
	    case X==C of
		true -> {C,CS};
		false -> pzero
	    end;
	(_) -> 
	    pzero
    end.

%% match any character in a set
one_of (Set) ->
    fun ({_,[C|CS]}) ->
	    case string:chr(Set,C) of
		0 -> pzero;
		_ -> {C,CS}
	    end;
	(_) ->
	    pzero
    end.

%% match any character except those in the set
none_of (Set) ->
    fun ({_,[C|CS]}) ->
	    case string:chr(Set,C) of
		0 -> {C,CS};
		_ -> pzero
	    end;
	(_) ->
	    pzero
    end.

%% match a series of characters in order
string (S) ->
    bind_(do([char(C) || C <- S]),return(S)).

%% optionally parse a combinator or return a default value
option (X,P) ->
    choice([P,return(X)]).

%% match a combinator one or more times
many1 (P) ->
    bind(P,fun (X) -> bind(many(P), fun(XS) -> return([X|XS]) end) end).

%% match a combinator zero or more times
many (P) ->
    option([],many1(P)).

%% ignore a combinator if present
maybe (P) ->
    option(undefined,bind_(P,return(undefined))).

%% skip a combinator zero or more times
skip (P) ->
    maybe(many(P)).

%% capture p between start and term
between (Start,Term,P) ->
    bind_(Start,bind(P,fun (X) -> bind_(Term,return(X)) end)).

%% prepends current parse result to p
cons (P) ->
    fun (X) ->
	    bind(P,fun (XS) -> return([X|XS]) end)
    end.

%% captures p separated by sep one or more times
sep_by1 (P,Sep) ->
    bind(P,cons(many(bind_(Sep,P)))).

%% captures p separated by sep zero or more times
sep_by (P,Sep) ->
    option([],sep_by1(P,Sep)).

%% captures p separated by sep and optionally ending with sep
sep_end_by1 (P,Sep) ->
    Rest=fun (X) ->
		 bind(bind_(Sep,sep_end_by(P,Sep)),
		      fun (XS) -> return([X|XS]) end)
	 end,
    bind(P,fun (X) -> choice([Rest(X),return([X])]) end).

%% captures p separated by sep and optionally ending with sep
sep_end_by (P,Sep) ->
    option([],sep_end_by1(P,Sep)).

%% keeps capturing p until term is found
many_till (P,Term) ->
    choice([bind_(Term,return([])),
	    bind(P,fun (X) ->
			   bind(many_till(P,Term),fun (XS) -> 
							  return([X|XS]) 
						  end)
		   end)
	   ]).
