-module(chat).
-author('tobbe@cslab.ericsson.se').
%%----------------------------------------------------------------------
%% File    : chat.erl
%% Created : 14 Oct 1998 by tobbe@cslab.ericsson.se
%% Function: Chat to a modem.
%%
%%           In order to be "good" functional programmers
%%           we can't make use of the timer module or (heaven
%%           forbid) the process dictionaries.
%%
%%           The run/3 function expects two functions to
%%           be provided: Send(Bytes) and Recv(Timeout,Cont), 
%%           where Cont is a continuation to be called with
%%           the received Result as argument. The Result may 
%%           either be: {ok,Bytes}, timeout, or: quit.
%%
%%           The run/3 function returns either: 'ok' or 'aborted' 
%%
%% Syntax of a chat script:
%%
%%   ChatScript ::= list_of(ChatCmd)
%%   ChatCmd    ::= {abort,String} |  % Abort when matching String
%%                  {timeout,Sec}  |  % Set timeout
%%                  break          |  % Send CR
%%                  {sex,S,X}      |  % Send string S, expect string X
%%                  {sub_sex,S,X}  |  % If a preceeding 'sex' command
%%                                    % is timedout, then send string S
%%                                    % expect string X
%%                  {delay,Sec}    |  % Wait Sec seconds
%%
%%----------------------------------------------------------------------
-vc('$Id: chat.erl,v 1.1.1.1 1999/09/05 09:54:03 tobbe Exp $ ').
-export([run/3,is_script_valid/1]).

debug(S,A) -> io:fwrite(S,A).

%% Exported interface

run(Send,Recv,ChatCmd) when function(Send),function(Recv) ->
    Aborts = [],
    Timeout = 0,
    Got = [],
    run(Send,Recv,ChatCmd,Aborts,Timeout,Got).

run(Send,Recv,[{abort,As}|Cs],Aborts,Timeout,Got) ->
    run(Send,Recv,Cs,[As|Aborts],Timeout,Got);
run(Send,Recv,[{timeout,T}|Cs],Aborts,Timeout,Got) ->
    run(Send,Recv,Cs,Aborts,T,Got);
run(Send,Recv,[break|Cs],Aborts,Timeout,Got) ->
    Send(cr()),
    run(Send,Recv,Cs,Aborts,Timeout,Got);
run(Send,Recv,[{sex,S,X}|Cs],Aborts,Timeout,Got) ->
    sex(Send,Recv,Cs,Aborts,Timeout,Got,S,X);
run(Send,Recv,[{sub_sex,S,X}|Cs],Aborts,Timeout,Got) ->
    run(Send,Recv,Cs,Aborts,Timeout,Got);
run(Send,Recv,[{delay,Sec}|Cs],Aborts,Timeout,Got) ->
    sleep(Sec),
    run(Send,Recv,Cs,Aborts,Timeout,Got);
run(Send,Recv,[],Aborts,Timeout,Got) ->
    ok.

sleep(Sec) -> receive after Sec*1000 -> true end.

%% Execute a send-expect pair.

sex(Send,Recv,Cs,Aborts,Timeout,Got,S,X) ->
    debug("Sending: ~p~n",S),
    Send(S ++ cr()),
    TS = timestamp(),
    Cont = mk_cont(Send,Recv,Cs,Aborts,Timeout,Got,TS,X),
    Recv(Timeout * 1000,Cont).

%% Create an expect continuation to be called
%% by the Recv/2 function. 

mk_cont(Send,Recv,Cs,Aborts,Timeout,Got,TS,X) ->
    fun(Result) -> expect(Send,Recv,Cs,Aborts,Timeout,Got,TS,X,Result) end.

%% Analyse the received bytes to see weather they
%% match the expected string or not. 

expect(Send,Recv,Cs,Aborts,Timeout,Got,TS,X,{ok,Bytes}) ->
    debug("Expecting: ~p  Got: ~p~n",[X,Got ++ Bytes]),
    case match(X,Aborts,Got ++ Bytes) of
	{yes,Bs}  -> 
	    run(Send,Recv,Cs,Aborts,Timeout,Bs);
	{no,NewGot} -> 
	    NewTimeout = new_timeout(Timeout,TS),
	    debug("New timeout: ~p~n",[Timeout]),
	    Cont = mk_cont(Send,Recv,Cs,Aborts,NewTimeout,NewGot,TS,X),
	    Recv(Timeout * 1000,Cont);
	aborted ->
	    aborted
    end;
expect(Send,Recv,[{sub_sex,S,X}|Cs],Aborts,Timeout,Got,_,_,timeout) ->
    debug("Timeout, Cs was: ~p  Got was: ~p  Sending sub-sex !~n",[Cs,Got]),
    sex(Send,Recv,Cs,Aborts,Timeout,Got,S,X);	      
expect(_,_,_Cs,_,_,_Got,_,_,timeout) ->
    debug("Timeout, Cs was: ~p  Got was: ~p~n",[_Cs,_Got]),
    aborted;
expect(_,_,_,_,_,_,_,_,quit) ->
    aborted.

match(X,Aborts,Bytes) ->
    case full_match(X,Bytes) of
	{true,Rest} -> {yes,Rest};
        false       -> match_aborts(Aborts,Bytes)
    end.

match_aborts([A|Aborts],Bytes) ->
    case full_match(A,Bytes) of
	{true,_} -> 
	    debug("Abort string matched ! Aborting....~n",[]),
	    aborted;
        false    -> match_aborts(Aborts,Bytes)
    end;
match_aborts([],Bytes) ->
    {no,Bytes}.

full_match(X,Bytes) ->
    case regexp:first_match(Bytes,X) of
	nomatch -> false;
	{match,Start,Length} when Length==length(X) -> 
	    {true,string:substr(Bytes,Start+Length)}
    end.

cr() -> "\r".

%% ---------------------------------------------
%% Timer handling. 

timestamp() -> 
    calendar:datetime_to_gregorian_seconds(calendar:local_time()).

new_timeout(Timeout,TS) ->
    case elapsed_time(TS,timestamp()) of
	T when T>Timeout -> 0;
	T -> Timeout - T
    end.

elapsed_time(TS1,TS2) when TS2>=TS1 -> TS2-TS1.

%% ------------------------------
%% Check if given script is valid

is_script_valid([{abort,S}|T]) when list(S)             -> is_script_valid(T);
is_script_valid([{timeout,S}|T]) when integer(S),S>=0   -> is_script_valid(T);
is_script_valid([break|T])                              -> is_script_valid(T);
is_script_valid([{sex,S,X}|T]) when list(S),list(X)     -> is_script_valid(T);
is_script_valid([{sub_sex,S,X}|T]) when list(S),list(X) -> is_script_valid(T);
is_script_valid([{delay,S}|T]) when integer(S),S>=0     -> is_script_valid(T);
is_script_valid([])                                     -> true;
is_script_valid(_)                                      -> false.
