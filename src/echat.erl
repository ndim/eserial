-module(echat).
-author('tnt@home.se').
%%% ------------------------------------------------------------------
%%% File    : eppp_chat.erl
%%% Created : 14 Oct 1998 by tnt@home.se
%%% Function: A chat program (see the description below).
%%%
%%% Copyright (C) 1999  Torbjörn Törnkvist, tnt@home.se
%%%
%%% This program is free software; you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation; either version 2 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
%%%
%%% --------------------------------------------------------------------
%%%
%%%   DESCRIPTION
%%%   -----------
%%%   The idea here was to write the most general chat
%%%   program you could think of, i.e it should be possible 
%%%   plug it in anywhere. The only thing needed are ways
%%%   for the chat program to send and receive bytes as well
%%%   as log important information. Further more, it shouldn't 
%%%   perform any other side effects, which meant that we 
%%%   couldn't make use of the timer module or (heaven forbid) 
%%%   the process dictionaries. 
%%%
%%%   The run/4 function expects three functions to be provided: 
%%%
%%%      Send(Bytes) 
%%%      Recv(Timeout,Cont)
%%%      Log(FormatStr,Args)
%%%
%%%   where: Bytes is an I/O-list to be sent from the chat program.
%%%          Cont is a continuation to be called with the received 
%%%          Result as argument. The Result may either be: 
%%%
%%%            Result ::= {ok,Bytes} | timeout | quit
%%%
%%%          FormatStr and Args are the same type of arguments as
%%%          the io:format/2 function uses.
%%%
%%%   The run/4 function returns either:  ok | aborted
%%%
%%%   Syntax of a chat script:
%%%   ========================
%%%
%%%   ChatScript ::= list_of(ChatCmd)
%%%   ChatCmd    ::= {abort,String} |  % Abort when matching String
%%%                  {timeout,Sec}  |  % Set timeout
%%%                  break          |  % Send CR
%%%                  {sex,S,X}      |  % Send string S, expect string X
%%%                  {sub_sex,S,X}  |  % If a preceeding 'sex' command
%%%                                    % is timedout, then send string S
%%%                                    % expect string X
%%%                  {delay,Sec}       % Wait Sec seconds
%%%
%%% ------------------------------------------------------------------
-vc('$Id: echat.erl,v 1.1 1999/09/05 10:01:49 tobbe Exp $ ').
-export([run/4,is_script_valid/1]).


%% Exported interface

run(Send,Recv,Log,ChatCmd) when function(Send),function(Recv),function(Log) ->
    Aborts = [],
    Timeout = 0,
    Got = [],
    run(Send,Recv,Log,ChatCmd,Aborts,Timeout,Got).

run(Send,Recv,Log,[{abort,As}|Cs],Aborts,Timeout,Got) ->
    run(Send,Recv,Log,Cs,[As|Aborts],Timeout,Got);
run(Send,Recv,Log,[{timeout,T}|Cs],Aborts,Timeout,Got) ->
    run(Send,Recv,Log,Cs,Aborts,T,Got);
run(Send,Recv,Log,[break|Cs],Aborts,Timeout,Got) ->
    Send(cr()),
    run(Send,Recv,Log,Cs,Aborts,Timeout,Got);
run(Send,Recv,Log,[{sex,S,X}|Cs],Aborts,Timeout,Got) ->
    sex(Send,Recv,Log,Cs,Aborts,Timeout,Got,S,X);
run(Send,Recv,Log,[{sub_sex,S,X}|Cs],Aborts,Timeout,Got) ->
    run(Send,Recv,Log,Cs,Aborts,Timeout,Got);
run(Send,Recv,Log,[{delay,Sec}|Cs],Aborts,Timeout,Got) ->
    sleep(Sec),
    run(Send,Recv,Log,Cs,Aborts,Timeout,Got);
run(Send,Recv,Log,[],Aborts,Timeout,Got) ->
    ok.

sleep(Sec) -> receive after Sec*1000 -> true end.

%% Execute a send-expect pair.

sex(Send,Recv,Log,Cs,Aborts,Timeout,Got,S,X) ->
    Log("Sending: ~p~n",S),
    Send(S ++ cr()),
    TS = timestamp(),
    Cont = mk_cont(Send,Recv,Log,Cs,Aborts,Timeout,Got,TS,X),
    Recv(Timeout * 1000,Cont).

%% Create an expect continuation to be called
%% by the Recv/2 function. 

mk_cont(Send,Recv,Log,Cs,Aborts,Timeout,Got,TS,X) ->
    fun(Result) -> expect(Send,Recv,Log,Cs,Aborts,Timeout,Got,TS,X,Result) end.

%% Analyse the received bytes to see whether they
%% match the expected string or not. 

expect(Send,Recv,Log,Cs,Aborts,Timeout,Got,TS,X,{ok,Bytes}) ->
    case match(X,Aborts,Got ++ Bytes) of
	{yes,Bs}  -> 
	    Log("Expecting: ~p  Got: ~p , match=YES ~n",[X,Got ++ Bytes]),
	    run(Send,Recv,Log,Cs,Aborts,Timeout,Bs);
	{no,NewGot} -> 
	    NewTimeout = new_timeout(Timeout,TS),
	    Log("Expecting: ~p  Got: ~p , match=NO , "
		"new timeout: ~p~n",
		[X,Got ++ Bytes,Timeout]),
	    Cont = mk_cont(Send,Recv,Log,Cs,Aborts,NewTimeout,NewGot,TS,X),
	    Recv(Timeout * 1000,Cont);
	aborted ->
	    aborted
    end;
expect(Send,Recv,Log,[{sub_sex,S,X}|Cs],Aborts,Timeout,Got,_,_,timeout) ->
    Log("Timeout, Cs was: ~p  Got was: ~p  Sending sub-sex !~n",[Cs,Got]),
    sex(Send,Recv,Log,Cs,Aborts,Timeout,Got,S,X);	      
expect(_,_,Log,_Cs,_,_,_Got,_,_,timeout) ->
    Log("Timeout, Cs was: ~p  Got was: ~p , aborting...~n",[_Cs,_Got]),
    aborted;
expect(_,_,Log,_,_,_,_,_,_,quit) ->
    Log("Quit , aborting...~n",[]),
    aborted.

match(X,Aborts,Bytes) ->
    case full_match(X,Bytes) of
	{true,Rest} -> {yes,Rest};
        false       -> match_aborts(Aborts,Bytes)
    end.

match_aborts([A|Aborts],Bytes) ->
    case full_match(A,Bytes) of
	{true,_} -> aborted;
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

%% --------------------------------------------
%% Timer handling
%% ---------------
%% Here we have to take the elapsed time into 
%% account when we compute the new timeout.
%% --------------------------------------------

timestamp() -> 
    calendar:datetime_to_gregorian_seconds(calendar:local_time()).

new_timeout(Timeout,TS) ->
    case elapsed_time(TS,timestamp()) of
	T when T>Timeout -> 0;
	T -> Timeout - T
    end.

elapsed_time(TS1,TS2) when TS2>=TS1 -> TS2-TS1.

%% -------------------------------------
%% Check if given script is a valid one

is_script_valid([{abort,S}|T]) when list(S)             -> is_script_valid(T);
is_script_valid([{timeout,S}|T]) when integer(S),S>=0   -> is_script_valid(T);
is_script_valid([break|T])                              -> is_script_valid(T);
is_script_valid([{sex,S,X}|T]) when list(S),list(X)     -> is_script_valid(T);
is_script_valid([{sub_sex,S,X}|T]) when list(S),list(X) -> is_script_valid(T);
is_script_valid([{delay,S}|T]) when integer(S),S>=0     -> is_script_valid(T);
is_script_valid([])                                     -> true;
is_script_valid(_)                                      -> false.
