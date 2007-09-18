-module(eppp_pred).
-author('tobbe@cslab.ericsson.se').
%%----------------------------------------------------------------------
%% File    : eppp_pred.erl
%% Created : 26 Oct 1998 by tobbe@cslab.ericsson.se
%% Function: PPP Predictor Compression Protocol (RFC-1978).
%%----------------------------------------------------------------------
-vc('$Id: eppp_pred.erl,v 1.1.1.1 1999/09/05 09:54:16 tobbe Exp $ ').
-export([compress/1,decompress/1]).
-export([nice/1,dec2hex/1]).

%% Test case, from the RFC.
%%io:fwrite(eppp_pred:nice(eppp_pred:compress(
%%"AAAAAAA\nAAAAAAA\nAAAAAAA\nAAAAAAA\nABABABA\nBABABAB\nxxxxxxx\n"))).
%%eppp_pred:decompress(eppp_pred:compress(
%%"AAAAAAA\nAAAAAAA\nAAAAAAA\nAAAAAAA\nABABABA\nBABABAB\nxxxxxxx\n")).

-define(HASH(Hash,B), (((Hash bsl 4) bxor (B)) band 16#ffff)).

%% Results in a better compression ratio. Non-RFC though...
%% -define(HASH(Hash,B), erlang:hash(B,16#ffff)).

init_guess_table() ->
    ets:new(guess_table,[]).

%% --- COMPRESS ---

compress(Bytes) when list(Bytes) ->
    io:fwrite("input len = ~p~n",[length(Bytes)]),
    Gt = init_guess_table(),
    Hash = 0,
    compress_block(Bytes,Gt,Hash).

compress_block(Bytes,Gt,Hash) ->
    Ack = [],
    Flags = 0,
    Bitmask = 1,
    I = 0,
    comp(Bytes,Gt,Hash,Flags,Ack,Bitmask,I).

comp([B|Bs],Gt,Hash,Flags,Ack,Bm,I) when I<8 ->
    case ets:lookup(Gt,Hash) of
	[{_,B}] -> %% Guess was right - don't output
	    comp(Bs,Gt,?HASH(Hash,B),Flags bor Bm,Ack,Bm bsl 1,I+1);
	_ -> %% Guess wrong, output char
	    ets:insert(Gt,{Hash,B}), 
	    comp(Bs,Gt,?HASH(Hash,B),Flags,[B|Ack],Bm bsl 1,I+1)
    end;
comp([],Gt,Hash,Flags,Ack,Bm,I) when I==8 ->
    [Flags|lists:reverse(Ack)];
comp(Bs,Gt,Hash,Flags,Ack,Bm,I) when I==8 ->
    [Flags|lists:reverse(Ack)] ++ compress_block(Bs,Gt,Hash);
comp([],Gt,Hash,Flags,Ack,Bm,I) ->
    [Flags|lists:reverse(Ack)].

%% --- DECROMPRESS ---

decompress(Bytes) when list(Bytes) ->
    io:fwrite("input len = ~p~n",[length(Bytes)]),
    Gt = init_guess_table(),
    Hash = 0,
    decompress_block(Bytes,Gt,Hash).

decompress_block([B|Bs],Gt,Hash) ->
    Ack = [],
    Flags = B,
    Bitmask = 1,
    I = 0,
    decomp(Bs,Gt,Hash,Flags,Ack,Bitmask,I).

decomp([B|Bs],Gt,Hash,Flags,Ack,Bm,I) when I<8 ->
    if ((Flags band Bm) > 0) -> %% Guess correct
	    [{_,B1}]  = ets:lookup(Gt,Hash),
	    decomp([B|Bs],Gt,?HASH(Hash,B1),Flags,[B1|Ack],Bm bsl 1,I+1);
	true -> %% Guess wrong, output char
	    ets:insert(Gt,{Hash,B}), 
	    decomp(Bs,Gt,?HASH(Hash,B),Flags,[B|Ack],Bm bsl 1,I+1)
    end;
decomp([],Gt,Hash,Flags,Ack,Bm,I) when I==8 ->
    lists:reverse(Ack);
decomp(Bs,Gt,Hash,Flags,Ack,Bm,I) when I==8 ->
    lists:reverse(Ack) ++ decompress_block(Bs,Gt,Hash);
decomp([],Gt,Hash,Flags,Ack,Bm,I) ->
    lists:reverse(Ack).


nice(Bs) ->
    io:fwrite("output len ~p~n",[length(Bs)]),
    nice(dec2hex(Bs),0).

nice(Bs,8)          -> [$\n|nice(Bs,0)];
nice([A,B,C,D|T],W) -> [A,B,C,D,$ |nice(T,W+1)];
nice(Bs,_)          -> Bs ++ "\n".

dec2hex([X|Xs]) ->
    [hex(X bsr 4), hex(X) | dec2hex(Xs)];
dec2hex([]) -> [].

hex(X) ->
    X4 = (X band 16#f),
    if X4 < 10 -> X4 + $0;
       true -> (X4-10) + $a
    end.


