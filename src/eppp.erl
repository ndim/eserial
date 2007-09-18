-module(eppp).
-author('klacke@erix.ericsson.se').
%%% -------------------------------------------------------------------
%%% File    : eppp.erl
%%% Author  : Claes Wikstrom <klacke@erix.ericsson.se>
%%% Purpose : Erlang PPP implementation.
%%% Created : 25 Feb 1997 by Claes Wikstrom <klacke@erix.ericsson.se>
%%%
%%% Modified: 20 Oct 1998 by tobbe@cslab.ericsson.se
%%%           Completely re-structured, re-written and enhanced.
%%%
%%%   This is the PPP server that ties together all protocols 
%%%   of the PPP family.
%%% -------------------------------------------------------------------
-vc('$Id: eppp.erl,v 1.1.1.1 1999/09/05 09:54:04 tobbe Exp $ ').
-export([start/1,stop/1]).
-export([i16/1,i16/2,i32/1,i32/4,ts/0,dec2hex/1]).
-export([version/0,init/1]).

-import(eppp_log,[emsg/2,imsg/2,w3/3]).

-define(PPP_ALLSTATION, 16#ff).    %% All-Stations broadcast address
-define(PPP_UI, 16#03).            %% Unnumbered Information

-record(eppp, 
	{dict,               %% Dictionary containing config info
	 lcp,                %% Handle to LCP
	 log,                %% Handle to Log mechanism
	 serial,             %% Handle to the serial device
	 state=dead          %% dead | establish | authenticate |
                             %% network | terminate
	}).

version() ->
    [_,Rev|_] = string:tokens("$Revision: 1.1.1.1 $ "," "),Rev.


%% ---------------------------------------------
%% Start a PPP session according to the session
%% tag specified in the configure file.

start(Session) when atom(Session) ->
    timer:start(),
    spawn_link(?MODULE,init,[Session]).

stop(Pid) when pid(Pid) -> 
    Pid ! stop.

%% ---------------
%% The server code
%% ---------------

init(Session) ->
    process_flag(trap_exit, true),
    {Log,Dict} = do_init(Session),
    Serial = start_serial(Dict),
    Lcp = init_lcp(Serial),
    S = #eppp{dict=Dict,lcp=Lcp,log=Log,serial=Serial},
    sleep(500),                           % DEBUGGING !!!
    w3(Log,"~w: Entering main loop !!~n",[?MODULE]),
    run(S,Serial).


%% TEMPORARY, JUST TO LET US START AS A CLIENT !!!
run(S,Serial) ->
    loop(dispatch_lcp(dispatch_lcp(S,up),open),Serial,false).

do_init(Session) ->
    case eppp_conf:initrc() of
	{ok,Dict} -> 
	    case init_logger(Dict) of
		{ok,Log} -> 
		    {Log,eppp_conf:set_session_values(Dict,Session)};
		{error,Reason} -> exit(Reason)
	    end;
	{error,Reason} -> 
	    exit(Reason)
    end.

init_lcp(Serial) ->
    Lcp = eppp_lcp:init(mk_send(Serial),mk_start_timer(lcp),mk_stop_timer()),
    set_lcp_auth(set_log_level(Lcp)).

set_log_level(Lcp) ->
    case getopt(Dict,log) of
	Level when integer(Level) -> eppp_lcp:set_log(Lcp,Level);
	_                         -> Lcp
    end.

%% Currently we only speak PAP
set_lcp_auth(Lcp) ->
    eppp_lcp:set_auth(Lcp,pap).

%% -----------------------------------------------
%% Create the callback routines for sending bytes,
%% starting and stopping the timer.

mk_send(Serial) ->
    fun(Ptype,Bytes) -> send(Serial,Ptype,Bytes) end.

send(Serial,Ptype,Bytes) ->
    w3(getopt(Dict,log),
       "~w: Sending ~w packet: DEC-BEGIN ~w DEC-END~n"
       "HEX-BEGIN ~w HEX-END~n",
       fun() -> [?MODULE,Ptype,Bytes,dec2hex(Bytes)] end),
    eppp_serial:send(Serial,encode(Ptype,Bytes)).

%% Each underlying protocol may have added info to the 
%% TagList. This makes it possible for every protocol 
%% layer to identify the timeout.
	    
mk_start_timer(Proto) ->
    Self = self(),
    fun(Timeout,TagList) when integer(Timeout),list(TagList) ->
	    w3(getopt(Dict,log),"~w: Starting timer, timeout = ~w~n",[?MODULE,Timeout]),
	    timer:send_after(Timeout,Self,{timeout,[Proto|TagList]})
    end.
	    
mk_stop_timer() -> 
    fun(Ticket) -> timer:cancel(Ticket) end.

%% ---------------
%% The server loop
%% ---------------

loop(S,Serial,More) ->
    receive
	{Serial,Msg} ->
	    dispatch_serial(S,Serial,Msg,More);
	{timeout,TagList} ->
	    io:format("EPPP: Got a timeout, TagList = ~w~n",[TagList]),
	    S2 = dispatch_timeout(S,TagList),
	    loop(S2,Serial,false);
	{'EXIT',Serial,Reason} ->
	    emsg("eppp: Serial device has died, reason: ~p~n",[Reason]),
	    exit(Reason);
	_ ->
	    loop(S,Serial,More)
    end.

dispatch_timeout(S,[lcp|TagList]) -> dispatch_lcp(S,{timeout,TagList}).

%% ------------------------------------
%% PART DEALING WITH THE SERIAL DEVICE
%% ------------------------------------

%% Start the serial device, and configure it accordingly.

start_serial(Dict) ->
    Serial = eppp_serial:start(),
    configure_serial(Dict,Serial),
    Serial.
    
configure_serial(Dict,Serial) ->
    eppp_serial:log_level(Serial,getopt(Dict,log)),
    eppp_serial:speed(Serial,getopt(Dict,speed)),
    eppp_serial:parity(Serial,getopt(Dict,parity)),
    eppp_serial:open(Serial,getopt(Dict,device)),
    start_chatting(Dict,Serial).

start_chatting(Dict,Serial) ->
    Self = self(),
    F = fun(ChatResult) -> Self ! {chat_result,ChatResult} end,
    eppp_serial:chat(Serial,getopt(Dict,dial),F),
    receive
	{chat_result,ok} -> 
	    imsg("PPP: Chat succeded !~n",[]),
	    ok;
	{chat_result,_}  ->
	    emsg("PPP: Chat failed, ppp session terminated...~n",[]),
	    exit(chat_failed)
    end.


%% Take care of messages from the serial device

dispatch_serial(S,Serial,{data,B},More) when S#eppp.state==dead ->
    Bs = binary_to_list(B),
    io:format("~nEPPP: got ~w~n",[Bs]),
    io:format("            ~p~n~n",[dec2hex(Bs)]),
    case hdlc_detect(Bs) of
	{ok,Bytes} ->
	    io:format("EPPP: YES detected valid hdlc frame !~n"),
	    do_hdlc_decode(S#eppp{state=establish},Serial,Bytes,More);
	_ ->
	    io:format("EPPP: NO wrong hdlc frame !~n"),
	    loop(S,Serial,false)
    end;
dispatch_serial(S,Serial,{data,B},More) ->
    Bytes = binary_to_list(B),
    emsg("~neppp: got ~w~n",[Bytes]),
    emsg("            ~p~n~n",[dec2hex(Bytes)]),
    do_hdlc_decode(S,Serial,Bytes,More).

do_hdlc_decode(S,Serial,Bytes,More) ->
    case catch hdlc_decode(Bytes,More) of
	{ok,Bs} ->
	    S2 = incoming(S,Bs),
	    loop(S2,Serial,false);
	{more,F} ->
	    loop(S,Serial,F);
	{error,Reason} ->
	    emsg("PPP: error reason: ~p~n",[Reason]),
	    loop(S,Serial,false);
	{'EXIT',Reason} ->
	    emsg("PPP: crashed reason: ~p~n",[Reason]),
	    loop(S,Serial,false)
    end.

%% Check for valid HDLC frame headers !

hdlc_detect([$\176,$\377,$\003,$\300,$\041|T]) -> 
    {ok,[$\176,$\377,$\003,$\300,$\041|T]};
hdlc_detect([$\176,$\377,$\175,$\043,$\300,$\041|T]) -> 
    {ok,[$\176,$\377,$\175,$\043,$\300,$\041|T]};
hdlc_detect([$\176,$\177,$\175,$\043,$\100,$\041|T]) ->
    {ok,[$\176,$\177,$\175,$\043,$\100,$\041|T]};
hdlc_detect([$\176,$\175,$\337,$\175,$\043,$\300,$\041|T]) ->
    {ok,[$\176,$\175,$\337,$\175,$\043,$\300,$\041|T]};
hdlc_detect([$\176,$\175,$\137,$\175,$\043,$\100,$\041|T]) ->
    {ok,[$\176,$\175,$\137,$\175,$\043,$\100,$\041|T]};
hdlc_detect([_|T]) -> hdlc_detect(T);
hdlc_detect([])    -> false.

hdlc_decode(Bytes,false) -> eppp_hdlc:decode(Bytes);
hdlc_decode(Bytes,F) when function(F) -> F(Bytes).

incoming(S,Bytes) ->
    case Bytes of
	[?PPP_ALLSTATION, ?PPP_UI, 16#C0, 16#21 | Tail] ->
	    dispatch_lcp(S,Tail);
	[?PPP_ALLSTATION, ?PPP_UI, 0, 16#21 | Tail] ->
	    ip(S,Tail);
	[?PPP_ALLSTATION, ?PPP_UI, 16#80, 16#21 | Tail] ->
	    ncp(S,Tail);
	[?PPP_ALLSTATION, ?PPP_UI, 16#C0, 16#23 | Tail] ->
	    pap(S,Tail);
	[?PPP_ALLSTATION, ?PPP_UI, Hi, Lo|Tail] ->
	    unknown_code(S,Hi,Lo,Tail)
    end.

ip(_,_) -> nyi.
ncp(_,_) -> nyi.
pap(_,_) -> nyi.
unknown_code(_,_,_,_) -> nyi.

%% -----------------------------------------------------
%% Call the LCP machine and analyse the returned result

dispatch_lcp(S,Event) ->
    case call_lcp(S#eppp.lcp,Event) of
	{Tag,NewLcp} ->
	    lcp_tag(S,Tag,NewLcp);
	{echo_reply,NewLcp,Data} ->
	    S#eppp{lcp=NewLcp}; %% NYI !!
	{protocol_reject,NewLcp,Data} ->
	    S#eppp{lcp=NewLcp}  %% NYI !!
    end.

%% Either we got an Event or we shall call a Continuation

call_lcp(Lcp,Cont) when function(Cont) -> Cont(Lcp);
call_lcp(Lcp,Event) -> eppp_lcp:run(Lcp,Event).

%% Act on the return tag from LCP
%% !! NYI !!

lcp_tag(S,tlu,Lcp) ->              %% LCP is up !!
    imsg("~w(~w): LCP is UP !!~n",[?MODULE,?LINE]),
    S#eppp{lcp=Lcp};
lcp_tag(S,tld,Lcp) ->              %% LCP is down !!
    S#eppp{lcp=Lcp};
lcp_tag(S,tlf,Lcp) ->              %% LCP is finished !!
    S#eppp{lcp=Lcp};
lcp_tag(S,tls,Lcp) ->              %% LCP is started !!
    S#eppp{lcp=Lcp};
lcp_tag(S,_,Lcp) ->                %% LCP is working...
    S#eppp{lcp=Lcp}.

%%ip(S,Buf) ->    nyi.
%%
%%ncp(S,[Code, Id, Hi, Lo | Tail]) ->
%%    Len = i16(Hi, Lo),
%%    Opts = decode_ncp_opts(Tail),
%%    {decode_lcp_code(Code), {id, Id}, Opts}.
%%
%%decode_ncp_opts([2, Len | Tail]) ->
%%    {Data, Rest} = split_list(Len-2, Tail),
%%    [{ip_compression, Data} | decode_ncp_opts(Tail)];
%%decode_ncp_opts([3,Len|Tail]) ->
%%    {Ip, Rest} = split_list(Len-2, Tail),
%%    [{ipaddr, Ip} | decode_ncp_opts(Tail)].
%%
%%pap(S,Buf) ->    nyi.

%% -----------------------------------------
%% HDLC encode a sequence of bytes according 
%% to the type of packet.

encode(lcp,Bytes) ->
    eppp_hdlc:encode([?PPP_ALLSTATION, ?PPP_UI, 16#C0, 16#21 | Bytes]);
encode(ip,Bytes) ->
    eppp_hdlc:encode([?PPP_ALLSTATION, ?PPP_UI,0, 16#21 | Bytes]);
encode(pap,Bytes) ->
    eppp_hdlc:encode([?PPP_ALLSTATION, ?PPP_UI, 16#C0, 16#23 | Bytes]);
encode(ncp,Bytes) ->
    [?PPP_ALLSTATION, ?PPP_UI, 16#80, 16#21 | Bytes],
    nyi. %% !!!!!!!!!!!??????????

i16(Int) when binary(Int) ->
    i16(binary_to_list(Int));
i16(Int)  when integer(Int) -> [
				(Int bsr  8) band 255,
				Int band 255];
i16([X3,X4]) ->
    (X3 bsl 8) bor X4.
i16(X3,X4) ->
    (X3 bsl 8) bor X4.

i32(Int) when binary(Int) ->
    i32(binary_to_list(Int));
i32(Int)  when integer(Int) -> [(Int bsr 24) band 255,
				(Int bsr 16) band 255,
				(Int bsr  8) band 255,
				Int band 255];
i32([X1,X2,X3,X4]) ->
    (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4.
i32(X1,X2,X3,X4) ->
    (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4.
    
%% Create a (nice) string containing the current time.

ts() ->
    {HH,MM,SS} = time(),
    i2l(HH) ++ ":" ++ i2l(MM) ++ ":" ++ i2l(SS).

%% Misc. stuff

i2l(I) -> i2l_0(integer_to_list(I)).
i2l_0([X]) -> [$0,X];
i2l_0(X)   -> X.

dec2hex([X|Xs]) ->
    [hex(X bsr 4), hex(X) | dec2hex(Xs)];
dec2hex([]) -> [].

hex(X) ->
    X4 = (X band 16#f),
    if X4 < 10 -> X4 + $0;
       true -> (X4-10) + $a
    end.

sleep(T) -> receive after T -> true end.

getopt(Dict,Key) ->
    {ok,Value} = eppp_conf:read_const(Dict,Key),
    Value.
