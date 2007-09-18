-module(eppp_lcp).
-author('tobbe@cslab.ericsson.se').
%%% -------------------------------------------------------------------
%%% Created : 20 Oct 1998 by tobbe@cslab.ericsson.se
%%% Function: 
%%%
%%%    I have re-constructed the original PPP/LCP implementation
%%%    and added code that were missing (in particular for dealing
%%%    with negotiation and analysis of various odd cases).
%%%    I have removed anything not specific for LCP, so now the 
%%%    LCP protocol is located as a separate "plug-in" module.
%%%    This means that it can be restarted at any time.
%%%
%%% External Interface:
%%%
%%%    init(Send,StartTimer,StopTimer)  => LCP-record
%%%
%%%    run(LCP-record,LCP-packet)       => {Result, LCP-record} |
%%%                                        {Result2, LCP-record, Data} 
%%%
%%%      where:
%%%
%%%        Send              => Send(Proto,Bytes)
%%%        StartTimer        => StartTimer(Timeout,TagList)
%%%        StopTimer         => StopTimer(Ticket)
%%%
%%%        LCP-record        => <internal LCP record>
%%%        LCP-packet        => Bytes | up | down | open | 
%%%                             close | {timeout,TimeoutTag}
%%%        TagList           => <a tag to identify a timeout event>
%%%        Bytes             => <a list of bytes>
%%%        TimeoutTag        => <a tag as given to eppp:start_timer/3>
%%%        Result            => this_layer_up | this_layer_down |
%%%                             this_layer_finished | this_layer_started |
%%%                             State
%%%        ThisLayerUp       => tlu
%%%        ThisLayerDown     => tld
%%%        ThisLayerFinished => tlf
%%%        ThisLayerStarted  => tls
%%%        State             =>
%%%        RunResult2        => echo_reply | protocol_reject
%%%        Data              => EchoReplyData | ProtocolRejectData
%%%        EchoReplyData     => {Id, Data}
%%%        ProtocolRejectData=> Data
%%%        Id                => <the identifier field of the LCP header>
%%%        Data              => <zero or more octets of uninterpreted data>
%%%
%%% -------------------------------------------------------------------
-vc('$Id: eppp_lcp.erl,v 1.1.1.1 1999/09/05 09:54:15 tobbe Exp $ ').
-export([init/3,run/2,send_protocol_reject/3,send_echo_request/2,version/0]).
-export([set_mru/2,set_pfc/2,set_acfc/2,set_auth/2,set_auth_data/2,set_log/2]).
-export([get_mru/1,get_pfc/1,get_acfc/1,get_auth/1,get_auth_data/1,
	 get_auth_proto/1,all_opts/0,nyi_opts/0]).

-import(eppp,[i16/1,i16/2,i32/1,i32/4]). 
-import(eppp_log,[emsg/2,w1/3,w2/3,w3/3]).

version() ->
    [_,Rev|_] = string:tokens("$Revision: 1.1.1.1 $ "," "),Rev.

%% -------------------
%% Type of LCP packets

-define(CONFIGURE_REQUEST, 1).
-define(CONFIGURE_ACK, 2).
-define(CONFIGURE_NAK, 3).
-define(CONFIGURE_REJECT, 4).
-define(TERMINATE_REQUEST, 5).
-define(TERMINATE_ACK, 6).
-define(CODE_REJECT, 7).
-define(PROTOCOL_REJECT, 8).
-define(ECHO_REQUEST, 9).
-define(ECHO_REPLY, 10).
-define(DISCARD_REQUEST, 11).

%% -------------------------
%% LCP configuration Options

-define(LCP_MRU,         1).
-define(LCP_ACCM,        2).  %% Async-Control-Character-Map
-define(LCP_AUTH,        3).
-define(LCP_QPROTO,      4).  %% Quality-Protocol
-define(LCP_MAGIC,       5).
-define(LCP_RESERVED,    6).
-define(LCP_PFC,         7).
-define(LCP_ACFC,        8).
-define(LCP_FCS,         9).  %% FCS-Alternatives
-define(LCP_SDP,        10).  %% Self-Describing-Pad
-define(LCP_NM,         11).  %% Numbered-Mode
-define(LCP_MLINK,      12).  %% Multi-Link-Procedure
-define(LCP_CBACK,      13).  %% Callback
-define(LCP_CTIME,      14).  %% Connect-Time
-define(LCP_CFRAMES,    15).  %% Compound-Frames
-define(LCP_NDE,        16).  %% Nominal-Data-Encapsulation
-define(LCP_MMRRU,      17).  %% Multilink-MRRU
-define(LCP_MSSNHF,     18).  %% Multilink-Short-Sequence-Number-Header-Format
-define(LCP_MED,        19).  %% Multilink-Endpoint-Discriminator
-define(LCP_PROP,       20).  %% Proprietary
-define(LCP_DCEID,      21).  %% DCE-Identifier

%% All the options that we have implemented support for.
all_opts() -> 
    [?LCP_MRU,?LCP_AUTH,?LCP_MAGIC,?LCP_PFC,?LCP_ACFC].

%% Options for which support is not yet implemented.
nyi_opts() ->
    [?LCP_ACCM,?LCP_QPROTO,?LCP_RESERVED,?LCP_FCS,
     ?LCP_SDP,?LCP_NM,?LCP_MLINK,?LCP_CBACK,?LCP_CTIME,
     ?LCP_CFRAMES,?LCP_NDE,?LCP_MMRRU,?LCP_MSSNHF,
     ?LCP_MED,?LCP_PROP,?LCP_DCEID].

%% What we initially will try and negotiate
default_opts() -> 
    [?LCP_MAGIC,?LCP_PFC,?LCP_ACFC].

-define(MAX_TERMINATE,  2).           %% try to send Term-req 2 times

-record(lcp,{
	  acfc = true,                %% address & control field compression
	  auth = [],                  %% valid auth protocols in preferred order
	  auth_data = false,          %% authentication data
	  auth_proto = false,         %% negotiated auth. protocol to be used
	  client = true,              %% act as server if false
	  id = 1,                     %% packet identifier
	  lcp_gen,                    %% the lcp_gen state   
	  log = 1,                    %% log level
	  mru = 1500,                 %% mru option
	  magic = magic(),            %% magic number option
	  pfc = true,                 %% protocol field compression
	  restart_cnt = 0,            %% restart counter
	  seen_ack = false,           %% have got valid Ack/Nak/Rej to Req
          send,                       %% The Send/2 function
	  state = initial,            %% FSM state
	  start_timer,                %% The StartTimer/2 function
	  stop_timer,                 %% The StopTimer/1 function
	  timeout = 3000,             %% restart timeout is 3 seconds
	  timer_ticket = false,       %% handle to current timer
	  upcall_result               %% result from the last upcall made
	  }).

%% ---------------
%% Get LCP options

get_acfc(S)       -> S#lcp.acfc.
get_auth(S)       -> S#lcp.auth.
get_auth_data(S)  -> S#lcp.auth_data.
get_auth_proto(S) -> S#lcp.auth_proto.
get_mru(S)        -> S#lcp.mru.
get_pfc(S)        -> S#lcp.pfc.

%% ---------------
%% Set LCP options

set_acfc(S,true)                      -> S#lcp{acfc=true};
set_acfc(S,false)                     -> S#lcp{acfc=false}.
set_auth(S,Auth)                      -> set_auth_0(S,Auth).
set_auth_data(S,Data) when list(Data) -> S#lcp{auth_data=Data}.
set_mru(S,Mru) when integer(Mru)      -> S#lcp{mru=Mru}.
set_pfc(S,true)                       -> S#lcp{pfc=true};
set_pfc(S,false)                      -> S#lcp{pfc=false}.

%% Want to do a thorough check of the auth parameter
set_auth_0(S,pap)   -> S#lcp{auth=[pap]};
set_auth_0(S,chap)  -> S#lcp{auth=[chap]};
set_auth_0(S,false) -> S#lcp{auth=[]};
set_auth_0(S,Ps) when list(Ps) -> 
    case valid_protos(Ps) of
	true  -> S#lcp{auth=Ps};
	false -> exit(badarg)
    end.

valid_protos([pap|Ps])  -> valid_protos(Ps);
valid_protos([chap|Ps]) -> valid_protos(Ps);
valid_protos([X|Ps])    -> false;
valid_protos([])        -> true.

%% -----------------
%% Set the log level

set_log(S,Level) when integer(Level)  -> S#lcp{log=Level}.

%% -------------------------
%% Initialize the LCP record

init(Send,StartTimer,StopTimer)
when function(Send),function(StartTimer),function(StopTimer) ->
    LcpGen = init_lcp_gen(),
    #lcp{send=Send,start_timer=StartTimer,
	 stop_timer=StopTimer,lcp_gen=LcpGen}.

%% ---------------------------
%% Set up the lcp_gen machine.

init_lcp_gen() ->
    eppp_lcp_gen:init(mk_upcall(),mk_result()).

mk_upcall() -> 
    fun(S,What) when record(S,lcp) ->  do_upcall(S,What) end.

mk_result() -> 
    fun(S) when record(S,lcp) ->  S#lcp.upcall_result end.

%% --------------------------------------------------
%% do_upcall/2 will be called by the lcp_gen machine.

do_upcall(S,{start_timer,Timeout}) ->
    set_upcall_result(S,(S#lcp.start_timer)(Timeout,[lcp_gen]));
do_upcall(S,{stop_timer,Ticket}) ->
    set_upcall_result(S,(S#lcp.stop_timer)(Ticket));
do_upcall(S,{send,Bytes}) ->
    set_upcall_result(S,(S#lcp.send)(lcp,Bytes));
do_upcall(S,{encode,Options}) ->
    set_upcall_result(S,encode_options(Options));
do_upcall(S,{decode,Bytes}) ->
    set_upcall_result(S,decode_options(Bytes));
do_upcall(S,{analyse,Options}) ->
    {S1,Result} = analyse_options(S,Options),
    set_upcall_result(S1,Result);
do_upcall(S,{log,Fstr,Args}) ->
    do_log(S,Fstr,Args),
    set_upcall_result(S,ok).

set_upcall_result(S,Result) -> S#lcp{upcall_result=Result}.

%% ------------------------------------------------
%% Encode the LCP options into a sequence of bytes.

encode_options([{?LCP_MRU,Mru}|Opts]) ->
    [Hi,Lo] = i16(Mru),
    [1,4,Hi,Lo|encode_options(Opts)];
encode_options([{?LCP_AUTH,pap}|Opts]) ->
    %% (RFC-1334)
    [3, 4, 16#c0, 16#23 | encode_options(Opts)];
encode_options([{?LCP_AUTH,chap}|Opts]) ->
    %% CHAP with MD5 (RFC-1994)
    [3, 5, 16#c2, 16#23, 5 | encode_options(Opts)];
encode_options([{?LCP_MAGIC,Magic}|Opts]) ->
    [X1,X2,X3,X4] = i32(Magic),
    [5,6,X1,X2,X3,X4 | encode_options(Opts)];
encode_options([{?LCP_PFC,true}|Opts]) ->
    [7,2 | encode_options(Opts)];
encode_options([{?LCP_PFC,false}|Opts]) ->
    encode_options(Opts);
encode_options([{?LCP_ACFC,true}|Opts]) ->
    [8,2 | encode_options(Opts)];
encode_options([{?LCP_ACFC,false}|Opts]) ->
    encode_options(Opts);
encode_options([{Opt,Len,Data}|Opts]) ->
    %% This is an explicitly specified option, e.g if
    %% we get an unkown option from the peer then we
    %% may end up here encoding it for a NAK reply.
    [Opt,Len|Data] ++ encode_options(Opts);
encode_options([]) ->
    [].

%% --------------------------------------------
%% Decode a sequence of bytes into LCP options.

decode_options(Options) ->
    case catch decode_options(Options,length(Options)) of
	{'EXIT', Reason} -> {error,Reason};
	Opts -> {ok,Opts}
    end.

decode_options(_, 0) -> 
    [];
decode_options([?LCP_MRU,4,Hi,Lo|Tail],Sz) ->
    [{?LCP_MRU, i16(Hi,Lo)}|decode_options(Tail,Sz - 4)];
decode_options([?LCP_AUTH,Len|Tail],Sz) ->
    %% Neither pap nor chap makes use of any 'Data'.
    %% This may have to be taken care of for other
    %% authentication protocols though...
    {[A1,A2|_Data],Rest} = split_list(Len - 2, Tail),
    [{?LCP_AUTH,auth(A1,A2)}|decode_options(Rest,Sz - Len)];
decode_options([?LCP_MAGIC,Len,X1,X2,X3,X4|Tail], Sz) ->
    [{?LCP_MAGIC,i32(X1,X2,X3,X4)}|decode_options(Tail,Sz - Len)];
decode_options([?LCP_PFC,2|Tail],Sz) ->
    [{?LCP_PFC, true}|decode_options(Tail,Sz - 2)];
decode_options([?LCP_ACFC,2|Tail],Sz) ->
    [{?LCP_ACFC,true}|decode_options(Tail,Sz - 2)];
decode_options([Option,Len|Tail0],Sz) -> 
    %% Currently no support for this option.
    %% Discard any data but keep the option for
    %% negotiation purposes.
    {Packet,Tail} = split_list(Len - 2, Tail0),
    [{Option,Len,Packet}|decode_options(Tail,Sz - Len)].

split_list(Pos,L)         -> split_list(Pos,L,[]).
split_list(0,L,Ack)       -> {lists:reverse(Ack),L};
split_list(Pos,[H|T],Ack) -> split_list(Pos - 1,T,[H|Ack]);
split_list(Pos,[],Ack)    -> {[],[]}.

auth(16#c0,16#23) -> pap;
auth(16#c2,16#23) -> chap;
auth(_,_)         -> unknown.

%% -------------------------------------------------
%% Analyse the options we have received from the
%% peer and adopt as many of them as possible.
%% For those options we can't accept, produce a
%% list of those options not to be acknowledged.
%% Also, we have to check if there are any particular
%% options we want to negotiate which haven't been
%% covered by the peer request.

analyse_options(S,Opts) ->
    analyse_options(S,Opts,[]).

analyse_options(S,[{?LCP_MRU,MRU}|Opts],Nak) when MRU > 0 ->
    analyse_options(S#lcp{mru=MRU},Opts,Nak);
analyse_options(S,[{?LCP_MRU,_}|Opts],Nak) ->                    
    analyse_options(S,Opts,[{?LCP_MRU,S#lcp.mru}|Nak]);
analyse_options(S,[{?LCP_AUTH,Proto}|Opts],Nak) ->     
    Auth  = S#lcp.auth,
    case lists:member(Proto,Auth) of
	true  -> analyse_options(S#lcp{auth_proto=Proto},Opts,Nak);
	false -> analyse_options(S,Opts,[{?LCP_AUTH,Proto}|Nak])                 
    end;
analyse_options(S,[{?LCP_MAGIC,Magic}|Opts],Nak) when S#lcp.magic=/=Magic ->
    %% Magic differ, no loop-back !
    analyse_options(S,Opts,Nak);
analyse_options(S,[{?LCP_MAGIC,Magic}|Opts],Nak) when S#lcp.magic==Magic ->
    %% Magic equal, possible loop-back !!
    NewMagic = magic(),
    analyse_options(S#lcp{magic=NewMagic},Opts,[{?LCP_MAGIC,NewMagic}|Nak]);
analyse_options(S,[{pfc,true}|Opts],Nak) ->
    analyse_options(S#lcp{pfc=true},Opts,Nak);
analyse_options(S,[{acfc,true}|Opts],Nak) ->
    analyse_options(S#lcp{acfc=true},Opts,Nak);
analyse_options(S,[{Opt,Len,Packet}|Opts],Nak) ->
    %% This is either an unsupported option or
    %% a supported option with wrong length field.
    analyse_options(S,Opts,[{Opt,Len,Packet}|Nak]);
analyse_options(S,[],[]) ->
    %% Every option is to be acknowledged !!
    set_upcall_result(S,additional_options(S));
analyse_options(S,[],Nak) ->
    %% Some options is not to be acknowledged !!
    %% NB: have to keep the order of the options.
    set_upcall_result(S,additional_options(S,reject_or_nak(S,Nak))).

additional_options(S) -> 
    %% <<< TBD >>> A more thorough analyse here !!
    %% We should check if we there are more options to
    %% be negotiated, which hasn't been covered so far.
    {ok,[]}.

additional_options(S,NakResult) ->
    %% Some of the options proposed by the peer has
    %% to be re-negotiated. So we won't try and check
    %% if we should negotiate more options which hasn't
    %% been covered so far.
    NakResult.
    

%% NB: It is a bit unclear in the RFC (1661) what
%% should happend if we have both options that should
%% be rejected and options that should be Nak'ed.
%%
%% I assume here that a reject takes precedence
%% over a Nak, so here we need to analyse if a 
%% configure reject should be sent. This may 
%% happend if: 
%%
%%   a) We didn't recognise the option at all.
%%   b) The options is not acceptable for negotiation.

reject_or_nak(S,Opts) -> 
    reject_or_nak(S,Opts,[]).

reject_or_nak(S,[{?LCP_AUTH,Proto}|Opts],Acc) when S#lcp.auth==[] ->
    %% We don't even want to negotiate authentication !!
    reject(S,Opts,[{?LCP_AUTH,Proto}]);
reject_or_nak(S,[{?LCP_AUTH,_}|Opts],Acc) when S#lcp.auth=/=[] ->
    PreferedProto = hd(S#lcp.auth),
    reject_or_nak(S,Opts,[{?LCP_AUTH,PreferedProto}|Acc]);
reject_or_nak(S,[{Opt,Len,Packet}|Opts],Acc) ->
    reject(S,Opts,[{Opt,Len,Packet}]);
reject_or_nak(S,[H|T],Acc) ->
    reject_or_nak(S,T,[H|Acc]);
reject_or_nak(_,[],Acc) ->
    {nak,Acc}.

reject(S,[{?LCP_AUTH,Proto}|Opts],Acc) when S#lcp.auth==[] ->
    %% We don't even want to negotiate authentication !!
    reject(S,Opts,[{?LCP_AUTH,Proto}]);
reject(S,[{Opt,Len,Packet}|Opts],Acc) ->
    reject(S,Opts,[{Opt,Len,Packet}|Acc]);
reject(S,[_|T],Acc) ->
    reject(S,T,Acc);
reject(_,[],Acc) ->
    {reject,Acc}.

%% ---------------------------
%% Log information at Level 2

do_log(S,Fstr,Args) ->
    F1 = fun() -> "LCP(~s): " ++ Fstr end,
    F2 = fun() -> [eppp:ts()|Args] end,
    w2(S#lcp.log,F1,F2).



%% -------------------------------------------------------
%% We have the following possible LCP states
%% during the link establishment phase.
%%
%% initial	    Down, hasn't been opened 
%% starting         Down, been opened 
%% closed	    Up, hasn't been opened 
%% stopped	    Open, waiting for down event
%% closing	    Terminating the connection, not open 
%% stopping         Terminating, but open 
%% reqsent	    We've sent a Config Request 
%% ackrcvd	    We've received a Config Ack 
%% acksent	    We've sent a Config Ack 
%% opened	    Connection available 
%% -------------------------------------------------------

run(S,Msg) when record(S,lcp) ->
    analyse_msg(S,Msg).

ok(S) ->
    {ok,S}.

analyse_msg(S,Msg) ->
    case Msg of
	Bytes when list(Bytes) ->
	    decode(S,Bytes);
	up ->
	    log_se(S,up),
	    {_,S1,G} = eppp_lcp_gen:up(S,S#lcp.lcp_gen),
	    S1#lcp{lcp_gen=G};
	down ->
	    log_se(S,down),
	    {_,S1,G} = eppp_lcp_gen:down(S,S#lcp.lcp_gen),
	    S1#lcp{lcp_gen=G};
	open ->
	    log_se(S,open),
	    {_,S1,G} = eppp_lcp_gen:open(S,S#lcp.lcp_gen),
	    S1#lcp{lcp_gen=G};
	close ->
	    log_se(S,close),
	    {_,S1,G} = eppp_lcp_gen:close(S,S#lcp.lcp_gen),
	    S1#lcp{lcp_gen=G};
	{timeout,[lcp_gen|TagList]} ->
	    {_,S1,G} = eppp_lcp_gen:timeout(S,S#lcp.lcp_gen,TagList),
	    S1#lcp{lcp_gen=G}
    end.


%% -----------------
%% Decode LCP packet
	    
decode(S,[Code|Tail]) when (Code>=?CONFIGURE_REQUEST),(Code=<?CODE_REJECT)->
    {_,S1,G} = eppp_lcp_gen:dispatch_event(S,S#lcp.lcp_gen,Code,Tail),
    S1#lcp{lcp_gen=G};
decode(S,[?PROTOCOL_REJECT,Id,_,_,Hi,Lo|Tail]) when S#lcp.state==opened ->
    %% If we are in state opened, then a protocol_reject
    %% of LCP is bad, otherwise it is good.
    case {Hi,Lo} of
	{16#C0,16#21} -> %% A LCP packet !!
	    log_se(S,'-proto-rej'),
	    rxj_bad(opened,S,Id,[],{protocol_reject,{{Hi,Lo},Tail}});
	_ -> 
	    log_se(S,'+proto-rej'),
	    rxj_good(opened,S,Id,[],{protocol_reject,{{Hi,Lo},Tail}})
    end;
decode(S,[?PROTOCOL_REJECT,Id,_,_,Hi,Lo|Tail]) when S#lcp.state=/=opened ->
    %% If we are not in state opened, then silently 
    %% discard all protocol_reject packets.
    ok(S);
decode(S,[?ECHO_REQUEST,Id,Hi,Lo,_,_,_,_|Tail]) ->
    log_se(S,'echo-req'),
    Len = i16(Hi,Lo),
    Data = truncate(Tail,Len),
    rxr(S#lcp.state,S,Id,Data,echo_request);
decode(S,[?ECHO_REPLY,Id,Hi,Lo,_,_,_,_|Tail]) ->
    log_se(S,'echo-rpl'),
    Len = i16(Hi,Lo),
    Data = truncate(Tail,Len),
    rxr(S#lcp.state,S,Id,Data,echo_reply);
decode(S,[?DISCARD_REQUEST,Id,Hi,Lo,_,_,_,_|Tail]) ->
    log_se(S,'disc-req'),
    Len = i16(Hi,Lo),
    Data = truncate(Tail,Len),
    rxr(S#lcp.state,S,Id,Data,discard_request);
decode(S,[Code,Id,Hi,Lo|Tail]) ->
    %% Code not recognized, return a CODE_REJECT !!
    log_se(S,Code),
    NewId = bump_id(S),
    RejPack = truncate([Code,Id,Hi,Lo|Tail],S#lcp.mru),
    S2 = send_code_reject(S#lcp{id=NewId},RejPack),
    ok(S2).

%% -----------------
%% Encode LCP packet

encode(S,Type,Id,Opts) ->
    OptsBytes = encode_options(Opts),
    [Type, Id | i16(4 + length(OptsBytes)) ++ OptsBytes].


%% ----------------------------------------
%% Part of the the LCP FSM
%% ----------------------------------------

%% Received: Proto.Rej

rxj_good(ackrcvd, S, Id, Opts, {Class,Data}) -> 
    ok(S#lcp{state=reqsent});
rxj_good(opened, S, Id, Opts, {protocol_reject,Data}) -> 
    protocol_reject(S,Data);
rxj_good(State, S, Id, Opts, {Class,Data}) -> 
    ok(S).

rxj_bad(closed, S, Id, Opts, {Class,Data})  ->
    emsg("LCP: bad ~w, data=~w~n",[Class,Data]),
    this_layer_finished(S);
rxj_bad(stopped, S, Id, Opts, {Class,Data})  ->
    emsg("LCP: bad ~w, data=~w~n",[Class,Data]),
    this_layer_finished(S);
rxj_bad(closing, S, Id, Opts, {Class,Data})  ->
    emsg("LCP: bad ~w, data=~w~n",[Class,Data]),
    this_layer_finished(S#lcp{state=closed});
rxj_bad(stopping, S, Id, Opts, {Class,Data})  ->
    emsg("LCP: bad ~w, data=~w~n",[Class,Data]),
    this_layer_finished(S#lcp{state=stopped});
rxj_bad(reqsent, S, Id, Opts, {Class,Data})  ->
    emsg("LCP: bad ~w, data=~w~n",[Class,Data]),
    this_layer_finished(S#lcp{state=stopped});
rxj_bad(ackrcvd, S, Id, Opts, {Class,Data})  ->
    emsg("LCP: bad ~w, data=~w~n",[Class,Data]),
    this_layer_finished(S#lcp{state=stopped});
rxj_bad(acksent, S, Id, Opts, {Class,Data})  ->
    emsg("LCP: bad ~w, data=~w~n",[Class,Data]),
    this_layer_finished(S#lcp{state=stopped});
rxj_bad(opened, S, Id, Opts, {Class,Data}) -> 
    emsg("LCP: bad ~w, data=~w~n",[Class,Data]),
    {_,S1,G} = eppp_lcp_gen:terminate_request(S,S#lcp.lcp_gen),
    this_layer_down(S1#lcp{state=stopping,lcp_gen=G}).

%% Received: Echo.Req , Echo.Rep , Disc.Req

rxr(opened,S,Id,Data, echo_request)  ->
    send_echo_reply(S,Id,Data),
    ok(S);
rxr(opened,S,Id,Data, echo_reply)  ->
    echo_reply(S,Id,Data);
rxr(opened,S,Id,Data, discard_request)  ->
    ok(S);
rxr(State,S,_,_,Class)  ->
    emsg("LCP: got event ~w ,in state ~w~n",[Class,State]),
    ok(S).

%% -----------
%% FSM Actions
%% -----------


this_layer_up(S) ->                          %% tlu
    w1(S#lcp.log,"LCP: this layer up~n",[]),
    {tlu,S}.

this_layer_down(S) ->                        %% tld
    w1(S#lcp.log,"LCP: this layer down~n",[]),
    {tld,S}. 

this_layer_finished(S) ->                    %% tlf
    w1(S#lcp.log,"LCP: this layer finished~n",[]),
    {tlf,S}.
    
this_layer_started(S) ->                     %% tls
    w1(S#lcp.log,"LCP: this layer stated~n",[]),
    {tls,S}.

send_code_reject(S, RejPack) ->              %% scj
    Id = S#lcp.id,
    Bytes = [?CODE_REJECT, Id | i16(4 + length(RejPack)) ++ RejPack],
    (S#lcp.send)(lcp,Bytes),
    S.

%% This function must be called by the PPP engine since
%% it is there the unknown protocol field shows up.
%% NB: This message is only sent when in state: OPENED
send_protocol_reject(S,RejProto,RejPack) when S#lcp.state == opened ->   
    Id = bump_id(S),
    Len = i16(6 + length(RejPack)),
    Rp = i16(RejProto),
    Bytes = [?PROTOCOL_REJECT, Id | Len ++ Rp  ++ RejPack],
    (S#lcp.send)(lcp,Bytes),
    {ok,S#lcp{id=Id}};
send_protocol_reject(S,_,_) ->
    {ok,S}.

protocol_reject(S,Data) ->
    {protocol_reject,S,Data}.

%% This function must be called by the PPP engine.
send_echo_request(S,Data) ->
    Id = bump_id(S),
    Len = i16(length(Data)+8),
    Magic = i32(S#lcp.magic),
    Bytes = [?ECHO_REQUEST, Id | Len ++ Magic ++ Data],
    (S#lcp.send)(lcp,Bytes),
    S#lcp{id=Id}.

send_echo_reply(S,Id,Data) ->
    Bytes = [?ECHO_REPLY, Id | i16(length(Data)+4) ++ Data],
    (S#lcp.send)(lcp,Bytes),
    S.

%% We have received an ECHO_REPLY !!
echo_reply(S,Id,Data) ->      
    {echo_reply,S,{Id,Data}}.

%% --------------
%% Misc. routines

magic() -> 
    (466332 + element(3, now())) band 16#ffffffff.

%% The identifier field is one byte

bump_id(S) -> (S#lcp.id + 1) rem 256.

truncate([],_)     -> [];
truncate(_,0)      -> [];
truncate([B|Bs],N) -> [B|truncate(Bs,N-1)].

%% Log info, at Level 2, about the current State and Event

log_se(S,Event) ->
    F = fun() -> [eppp:ts(),Event,S#lcp.state] end,
    w2(S#lcp.log,"LCP(~s): event(~w) state(~w)~n",F).



