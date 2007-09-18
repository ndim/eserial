-module(eppp_lcp_gen).
-author('tobbe@cslab.ericsson.se').
%%% -------------------------------------------------------------------
%%% Created : 20 Oct 1998 by tobbe@cslab.ericsson.se
%%% Function: This is the general LCP machine (Config.Req - Code.Rej).
%%%           It is used by LCP as well as IPCP, CCP, ...
%%% -------------------------------------------------------------------
-vc('$Id: eppp_lcp_gen.erl,v 1.1.1.1 1999/09/05 09:54:16 tobbe Exp $ ').
-export([version/0,init/2,dispatch_event/4,open/2,close/2,up/2,down/2]).
-export([timeout/3,terminate_request/2]).

-import(eppp,[i16/1,i16/2]). 

version() ->
    [_,Rev|_] = string:tokens("$Revision: 1.1.1.1 $ "," "),Rev.

%% -------------------
%% Type of LCP packets

-define(CONFIGURE_REQUEST,   1).
-define(CONFIGURE_ACK,       2).
-define(CONFIGURE_NAK,       3).
-define(CONFIGURE_REJECT,    4).
-define(TERMINATE_REQUEST,   5).
-define(TERMINATE_ACK,       6).
-define(CODE_REJECT,         7).

%% --------------------------------------
%% Default values for the restart counter

-define(MAX_TERMINATE,  2).           %% try to send Term-req 2 times
-define(MAX_CONFIGURE, 10).           %% try to send Conf-req 10 times
-define(MAX_FAILURE,    5).           %% try to send Conf-nak 5 times

%% ---------------------
%% Internal state record

-record(lcp_gen,{
	  id = 1,                     %% Packet identifier
	  restart_cnt = 0,            %% Restart counter
	  state = initial,            %% FSM state
	  timeout = 3000,             %% Restart timeout is 3 seconds
	  timer_ticket = false,       %% Handle to current timer
	  upcall,                     %% Call back routine
	  result                      %% Call back routine
	  }).

%% -------------------
%% EXPORTED FUNCTIONS
%% -------------------

%% ------------------------------------------------
%% Initialize and return a lcp_gen data structure.

init(Upcall,Result) when function(Upcall),function(Result) ->
    #lcp_gen{upcall=Upcall,result=Result}.

%% --------------------------------------------------
%% A LCP event has occured. Take appropriate action.

dispatch_event(U,S,Code,[Id,Hi,Lo|Tail]) when record(S,lcp_gen), 
					      (Code >= ?CONFIGURE_REQUEST),
					      (Code =< ?TERMINATE_ACK) ->
    Len = i16(Hi,Lo),
    U1 = (S#lcp_gen.upcall)(U,{decode,Tail}),
    case (S#lcp_gen.result)(U1) of
	{ok,Opts} -> 
	    dispatch(U1,S,Code,Id,Opts);
	{error,Reason} ->
	    Msg = {log,"<dispatch_event> decode failed, reason: ~p~n",[Reason]},
	    (S#lcp_gen.upcall)(U,Msg),
	    {U1,S}
    end;
dispatch_event(U,S,?CODE_REJECT,[Id,_,_,Code|Tail]) ->
    %% A code_reject of a 'basic' LCP packet is 'bad'.
    %% A code_reject of an 'extended' LCP packet is 'good'.
    if (Code >= ?CONFIGURE_REQUEST),(Code =< ?CODE_REJECT) ->
	    log_se(U,S,S#lcp_gen.state,"-code-rej"),
	    rxj_bad(S#lcp_gen.state,U,S);
       true ->
	    log_se(U,S,S#lcp_gen.state,"+code-rej"),
	    rxj_good(S#lcp_gen.state,U,S)
    end.

dispatch(U,S,?CONFIGURE_REQUEST,I,O) ->
    State = S#lcp_gen.state,
    configure_request(State,log_se(U,S,State,"conf-req"),S,I,O);
dispatch(U,S,?CONFIGURE_ACK,I,O) -> 
    State = S#lcp_gen.state,
    configure_ack(State,log_se(U,S,State,"conf-ack"),S,I,O);
dispatch(U,S,?CONFIGURE_NAK,I,O) -> 
    State = S#lcp_gen.state,
    configure_nak(State,log_se(U,S,State,"conf-nak"),S,I,O);
dispatch(U,S,?CONFIGURE_REJECT,I,O) -> 
    State = S#lcp_gen.state,
    configure_reject(State,log_se(U,S,State,"conf-rej"),S,I,O);
dispatch(U,S,?TERMINATE_REQUEST,I,O) -> 
    State = S#lcp_gen.state,
    terminate_request(State,log_se(U,S,State,"term-req"),S,I,O);
dispatch(U,S,?TERMINATE_ACK,I,O) -> 
    State = S#lcp_gen.state,
    terminate_ack(State,log_se(U,S,State,"term-ack"),S,I,O).


%% -------------------------------------------------------
%% The Restart timer has expired. Take appropriate action.
%% NB: The TagList is not important for us since we don't
%% have any other 'plug-in' module 'below' us. So really,
%% the TagList *should* be an empty list here. 

timeout(U,S,TagList) when S#lcp_gen.restart_cnt=<0 -> 
    log_se(U,S,S#lcp_gen.state,"-timeout"),
    nok_timeout(U,S#lcp_gen.state,S);
timeout(U,S,TagList) when S#lcp_gen.restart_cnt>0 -> 
    log_se(U,S,S#lcp_gen.state,"+timeout"),
    ok_timeout(U,S#lcp_gen.state,S).


%% -------------------
%% GENERAL LCP MACHINE
%% -------------------

%% ---------------------------------------------
%% The UP event occurs when a lower layer 
%% indicates that it is ready to carry packets.

up(U,S) -> up(S#lcp_gen.state,U,S).

up(initial,U,S) -> 
    ok(U,S#lcp_gen{state=closed});
up(starting,U,S) ->
    U1 = (S#lcp_gen.upcall)(U,options),
    {_,Opts} = (S#lcp_gen.result)(U1),
    S1 = init_restart_count(S,?MAX_CONFIGURE),
    {U2,S2} = send_configure_request(U1,S1,Opts),
    ok(U2,S2#lcp_gen{state=reqsent});
up(State,U,S) ->
    U1 = illegal_event(U,S,up,State),
    ok(U1,S).

%% ---------------------------------------------------
%% The DOWN event occurs when a lower layer indicates
%% that it is no longer ready to carry packets.

down(U,S) -> down(S#lcp_gen.state,U,S).

down(initial,U,S) ->
    U1 = illegal_event(U,S,down,initial),
    ok(U1,S);
down(starting,U,S) ->
    U1 = illegal_event(U,S,down,starting),
    ok(U1,S);
down(closed,U,S) ->
    ok(U,S#lcp_gen{state=initial});
down(stopped,U,S) ->
    this_layer_started(U,S#lcp_gen{state=starting});
down(closing,U,S) ->
    ok(U,S#lcp_gen{state=initial});
down(opened,U,S) ->
    this_layer_down(U,S#lcp_gen{state=starting});
down(_,U,S) ->
    ok(U,S#lcp_gen{state=starting}).

%% -----------------------------------------------------------
%% The OPEN event indicates that the link is administratively
%% available for traffic; that is, the network administrator
%% (human or program) has indicated that the link is allowed
%% to be Opened.

open(U,S) -> open(S#lcp_gen.state,U,S).

open(initial,U,S) ->
    this_layer_started(U,S#lcp_gen{state=starting});
open(starting,U,S) ->
    ok(U,S);
open(closed,U,S) ->
    U1 = (S#lcp_gen.upcall)(U,options),
    {_,Opts} = (S#lcp_gen.result)(U1),
    S1 = init_restart_count(S,?MAX_CONFIGURE),
    {U2,S2} = send_configure_request(U1,S1,Opts),
    ok(U2,S1#lcp_gen{state=reqsent});
open(stopped,U,S) ->
    ok(U,S);
open(closing,U,S) ->
    ok(U,S#lcp_gen{state=stopping});
open(_,U,S) ->
    ok(U,S).

%% -------------------------------------------
%% The CLOSE event indicates that the link is 
%% not available for traffic

close(U,S) -> close(S#lcp_gen.state,U,S).

close(initial,U,S) ->
    ok(U,S);
close(starting,U,S) ->
    this_layer_finished(U,S#lcp_gen{state=initial});
close(closed,U,S) ->
    ok(U,S);
close(stopped,U,S) ->
    ok(U,S#lcp_gen{state=closed});
close(closing,U,S) ->
    ok(U,S);
close(stopping,U,S) ->
    ok(U,S#lcp_gen{state=closing});
close(opened,U,S) ->
    {U1,S1} = send_terminate_request(U,init_restart_count(S,?MAX_TERMINATE)),
    this_layer_down(U1,S1#lcp_gen{state=closing});
close(State,U,S) ->
    {U1,S1} = send_terminate_request(U,init_restart_count(S,?MAX_TERMINATE)),
    ok(U1,S1#lcp_gen{state=closing}).

%%---------------------------------------------------------
%% The TO+ (ok_timeout) event indicates that the restart
%% counter is still greater than zero, which triggers the
%% corresponding Configure/Terminate -Request packet to be
%% retransmitted.

ok_timeout(closing,U,S) ->
    ok(send_terminate_request(U,S));
ok_timeout(stopping,U,S) ->
    ok(send_terminate_request(U,S));
ok_timeout(reqsent,U,S) ->
    U1 = (S#lcp_gen.upcall)(U,options),
    {_,Opts} = (S#lcp_gen.result)(U1),
    ok(send_configure_request(U1,S,Opts));
ok_timeout(ackrcvd,U,S) ->
    U1 = (S#lcp_gen.upcall)(U,options),
    {_,Opts} = (S#lcp_gen.result)(U1),
    {U2,S1} = send_configure_request(U1,S,Opts),
    ok(U2,S1#lcp_gen{state=reqsent});
ok_timeout(acksent,U,S) ->
    U1 = (S#lcp_gen.upcall)(U,options),
    {_,Opts} = (S#lcp_gen.result)(U1),
    ok(send_configure_request(U1,S,Opts));
ok_timeout(State,U,S) ->
    illegal_event(U,S,"+timeout",State),
    ok(U,S).

%%---------------------------------------------------------
%% The TO- (nok_timeout) event indicates that the restart
%% counter is not greater than zero, and no more packets
%% need to be retransmitted.

nok_timeout(closing,U,S) ->
    this_layer_finished(U,S#lcp_gen{state=closed});
nok_timeout(stopping,U,S) ->
    this_layer_finished(U,S#lcp_gen{state=stopped});
nok_timeout(reqsent,U,S) ->
    this_layer_finished(U,S#lcp_gen{state=stopped});
nok_timeout(ackrcvd,U,S) ->
    this_layer_finished(U,S#lcp_gen{state=stopped});
nok_timeout(acksent,U,S) ->
    this_layer_finished(U,S#lcp_gen{state=stopped});
nok_timeout(State,U,S) ->
    illegal_event(U,S,"-timeout",State),
    ok(U,S).

%% -------------------------------------------------------------
%% A Configure-Request (CR) packet is received from the peer.
%% If the CR is acceptable it triggers the transmission of a
%% corresponding Configure-Ack. If the CR is not acceptable
%% it triggers the transmission of a corresponding Configure-Nak
%% or Configure-Reject

configure_request(closed,U,S,Id,Opts) -> 
    ok(send_terminate_ack(U,S,Id,Opts));
configure_request(stopped,U,S,Id,Opts) ->
    S1 = init_restart_count(bump_id(S),?MAX_CONFIGURE),
    U1 = (S1#lcp_gen.upcall)(U,{analyse,Opts}),
    case (S1#lcp_gen.result)(U1) of
	{ok,NewOpts} ->
	    {U2,S2} = do_scr(U1,S1,NewOpts), 
	    {U3,S3} = send_configure_ack(U2,S2,Id,Opts),
	    ok(U3,S3#lcp_gen{state=acksent});
	{reject,NewOpts,RejOpts} ->
	    {U2,S2} = do_scr(U1,S1,NewOpts), 
	    {U3,S3} = send_configure_reject(U2,S2,Id,RejOpts),
	    ok(U3,S3#lcp_gen{state=reqsent});
	{nak,NewOpts,NakOpts} ->
	    {U2,S2} = do_scr(U1,S1,NewOpts), 
	    {U3,S3} = send_configure_nak(U2,S2,Id,NakOpts),
	    ok(U3,S3#lcp_gen{state=reqsent})
    end;
configure_request(closing,U,S,Id,Opts) ->
    ok(U,S);
configure_request(stopping,U,S,Id,Opts) ->
    ok(U,S);
configure_request(reqsent,U,S,Id,Opts) ->
    U1 = (S#lcp_gen.upcall)(U,{analyse,Opts}),
    case (S#lcp_gen.result)(U1) of
	{ok,_} ->
	    {U2,S1} = send_configure_ack(U1,S,Id,Opts),
	    ok(U2,S1#lcp_gen{state=acksent});
	{reject,NewOpts,RejOpts} ->
	    {U2,S1} = send_configure_reject(U1,S,Id,RejOpts),
	    ok(U2,S1);
	{nak,NewOpts,NakOpts} ->
	    {U2,S1} = send_configure_nak(U1,S,Id,NakOpts),
	    ok(U2,S1)
    end;
configure_request(ackrcvd,U,S,Id,Opts) ->
    U1 = (S#lcp_gen.upcall)(U,{analyse,Opts}),
    case (S#lcp_gen.result)(U1) of
	{ok,_} ->
	    {U2,S1} = send_configure_ack(U1,S,Id,Opts),
	    this_layer_up(U2,S1#lcp_gen{state=opened});
	{reject,NewOpts,RejOpts} ->
	    {U2,S1} = send_configure_reject(U1,S,Id,RejOpts),
	    ok(U2,S1);
	{nak,NewOpts,NakOpts} ->
	    {U2,S1} = send_configure_nak(U1,S,Id,NakOpts),
	    ok(U2,S1)
    end;
configure_request(acksent,U,S,Id,Opts) ->
    U1 = (S#lcp_gen.upcall)(U,{analyse,Opts}),
    case (S#lcp_gen.result)(U1) of
	{ok,_} ->
	    {U2,S1} = send_configure_ack(U1,S,Id,Opts),
	    ok(U2,S1);
	{reject,NewOpts,RejOpts} ->
	    {U2,S1} = send_configure_reject(U1,S,Id,RejOpts),
	    ok(U2,S1#lcp_gen{state=reqsent});
	{nak,NewOpts,NakOpts} ->
	    {U2,S1} = send_configure_nak(U1,S,Id,NakOpts),
	    ok(U2,S1#lcp_gen{state=reqsent})
    end;
configure_request(opened,U,S,Id,Opts) ->
    U1 = (S#lcp_gen.upcall)(U,{analyse,Opts}),
    case (S#lcp_gen.result)(U1) of
	{ok,NewOpts} ->
 	    {U2,S1} = do_scr(U1,S,NewOpts), 
	    {U3,S2} = send_configure_ack(U1,S1,Id,Opts),
	    this_layer_down(U3,S2#lcp_gen{state=acksent});
	{reject,NewOpts,RejOpts} ->
 	    {U2,S1} = do_scr(U1,S,NewOpts), 
	    {U3,S2} = send_configure_reject(U2,S1,Id,RejOpts),
	    this_layer_down(U3,S2#lcp_gen{state=reqsent});
	{nak,NewOpts,NakOpts} ->
 	    {U2,S1} = do_scr(U1,S,NewOpts), 
	    {U3,S2} = send_configure_nak(U2,S1,Id,NakOpts),
	    this_layer_down(U3,S2#lcp_gen{state=reqsent})
    end.

do_scr(U,S,[]) -> 
    {U,S};
do_scr(U,S,Opts) ->
    send_configure_request(U,S,Opts).

%% ------------------------------------------------------
%% A Configure-Ack (CA) packet is received from the peer.
%% The CA is a positive response to a Configure-Request
%% packet. An out of sequence or otherwise invalid packet
%% is silently discarded.

configure_ack(closed,U,S,_,Opts) ->
    S1 = bump_id(S),
    ok(send_terminate_ack(U,S1,S1#lcp_gen.id,Opts));
configure_ack(stopped,U,S,_,Opts) ->
    S1 = bump_id(S),
    ok(send_terminate_ack(U,S1,S1#lcp_gen.id,Opts));
configure_ack(closing,U,S,Id,Opts) ->
    ok(U,S);
configure_ack(stopping,U,S,Id,Opts) ->
    ok(U,S);
configure_ack(reqsent,U,S,Id,Opts) ->
    S1 = init_restart_count(S,?MAX_CONFIGURE),
    ok(U,S1#lcp_gen{state=ackrcvd});
configure_ack(ackrcvd,U,S,_,_) ->
    %% crossed connection ?
    U1 = (S#lcp_gen.upcall)(U,options),
    {_,Opts} = (S#lcp_gen.result)(U1),
    {U2,S1} = send_configure_request(U1,S,Opts),
    ok(U2,S1#lcp_gen{state=reqsent});  
configure_ack(acksent,U,S,Id,Opts) ->
    {U1,S1} = stop_timer(U,S),
    S2 = init_restart_count(S1,?MAX_CONFIGURE),
    this_layer_up(U1,S2#lcp_gen{state=opened});
configure_ack(opened,U,S,Id,_) ->
    %% crossed connection ?
    U1 = (S#lcp_gen.upcall)(U,options),
    {_,Opts} = (S#lcp_gen.result)(U1),
    {U2,S1} = send_configure_request(U1,S,Opts),
    this_layer_down(U2,S1#lcp_gen{state=reqsent}).

%% ------------------------------------------------------
%% A Configure-Nak (CN) packet is received from the peer.
%% The CN is a negative response to a Configure-Request
%% packet, and indicates that one or more options are not
%% acceptable, and have to be re-negotiated.

configure_nak(closed,U,S,_,Opts) ->
    S1 = bump_id(S),
    ok(send_terminate_ack(U,S1,S1#lcp_gen.id,Opts));
configure_nak(stopped,U,S,_,Opts) ->
    S1 = bump_id(S),
    ok(send_terminate_ack(U,S1,S1#lcp_gen.id,Opts));
configure_nak(closing,U,S,Id,Opts) ->
    ok(U,S);
configure_nak(stopping,U,S,Id,Opts) ->
    ok(U,S);
configure_nak(reqsent,U,S,Id,_) ->
    U1 = (S#lcp_gen.upcall)(U,options),
    {_,Opts} = (S#lcp_gen.result)(U1),
    ok(send_configure_request(U1,init_restart_count(S,?MAX_CONFIGURE),Opts));
configure_nak(ackrcvd,U,S,Id,_) ->
    %% crossed connection ?
    U1 = (S#lcp_gen.upcall)(U,options),
    {_,Opts} = (S#lcp_gen.result)(U1),
    {U2,S2} = send_configure_request(U1,S,Opts),
    ok(U2,S2#lcp_gen{state=reqsent});   
configure_nak(acksent,U,S,Id,_) ->
    U1 = (S#lcp_gen.upcall)(U,options),
    {_,Opts} = (S#lcp_gen.result)(U1),
    ok(send_configure_request(U1,init_restart_count(S,?MAX_CONFIGURE),Opts));
configure_nak(opened,U,S,Id,_) ->
    %% crossed connection ?
    U1 = (S#lcp_gen.upcall)(U,options),
    {_,Opts} = (S#lcp_gen.result)(U1),
    {U2,S2} = send_configure_request(U1,S,Opts),
    this_layer_down(U2,S2#lcp_gen{state=reqsent}).

%% ---------------------------------------------------------
%% A Configure-Reject (CR) packet is received from the peer.
%% The CT is a negative response to a Configure-Request
%% packet, and indicates that one or more options are not
%% recognizable or are not acceptable for negotiation.

configure_reject(closed,U,S,Id,Opts) ->
    S1 = bump_id(S),
    ok(send_terminate_ack(U,S1,S1#lcp_gen.id,Opts));
configure_reject(stopped,U,S,Id,Opts) ->
    S1 = bump_id(S),
    ok(send_terminate_ack(U,S1,S1#lcp_gen.id,Opts));
configure_reject(closing,U,S,Id,Opts) ->
    ok(U,S);
configure_reject(stopping,U,S,Id,Opts) ->
    ok(U,S);
configure_reject(reqsent,U,S,Id,_) ->
    U1 = (S#lcp_gen.upcall)(U,options),
    {_,Opts} = (S#lcp_gen.result)(U1),
    ok(send_configure_request(U1,init_restart_count(S,?MAX_CONFIGURE),Opts));
configure_reject(ackrcvd,U,S,Id,_) ->
    %% crossed connection ?
    U1 = (S#lcp_gen.upcall)(U,options),
    {_,Opts} = (S#lcp_gen.result)(U1),
    {U2,S2} = send_configure_request(U1,S,Opts),
    ok(U2,S2#lcp_gen{state=reqsent});   
configure_reject(acksent,U,S,Id,_) ->
    U1 = (S#lcp_gen.upcall)(U,options),
    {_,Opts} = (S#lcp_gen.result)(U1),
    ok(send_configure_request(U1,init_restart_count(S,?MAX_CONFIGURE),Opts));
configure_reject(opened,U,S,Id,_) ->
    %% crossed connection ?
    U1 = (S#lcp_gen.upcall)(U,options),
    {_,Opts} = (S#lcp_gen.result)(U1),
    {U2,S1} = send_configure_request(U1,S,Opts),
    this_layer_down(U2,S1#lcp_gen{state=reqsent}).

%% ----------------------------------------------------------
%% A Terminate-Request (TR) packet is received from the peer.
%% The TR packet indicates the desire of the peer to close
%% the connection.

terminate_request(ackrcvd,U,S,Id,Opts) ->
    {U1,S1} = send_terminate_ack(U,S,Id,Opts),
    ok(U1,S1#lcp_gen{state=reqsent});
terminate_request(acksent,U,S,Id,Opts) ->
    {U1,S1} = send_terminate_ack(U,S,Id,Opts),
    ok(U1,S1#lcp_gen{state=reqsent});
terminate_request(opened,U,S,Id,Opts) ->
    {U1,S1} = send_terminate_ack(U,zero_restart_count(S),Id,Opts),
    this_layer_down(U1,S1#lcp_gen{state=stopping});
terminate_request(State,U,S,Id, Opts) ->
    ok(send_terminate_ack(U,S,Id,Opts)).

%% -----------------------------------------------------------
%% A Terminate-Ack (TA) packet is received from the peer.
%% The TA packet is usually a response to a Terminate-Request
%% packet. The TA packet may also indicate that the peer is 
%% in Closed or Stopped states, and servers to re-synchronize
%% the link configuration.

terminate_ack(closed,U,S,Id,Opts) -> 
    ok(U,S);
terminate_ack(stopped,U,S,Id,Opts) -> 
    ok(U,S);
terminate_ack(closing,U,S,Id,Opts) ->
    this_layer_finished(U,S#lcp_gen{state=closed});
terminate_ack(stopping,U,S,Id,Opts) ->
    this_layer_finished(U,S#lcp_gen{state=stopped});
terminate_ack(reqsent,U,S,Id,Opts) ->
    ok(U,S);
terminate_ack(ackrcvd,U,S,Id,Opts) ->
    ok(U,S#lcp_gen{state=reqsent});
terminate_ack(acksent,U,S,Id,Opts) ->
    ok(U,S);
terminate_ack(opened,U,S,Id,_) ->
    U1 = (S#lcp_gen.upcall)(U,options),
    {_,Opts} = (S#lcp_gen.result)(U1),
    {U2,S1} = send_configure_request(U1,S,Opts),
    this_layer_down(U2,S1#lcp_gen{state=reqsent}).

%% -----------------------------------------------------
%% A Code-Reject (CJ) packet is received from the peer.
%% A 'good' CJ event arises when the rejectd value is
%% acceptable, such as a CJ of an extended code.
%% A 'bad' CJ event arises when the rejectd value is
%% catastrophic, such as a CJ of Configure-Request.

rxj_good(ackrcvd,U,S) -> ok(U,S#lcp_gen{state=reqsent});
rxj_good(State,U,S)   -> ok(U,S).

rxj_bad(closed,  U,S) -> this_layer_finished(U,S);
rxj_bad(stopped, U,S) -> this_layer_finished(U,S);
rxj_bad(closing, U,S) -> this_layer_finished(U,S#lcp_gen{state=closed});
rxj_bad(stopping,U,S) -> this_layer_finished(U,S#lcp_gen{state=stopped});
rxj_bad(reqsent, U,S) -> this_layer_finished(U,S#lcp_gen{state=stopped});
rxj_bad(ackrcvd, U,S) -> this_layer_finished(U,S#lcp_gen{state=stopped});
rxj_bad(acksent, U,S) -> this_layer_finished(U,S#lcp_gen{state=stopped});
rxj_bad(opened,  U,S) ->
    terminate_request(U,S).


terminate_request(U,S) ->
    S1 = init_restart_count(S, ?MAX_TERMINATE),
    {U1,S2} = send_terminate_request(U,S1),
    this_layer_down(U1,S2#lcp_gen{state=stopping}).

%% -------
%% ACTIONS
%% -------

illegal_event(U,S,Event,State) ->
    Upcall = S#lcp_gen.upcall,
    Upcall(U,{log,"illegal event: ~w ,in state: ~w~n",[Event,State]}).

init_restart_count(S,Value) ->               %% irc
    S#lcp_gen{restart_cnt=Value}.

zero_restart_count(S) ->                     %% zrc
    S#lcp_gen{restart_cnt=0}.

this_layer_up(U,S) ->                        %% tlu
    U1 = (S#lcp_gen.upcall)(U,{log,"this layer up~n",[]}),
    {tlu,U1,S}.

this_layer_down(U,S) ->                      %% tld
    U1 = (S#lcp_gen.upcall)(U,{log,"this layer down~n",[]}),
    {tld,U1,S}.

this_layer_finished(U,S) ->                  %% tlf
    U1 = (S#lcp_gen.upcall)(U,{log,"this layer finished~n",[]}),
    {tlf,U1,S}.
    
this_layer_started(U,S) ->                   %% tls
    U1 = (S#lcp_gen.upcall)(U,{log,"this layer stated~n",[]}),
    {tls,U1,S}.

send_configure_request(U,S,Opts) ->          %% scr
    {U1,S1} = start_timer(U,S),
    S2 = bump_id(S1),
    send(U1,S2,S2#lcp_gen.id,Opts,?CONFIGURE_REQUEST,"scr").

send_configure_ack(U,S,Id,Opts) ->           %% sca
    send(U,S,Id,Opts,?CONFIGURE_ACK,"sca").

send_configure_nak(U,S,Id,Opts) ->           %% scn
    send(U,S,Id,Opts,?CONFIGURE_NAK,"scn-nak").

send_configure_reject(U,S,Id,Opts) ->        %% scn
    send(U,S,Id,Opts,?CONFIGURE_REJECT,"scn-rej").

send_terminate_request(U,S) ->               %% str
    {U1,S1} = start_timer(U,S),
    S2 = bump_id(S1),
    send(U1,S2,S2#lcp_gen.id,[],?TERMINATE_REQUEST,"scr").

send_terminate_ack(U,S,Id,Opts) ->           %% sta
    send(U,S,Id,Opts,?TERMINATE_ACK,"term-ack").

send(U,S,Id,Opts,Type,Action) ->
    Upcall = S#lcp_gen.upcall,
    U1 = Upcall(U,{encode,Opts}),
    case (S#lcp_gen.result)(U1) of
	{ok,Data} ->
	    Bytes = [Type,Id|i16(length(Data)+4) ++ Data],
	    U2 = Upcall(U1,{send,Bytes}),
	    {U2,S};
	{error,Reason} ->
	    What = {log,"~s, encode options failed, reason: ~p !~n",
		    [Action,Reason]},
	    U2 = (S#lcp_gen.upcall)(U1,What),
	    {U2,S}
    end.

%% -------------------------------------------------------
%% There is one special timer used by the automaton.
%% The Restart timer is used to time transmissions of
%% Configure-Request (CR) and Terminate-Request (TR) 
%% packets. Expirations of the Restart timer causes a 
%% Timeout event, and retransmission of the corresponding
%% CR or TR packet.

start_timer(U,S) ->
    {U1,S1} = stop_timer(U,S),
    Rcnt = S1#lcp_gen.restart_cnt,
    Timeout = S1#lcp_gen.timeout,
    U2 = (S1#lcp_gen.upcall)(U1,{start_timer,Timeout}),
    case (S1#lcp_gen.result)(U2) of
	{ok,Ticket} ->
	    {U2,S1#lcp_gen{timer_ticket=Ticket,restart_cnt=Rcnt-1}};
	{error,Reason} ->
	    What = {log,"failed to start timer, reason: ~p !~n",[Reason]},
	    U3 = (S1#lcp_gen.upcall)(U2,What),
	    {U3,S1}
    end.

stop_timer(U,S) when S#lcp_gen.timer_ticket =/= false ->
    Rcnt = S#lcp_gen.restart_cnt,
    U1 = (S#lcp_gen.upcall)(U,{stop_timer,S#lcp_gen.timer_ticket}),
    {U1,S#lcp_gen{restart_cnt=Rcnt+1,timer_ticket=false}};
stop_timer(U,S) ->
    {U,S}.

%% ------
%% Return

ok({U,S}) -> {ok,U,S}.
ok(U,S)   -> {ok,U,S}.

%% -------------
%% Misc routines
%% -------------

%% The identifier field is one byte
bump_id(S) ->
    Nid = (S#lcp_gen.id + 1) rem 256,
    S#lcp_gen{id=Nid}.

%% Log the current State and Event
log_se(U,S,State,Event) ->
    What = {log,"got event (~p) in state (~p)~n",[Event,State]},
    (S#lcp_gen.upcall)(U,What).

