-module(eppp_serial).
-author('jb@erix.ericsson.se').
%%% ------------------------------------------------------------------
%%% File:      serial.erl
%%% Author:    Johan Bevemyr
%%% Created:   Tue Oct 22 14:07:24 1996
%%%
%%% Modified:  14 Oct 1998 by tobbe@cslab.ericsson.se
%%%            Adopted it into eppp, added some user
%%%            interface functions and a hook for my
%%%            own chat module.
%%% ------------------------------------------------------------------
-vc('$Id: eppp_serial.erl,v 1.1.1.1 1999/09/05 09:54:20 tobbe Exp $ ').
-export([start/0,start/1,send/2,connect/1,disconnect/1,
	 open/2,speed/2,speed/3,parity_odd/1,parity_even/1,
	 break/1,stop/1,chat/3,parity/2,log_level/2]).
%% Internal exports
-export([init/1,loop/3]).


-define(SEND,0).
-define(CONNECT,1).
-define(DISCONNECT,2).
-define(OPEN,3).
-define(CLOSE,4).
-define(SPEED,5).
-define(PARITY_ODD,6).
-define(PARITY_EVEN,7).
-define(BREAK,8).

-define(DEFAULT_LOG_LEVEL, 2).


start() ->
    start([]).

start(Options) ->
    Pid = spawn_link(?MODULE, init, [self()]),
    process_options(Pid,Options),
    Pid.

send(S,Bytes)       -> S ! {send,Bytes}.
connect(S)          -> S ! connect.
disconnect(S)       -> S ! disconnect.
open(S,Device)      -> S ! {open,Device}.
speed(S,Speed)      -> S ! {speed,Speed,Speed}.
speed(S,In,Out)     -> S ! {speed,In,Out}.
parity(S,none)      -> ok;
parity(S,odd)       -> parity_odd(S);
parity(S,even)      -> parity_even(S).
parity_odd(S)       -> S ! parity_odd.
parity_even(S)      -> S ! parity_even.
break(S)            -> S ! break.
stop(S)             -> S ! stop. 
log_level(S,Level)  -> S ! {log_level,Level}.

%% F/1 is called with either: ok | {error,Reason}
chat(S,Chat,F)  -> S ! {chat,Chat,F}. 


process_options(Pid,[]) -> done;
process_options(Pid,[Opt|Opts]) ->
    Pid ! Opt,
    process_options(Pid,Opts).

%% ----------------
%% The server part
%% ----------------

init(Pid) ->
    process_flag(trap_exit,true),
    Port = open_port({spawn, bin_dir() ++ "/eppp_serial -erlang"},
		     [binary,{packet,2}]),
    loop(Pid,Port,?DEFAULT_LOG_LEVEL).

%% ------------------------------------------------
%% The Erlang executables should exist in the 
%% $(EPPP_DIR)/ebin directory and the eppp_serial 
%% executable in $(EPPP_DIR)/bin.

bin_dir() ->
    {file,ObjFile} = code:is_loaded(?MODULE),
    Dir = filename:dirname(ObjFile),
    Dir ++ "/../bin",
    ".".  %% TEMPORARY HACK FOR DEBUGGING PURPOSES !!!!!!


loop(Pid,Port,LogLevel) ->
    receive

	%% Incoming bytes from the serial device

	{Port, {data, Bytes}} ->
	    Pid ! {self(),{data, Bytes}},
	    ?MODULE:loop(Pid,Port,LogLevel);

	%% Outgoing bytes to be sent to the serial device

	{send, Bytes} ->
	    send_serial(Port,[?SEND,Bytes]),
	    ?MODULE:loop(Pid,Port,LogLevel);

	%% Start chatting to the serial device

	{chat,Chat,F} ->
	    case do_chat(Port,Chat,LogLevel) of
		ok -> F(ok);
		_  -> F({error,chat_failed})
	    end,
	    ?MODULE:loop(Pid,Port,LogLevel);

	%% Set various options

	{log_level,NewLogLevel} ->
	    ?MODULE:loop(Pid,Port,NewLogLevel);

	connect ->
	    send_serial(Port,[?CONNECT]),
	    ?MODULE:loop(Pid,Port,LogLevel);

	disconnect ->
	    send_serial(Port,[?DISCONNECT]),
	    ?MODULE:loop(Pid,Port,LogLevel);

	{open, TTY} ->
	    send_serial(Port,[?OPEN,TTY]),
	    ?MODULE:loop(Pid,Port,LogLevel);

	{close} ->
	    send_serial(Port,[?CLOSE]),
	    ?MODULE:loop(Pid,Port,LogLevel);

	{speed, NewInSpeed, NewOutSpeed} ->
	    send_serial(Port,[?SPEED,integer_to_list(NewInSpeed)," ",
			      integer_to_list(NewOutSpeed),0]),
	    ?MODULE:loop(Pid,Port,LogLevel);

	parity_odd ->
	    send_serial(Port,[?PARITY_ODD]),
	    ?MODULE:loop(Pid,Port,LogLevel);

	parity_even ->
	    send_serial(Port,[?PARITY_EVEN]),
	    ?MODULE:loop(Pid,Port,LogLevel);

	break ->
	    send_serial(Port,[?BREAK]),
	    ?MODULE:loop(Pid,Port,LogLevel);

	stop ->
	    stopped;

	%% Erroneous cases

	{'EXIT', Port, Why} ->
	    io:format("Port exited with reason ~w~n", [Why]),
	    exit(Why);

	{'EXIT', Linked, Why} ->
	    io:format("Linked ~w exited with reason ~w~n", [Linked,Why]),
	    exit(Why);

	OtherError ->
	    io:format("Received unknown message ~w~n",[OtherError]),
	    ?MODULE:loop(Pid,Port,LogLevel)
    end.

%% -------------------------------------------------------
%% Setup the callback functions for the eppp_chat module.

do_chat(Port,Chat,LogLevel) ->
    Send = fun(Bytes) -> send_serial(Port,[?SEND,Bytes]) end,
    Recv = fun(Timeout,Cont) ->
		   receive
		       {Port, {data, Bytes}} ->
			   Cont({ok,binary_to_list(Bytes)})
		   after Timeout ->
			   Cont(timeout)
		   end
	   end,
    Log  = fun(Fstr,Args) -> 
		   Fs = fun() -> "eppp_chat(~w): " ++ Fstr end,
		   Fa = fun() -> [eppp:ts()|Args] end,
		   error_logger:w2(LogLevel,Fs,Fa) 
	   end,
    eppp_chat:run(Send,Recv,Log,Chat).

	
send_serial(Port,Message) ->
    Port ! {self(),{command,Message}}.

