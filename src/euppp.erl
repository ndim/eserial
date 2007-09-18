-module(euppp).
-author('tobbe@cslab.ericsson.se').
%%----------------------------------------------------------------------
%% File    : euppp.erl
%% Created : 20 Oct 1998 by tobbe@cslab.ericsson.se
%% Function: Erlang User PPP.
%%----------------------------------------------------------------------
-vc('$Id: euppp.erl,v 1.1.1.1 1999/09/05 09:54:20 tobbe Exp $ ').
-export([start/1,stop/1,version/0]).
-export([init/1]).

-import(error_logger,[error_msg/1]).

-include("estreams.hrl").

version() ->
    [_,Rev|_] = string:tokens("$Revision: 1.1.1.1 $ "," "),Rev.

start(Session) -> 
    Pid = spawn(?MODULE,init,[Session]),
    wait(Pid).

stop(Pid) when pid(Pid) ->
    call(Pid,stop),
    wait(Pid).

%% ----------
%% The server    
%% ----------

init(Pid) ->
    case initrc() of
	ok -> 
	    return(Pid,{ok,self()}),
	    configure();
	Else -> 
	    return(Pid,Else),
	    exit(Else)
    end.

configure() ->
    set_defaults(),
    activate_logfile(),
    configure_session().

activate_logfile() ->
    case read_const(log_dir) of
	Dir when list(Dir) ->
	    {Y,M,D} = date(),
	    LogFile = lists:concat([Dir ++ "/logfile.",Y,"-",M,"-",D]),
	    case error_logger:logfile({open,LogFile}) of
		ok -> error_logger:tty(false);
		_  -> error_msg("euppp: Could not open logfile !")
	    end;
	_ ->
	    error_msg("euppp: No log directory specified !"),
	    exit(log_dir)
    end.

configure_session() ->
    receive
	{_,run,Session} ->
	    setup_session(Session);
	{From,stop} ->
	    return(From,ok),
	    error_logger:logfile(close),
	    exit(normal)
    end.


setup_session(Session) -> 
    set_options(Session),
    Protocols = get_protocols(),
    Top = hd(Protocols),
    S = apply(Top,start,[tl(Protocols)]),
    configure(Top,S).

configure(Top,S) ->
    config_modem(Top,S),
    config_ppp(Top,S).

config_modem(Top,S) ->
    config(Top,device,?MODEM_DEVICE).
    config(Top,speed,?MODEM_SPEED),
    config(Top,parity,?MODEM_PARITY),
    config(Top,chat,?MODEM_CHAT_SCRIPT).

config_ppp(Top,S) ->
    config(Top,authkey,?PPP_AUTHKEY),
    config(Top,authname,?PPP_AUTHNAME).

config(Top,Opt,Tag) ->
    Val = getopt(Opt),
    apply(Top,config,[S,Tag,Val]).

%% NB: 'undefined' is not a valid option.
getopt(Opt) ->    
    case read_const(Opt) of
	undefined -> default_value(Opt);
	Value     -> Value
    end.

%% -----------------------------------------
%% Reasonable default values of the allowed 
%% options from the '.euppp' file.

default_value(authkey)     -> "";
default_value(authname)    -> "";
default_value(chat)        -> [];
default_value(chat_module) -> chat;
default_value(device)      -> "/dev/cuaa1";
default_value(log_dir)     -> log_dir();
default_value(parity)      -> none;
default_value(protocols)   -> [eppp,modem];
default_value(speed)       -> 9600.

%% -------------------------------------------
%% Default log directory is: $(EUPPP_DIR)/log

log_dir() ->
    %% NB: Can't fail below...
    {file,ObjFile} = code:is_loaded(?MODULE),
    Dir = filename:dirname(ObjFile),
    %% EUPPP root is one level up
    Dir ++ "/../log".

set_defaults() -> 
    set_options(default).

set_options(Tag) -> 
    case read_const(Tag) of
	Vs when list(Vs) -> store_options(Vs);
	_                -> ok
    end.

%% -----------------------------------------------------------
%% Deal with the user configuration file.
%% Try and find the file in the $EUPPP_HOME directory or the
%% $HOME directory. Read the file .euppp and store the values
%% (in the process dictionary).
%% NB: The values are regarded to be constants so there exist
%% no write function !!

initrc() ->
    case euppp_home() of
	{ok,Home} -> initrc(file:consult(Home ++ "/.euppp"));
	Else      -> Else
    end.

euppp_home() ->
    case os:getenv("EUPPP_HOME") of
	false ->
	    case os:getenv("HOME") of
		false -> {error,no_home_found};
		Home  -> {ok,Home}
	    end;
	Home ->
	    {ok,Home}
    end.

initrc({ok,Vs}) ->
    store_options(Vs),
    ok;
initrc(Else) ->
    {error,init_file}.

store_options(Vs) ->
    F = fun({K,V}) -> put(K,V) end,
    lists:foreach(F,Vs).

read_const(K) -> get(K).


call(Pid,Msg)   -> send(Pid,Msg).
wait(Pid)       -> receive {Pid,Msg} -> Msg end.
return(Pid,Msg) -> send(Pid,Msg).
send(Pid,Msg)   -> Pid ! {self(),Msg}.
send(Pid,Tag,Msg)   -> Pid ! {self(),Tag,Msg}.

