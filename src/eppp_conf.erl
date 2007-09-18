-module(eppp_conf).
-author('tobbe@serc.rmit.edu.au').
%%% -------------------------------------------------------------------
%%% Created : 10 Mar 1998 by tobbe@serc.rmit.edu.au
%%% Function: Configuration handling, etc. 
%%%
%%%   The file .eppp contains configuration parameters that
%%%   controls how eppp shall setup the PPP link. The file is
%%%   assumed to be located in the directory specified by the
%%%   EPPP_HOME or HOME environment variable.
%%%
%%%   To be able define several different scenarios, the file 
%%%   is structured in an arbitrary number of 'session definitions'. 
%%%   A session definition consists of a tuple: 
%%%
%%%     {SessionTag, ListOfOptions},
%%%
%%%   where an Option is defined as a tuple: {Key,Value}.
%%%
%%%   The SessionTag 'default' has a special meaning and is used
%%%   for configuration options that all sessions has in common.
%%%
%%%   initrc()              -  Read the config file and return: 
%%%                             {ok,Dict} | {error,Reason}
%%%
%%%   init_logger(Dict)     -  Initialize the log mechanism to be used.
%%%                              Return:  {ok,LogHandle} | {error,Reason}
%%%
%%%   read_const(Dict,Key)  -  Return corresponding value, return:
%%%                              {ok,Value} | {error,Reason}
%%%
%%%   set_session_values(Dict,       - Store all config values in Dict.
%%%                      SessionTag)   Return: NewDict
%%%
%%% -------------------------------------------------------------------
-vc('$Id: eppp_conf.erl,v 1.1.1.1 1999/09/05 09:54:04 tobbe Exp $ ').
-export([initrc/0,init_logger/1,read_const/2,set_session_values/2]).

-import(eppp_log,[imsg/2]).

-define(EPPP_CONF, ".eppp").

%%% -----------------------------------------
%%% Read the config file and store the info. 
%%% Return a dictionary.

initrc() ->
    case eppp_home() of
	{ok,Home} -> initrc(file:consult(Home ++ "/" ++ ?EPPP_CONF));
	Else      -> Else
    end.

eppp_home() ->
    case os:getenv("EPPP_HOME") of
	false ->
	    case os:getenv("HOME") of
		false -> {error,no_home_found};
		Home  -> {ok,Home}
	    end;
	Home ->
	    {ok,Home}
    end.

initrc({ok,Vs}) ->
    {ok,set_defaults(store_options(init_dict(),Vs))};
initrc(Else) ->
    {error,init_file}.

%% Store each {Key,Value} in the dictionary

store_options(Dict,Vs) ->
    F = fun({K,V},D) -> dict:store(K,V,D) end,
    lists:foldl(F,Dict,Vs).

%% Get the default 'session' and set the 
%% corresponding {Key,Value} pairs.

set_defaults(Dict) -> 
    set_session_values(Dict,default).

%% Initialize the dictionary

init_dict() ->
    store_options(dict:new(),default_values()).
    

%% Reasonable default values 

default_values() -> 
    [{authkey,""},
     {authname,""},
     {dial,[]},
     {device,"/dev/cuaa0"}, % I'm running FreeBSD ;-)
     {log_dir,log_dir()},
     {login,[]},
     {parity,none},
     {speed,9600}].

%% This module should exist in:        $(EPPP_DIR)/ebin
%% Default log directory therefore is: $(EPPP_DIR)/log

log_dir() ->
    {file,ObjFile} = code:is_loaded(?MODULE),
    Dir = filename:dirname(ObjFile),
    Dir ++ "/../log".

%%% --------------------------------------------
%%% Read the value of specified constant.
%%% NB: The values are regarded to be constants 
%%% so there exist no write function !!

read_const(Dict,K) -> 
    case catch dict:fetch(K,Dict) of
	{ok,Value} -> {ok,Value};
	_          -> {error,not_found}
    end.

%% ----------------------------------------------------
%% Set all the config parameters for specified session.

set_session_values(Dict,SessionKey) ->
    case read_const(Dict,SessionKey) of
	Vs when list(Vs) -> store_options(Dict,Vs);
	_                -> Dict
    end.

%% ----------------------------------------------------------
%% Default log mechanism is to use syslog. If: {syslog,false}
%% is explicitly specified then use a logfile in one of the
%% listed log directories.

init_logger(Dict) ->
    case read_const(Dict,syslog) of
	{ok,false} -> activate_logfile(Dict);
	_          -> eppp_log:init_syslog()
    end.

%% Activate the logfile specified in the configuration file

activate_logfile(Dict) ->
    case read_const(Dict,log_dir) of
	{ok,Dirs} when list(Dirs) ->
	    open_one_logdir(Dirs);
	_ ->
	    eppp_log:init_tty()
    end.

%% Try to open one of the specified directories.

open_one_logdir([Dir|T]) ->
    {Y,M,D} = date(),
    LogFile = lists:concat([Dir ++ "/logfile.",Y,"-",M,"-",D]),
    case eppp_log:init_logfile(LogFile) of
	{ok,Log}  -> {ok,Log};
	{error,_} -> open_one_logdir(T)
    end;
open_one_logdir(_) ->
    {error,open_logdir}.


