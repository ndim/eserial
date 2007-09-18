-module(eppp_log).
-author('tobbe@cslab.ericsson.se').
%%% --------------------------------------------------------------------
%%% File    : eppp_log.erl
%%% Created : 27 Oct 1998 by tobbe@cslab.ericsson.se
%%% Function: Log handling for eppp. 
%%%
%%%   Two kinds of log mechanisms are supported, using the
%%%   Unix syslog facility, or log to file.
%%%
%%%   Log-messages are written depending on what the current 
%%%   log level is. For example: If the log-level is MEDIUM, 
%%%   then only those messages written by the w1/3 and w2/3
%%%   functions will be outputed.
%%%
%%%   NB: The format-string and argument-list arguments may 
%%%       either be as expected of io:fwrite/2, or anomynous 
%%%       functions F/0 which, when called, produces the correct
%%%       arguments. This makes it possible to delay the evaluation 
%%%       of the arguments until it is really needed.
%%%
%%% --------------------------------------------------------------------
-vc('$Id: eppp_log.erl,v 1.1.1.1 1999/09/05 09:54:16 tobbe Exp $ ').
-export([init_logfile/2,init_tty/1,init_syslog/1,version/0,
	 w1/3,w2/3,w3/3,emsg/3]).

version() ->
    [_,Rev|_] = string:tokens("$Revision: 1.1.1.1 $ "," "),Rev.

%% Log levels, specifies amount of information to be output.

-define(LOW,    1).
-define(MEDIUM, 2).
-define(HIGH,   3).

-record(log,
	{level=?MEDIUM,  % Log level. Default is medium output
	 type            % Type of log: errlog | syslog 
	}).

%% -----------------------------
%% Initialize the log mechanism

init_logfile(Level,LogFile) when integer(Level),
				 Level >= ?LOW, Level =< ?HIGH,
				 list(LogFile) ->
    case error_logger:logfile({open,LogFile}) of
	ok -> 
	    error_logger:tty(false),
	    {ok,#log{type=errlog,level=Level}};
	_  -> 
	    {error,init_logfile}
    end.

init_tty(Level) ->
    error_logger:tty(true),
    {ok,#log{type=errlog,level=Level}}.

init_syslog(Level) ->
    eppp_syslog:add_handler(),
    {ok,#log{type=syslog,level=Level}}.
    

%% ---------------------------------------------
%% Write message if log level is LOW or higher.

w1(L,Fstr,Args) when L#log.type == syslog -> w1s(L#log.level,Fstr,Args);
w1(L,Fstr,Args) when L#log.type == errlog -> w1e(L#log.level,Fstr,Args).

w1s(Level,Fstr,Args) when Level >= ?LOW -> info(Fstr,Args);
w1s(_,_,_)                              -> true.

w1e(Level,Fstr,Args) when Level >= ?LOW -> imsg(Fstr,Args);
w1e(_,_,_)                              -> true.

%% ------------------------------------------------
%% Write message if log level is ?MEDIUM or higher.

w2(L,Fstr,Args) when L#log.type == syslog -> w2s(L#log.level,Fstr,Args);
w2(L,Fstr,Args) when L#log.type == errlog -> w2e(L#log.level,Fstr,Args).

w2s(Level,Fstr,Args) when Level >= ?MEDIUM -> info(Fstr,Args);
w2s(_,_,_)                                 -> true.

w2e(Level,Fstr,Args) when Level >= ?MEDIUM -> imsg(Fstr,Args);
w2e(_,_,_)                                 -> true.

%% -----------------------------------------------
%% Write message if log level is ?HIGH or higher.

w3(L,Fstr,Args) when L#log.type == syslog -> w3s(L#log.level,Fstr,Args);
w3(L,Fstr,Args) when L#log.type == errlog -> w3e(L#log.level,Fstr,Args).

w3s(Level,Fstr,Args) when Level >= ?HIGH  -> dbg(Fstr,Args);
w3s(_,_,_)                                -> true.

w3e(Level,Fstr,Args) when Level >= ?HIGH  -> imsg(Fstr,Args);
w3e(_,_,_)                                -> true.

%% ---------------------
%% Write error messages

emsg(L,Fstr,Args) when L#log.type == syslog -> 
    eppp_syslog:error(f2a(Fstr),f2a(Args));
emsg(L,Fstr,Args) when L#log.type == errlog -> 
    error_logger:error_msg(f2a(Fstr),f2a(Args)).

%% Write the messages

info(Fstr,Args) -> eppp_syslog:info(f2a(Fstr),f2a(Args)).
imsg(Fstr,Args) -> error_logger:info_msg(f2a(Fstr),f2a(Args)).

dbg(Fstr,Args)  -> eppp_syslog:dbg(f2a(Fstr),f2a(Args)).

%% If the argument is a function, then evaluate it.

f2a(F) when function(F) -> f2a(catch F());
f2a(L) when list(L)     -> L;
f2a(_)                  -> [].
