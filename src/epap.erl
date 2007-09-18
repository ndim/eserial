-module(epap).
-author('tobbe@cslab.ericsson.se').
%%----------------------------------------------------------------------
%% File    : epap.erl
%% Created : 25 Oct 1998 by tobbe@cslab.ericsson.se
%% Function: PPP Password Authentication Protocol (RFC-1334)
%%----------------------------------------------------------------------
-export([enc_request/2]).

-import(eppp,[i16/1]).

-define(AUTH_REQ,  1).
-define(AUTH_ACK,  2).
-define(AUTH_NACK, 3).


enc_request(Id,Name,Key) when integer(Id),list(Name),list(Key) ->
    Ln = length(Name) band 16#ff,
    Lk = length(Key) band 16#ff,
    [Hi,Lo] = i16(4+Ln+Lk),
    [?AUTH_REQ,Id,Hi,Lo,Ln|Name ++ [Lk|Key]].

