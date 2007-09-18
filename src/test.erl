-module(test).
-export([pajax/0,phome/0,ajax/0,home/0]).

%% -----------
%% Debug eppp

pajax() ->
    dbg(),
    eppp:start(ajax).

phome() ->
    dbg(),
    eppp:start(home).

dbg() ->
    int:i(eppp),
    int:i(eppp_lcp),
    int:break(eppp,50),
    int:auto_attach(break).

%% -------------------------------------
%% Test the serial- and the chat code.

ajax() -> go("/dev/cuaa0",ajax("54193")).
home() -> go("/dev/cuaa1",home("99254170")).

go(Device,Chat) ->
    Serial = eppp_serial:start(),
    eppp_serial:speed(Serial,9600),
    eppp_serial:parity(Serial,none),
    eppp_serial:open(Serial,Device),
    eppp_serial:chat(Serial,Chat).

home(Telno) when integer(Telno) ->
    home(integer_to_list(Telno));
home(Telno) when list(Telno) ->
    [{abort,"BUSY"},
     {abort,"NO CARRIER"},
     {timeout,10},
     break,
     {sex,"AT","OK"},
     {sub_sex,"AT","OK"},
     {sex,"ATE1Q0","OK"},
     {delay,1},
     {timeout,40},
     {sex,"ATDT" ++ Telno,"CONNECT"}
     ].

ajax(Telno) when integer(Telno) ->
    ajax(integer_to_list(Telno));
ajax(Telno) when list(Telno) ->
    [{abort,"BUSY"},
     {abort,"NO CARRIER"},
     {timeout,10},
     break,
     {sex,"AT","OK"},
     {sub_sex,"AT","OK"},
     {timeout,40},
     {sex,"ATDT" ++ Telno,"CONNECT"}
     ].



