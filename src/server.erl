-module(server).

-export([start/1, accept_loop/1, receive_loop/1]).

-define(PORT, 8091).

start(NOAcceptors) ->
    ets:new(accepted, [public, named_table]),
    {ok, ListenSocket} = gen_tcp:listen(?PORT, [{active,once}, {reuseaddr, true}]),
    [ spawn(?MODULE, accept_loop, [ListenSocket]) || _ <- lists:seq(1, NOAcceptors) ].

accept_loop(LSock) ->
    {ok, ASock} = gen_tcp:accept(LSock),
    {ok, {Ip, Port}} = inet:peername(ASock),
    ets:insert(accepted, {Port, Ip}),
    spawn(?MODULE, accept_loop, [LSock]),
    receive_loop(ASock).

receive_loop(Socket) ->
    receive
        {tcp, _S, "Ping"} ->
            ok = gen_tcp:send(Socket, "Pong"),
            ok = inet:setopts(Socket, [{active, once}]),
            receive_loop(Socket);
        Other ->
            io:format("error: ~p~n", [Other]),
            ok
    end.
