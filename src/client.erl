-module(client).

-export([start/1, loop/1]).

-define(SERVER, {127,0,0,1}).
-define(PORT, 8091).
-define(TIMEOUT, 10000).

start(NOConnections) ->
    case ets:info(port2pid) of
        undefined -> ets:new(port2pid, [public, named_table]);
        _ -> ok
    end,
    case ets:info(pid2port) of
        undefined -> ets:new(pid2port, [public, named_table]);
        _ -> ok
    end,
    [ spawn(?MODULE, loop, [?TIMEOUT]) || _ <- lists:seq(1,NOConnections) ].

loop(Timeout) ->
    {ok, S} = gen_tcp:connect(?SERVER, ?PORT, [{active,false}]),
    {ok, {_IP, Port}} = inet:sockname(S),
    true = ets:insert(pid2port, {self(), Port}),
    true = ets:insert_new(port2pid, {Port, self()}),
    ok = gen_tcp:send(S, "Ping"),
    {ok, "Pong"} = gen_tcp:recv(S, 0, Timeout),
    %% io:format("~w received Pong~n", [self()]),
    timer:sleep(infinity).
