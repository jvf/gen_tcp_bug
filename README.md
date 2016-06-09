Minimal example to demonstrate an error when trying to establish lots of TCP connections in parallel using Erlang's `gen_tcp`. 

# Error Description

When trying to establish a largeish number of TCP connections in parallel I observe some weird behavior I would consider a potential bug in `gen_tcp`. See attached files and the instructions below.

The scenario is a server listening to a port with multiple concurrent acceptors. From a client I establish a connection by calling `gen_tcp:connect/3`, afterwards I send a "Ping" message to the server and wait in passive mode for a "Pong" response. Normally this all works fine. The problem occurs when trying to establish a lot of connections in parallel. While most of the connections still get established, some connections fail with a `closed` error in `gen_tcp:receiv/3`. The weird thing is, that these connections did not fail before, the calls to `gen_tcp:connect/3` and `gen_tcp:send/2` were both successful
 (i.e. returned `ok`). On the server side I don't see a matching connection for these "weird" connections, i.e. no returning `gen_tcp:accept/1`. It is my understanding, that a successful 'get_tcp:connect/3' should result in a matching accepted connection at the client side.

I was able to reproduce the problem on the following configurations:
* Ubuntu 14.04.4 LTS + Erlang 18.2

# How to reproduce the error

Start a server with 10 acceptors:

```
server:start(10).
```

Start 100 client connections:

```
client:start(100).
```

This produces the following error:

```
=ERROR REPORT==== 7-Jun-2016::17:10:06 ===
Error in process <0.91.0> with exit value:
{{badmatch,{error,closed}},[{client,loop,1,[{file,"client.erl"},{line,26}]}]}
```

Compare with the source of the client. As you can see, the client fails in the `gen_tcp:receive/3` (line 26).

To verify, that the server did not see the connection (even though the connect from the client was successfull) get the pid of the process controlling the problematic connection from the error message, here  `<0.91.0>`. Get the port from the socket this process was controlling:

```
> ets:lookup(pid2port, pid(0,91,0)).
[{<0.91.0>,64383}]
```

Use the port to check the table of accepted connections from the server:

```
> ets:lookup(accepted, 64383).
[]
```

No connection with the respective socket was accepted at the server!


# ETS tables

The attached example code uses the following `ets` tables to track some global information on the connections.

## Client

After a successful (i.e. no error) call to `gen_tcp:connect/3`, the port of the connected socket and the pid of the controlling process are stored within the two `ets` tables `port2pid` and `pid2port`.

Get a list of all processes which successfully called `connect`:
```
ets:tab2list(port2pid).
```

```
length(ets:tab2list(port2pid)).
```
```
ets:tab2list(pid2port).
```

```
length(ets:tab2list(pid2port)).
```

## Server

After a `gen_tcp:accept/1` returned without error, the port and pid of the controlling process are stored in the `ets` table `accepted`. See all accepted connections:
```
ets:tab2list(accepted).
```
```
length(ets:tab2list(accepted)).
```
