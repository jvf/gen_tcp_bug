Minimal example to demonstrate a potential bug when trying to establish lots of TCP connections in parallel using Erlang's `gen_tcp`.

# Error Description

When trying to establish a largeish number of TCP connections in parallel I observe some weird behavior I would consider a potential bug in `gen_tcp`. See attached files and the instructions below.

The scenario is a server listening on a port with multiple concurrent acceptors. From a client I establish a connection by calling `gen_tcp:connect/3`, afterwards I send a "Ping" message to the server and wait in passive mode for a "Pong" response. When performing the 'get_tcp:connect/3' calls sequentially all works fine, including for large number of connections (I tested up to ~ 28000).

The problem occurs when trying to establish a lot of connections in parallel. While most of the connections still get established, some connections fail with a `closed` error in `gen_tcp:recv/3`. The weird thing is, that these connections did not fail before, the calls to `gen_tcp:connect/3` and `gen_tcp:send/2` were both successful
 (i.e. returned `ok`). On the server side I don't see a matching connection for these "weird" connections, i.e. no returning `gen_tcp:accept/1`. It is my understanding, that a successful 'get_tcp:connect/3' should result in a matching accepted connection at the server side.

I was able to reproduce the problem on the following configurations:
* Ubuntu 14.04.4 LTS + Erlang R14B
* Ubuntu 14.04.4 LTS + Erlang R15B
* Ubuntu 14.04.4 LTS + Erlang R16B
* Ubuntu 14.04.4 LTS + Erlang 17.0
* Ubuntu 14.04.4 LTS + Erlang 18.1.4
* Ubuntu 14.04.4 LTS + Erlang 18.2
* Ubuntu 14.04.4 LTS + Erlang 18.3
* OS X 10.11.5 + Erlang 18.0
* using a single Erlang VM on a single machine
* using two Erlang VM's on a single machine
* using two machines

On OS X the behavior is a bit different. In addition to the `closed` errors for the `gen_tcp:recv/3` calls I also get `econnreset` errors on the `get_tcp:connect/3` call, and `closed` errors on `gen_tcp:send/2` calls.

# How to reproduce the error

Compile (`erl -make`) and start an Erlang console (`erl -pa ebin`). Start a server with 10 acceptors:

```
server:start(10).
```

Start 100 client connections (depending on the machine you might need to start more than 100 connections):

```
client:start(100).
```

This produces the following error:

```
=ERROR REPORT==== 7-Jun-2016::17:10:06 ===
Error in process <0.91.0> with exit value:
{{badmatch,{error,closed}},[{client,loop,1,[{file,"client.erl"},{line,21}]}]}
```

Compare with the source of the client. As you can see, the client fails in the `gen_tcp:recve/3` (line 21).

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

After a successful (i.e. no error) call to `gen_tcp:connect/3`, the pid of the controlling process and the port of the connected socket are stored in an `ets` table `pid2port`.

Get a list of all processes which successfully called `connect`:
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
