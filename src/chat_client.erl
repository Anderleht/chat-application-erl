-module(chat_client).

-export([start/2, send/2, loop/1, stop/1, tcp_loop/1, client/2]).

start(Nick, Password) ->
    spawn(?MODULE, client, [Nick, Password]).

client(Nick, Password) ->
    {ok, Socket} = gen_tcp:connect("localhost", 1234, [binary, {packet, 2}, {active, false}]),
    NickBin = list_to_binary(Nick),
    PasswordBin = list_to_binary(Password),
    gen_tcp:send(Socket, <<"CONNECT:", NickBin/binary, ":", PasswordBin/binary, "\n" >>),
    spawn(?MODULE, tcp_loop, [Socket]),
    loop(Socket).

send(Pid, Msg) ->
    Pid ! {send, Msg},
    ok.

stop(Pid) ->
    Pid ! stop,
    ok.

loop(Socket) ->
    receive
        {send, Msg} ->
            MsgBin = list_to_binary(Msg),
            gen_tcp:send(Socket, <<"SAY:", MsgBin/binary>>),
            loop(Socket);
        stop ->
            gen_tcp:send(Socket, <<"QUIT:">>)
    after 200 ->
        loop(Socket)
    end.

tcp_loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Msg} ->
            io:format("~s~n", [Msg]),
            tcp_loop(Socket);
        {error, closed} ->
            io:format("Socket closed~n"),
            stop(Socket);
        {error, Reason} ->
            io:format("Error receiving data: ~p~n", [Reason]),
            stop(Socket)
    end.
