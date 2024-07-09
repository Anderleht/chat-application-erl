-module(chat_server).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1, start/0, pre_loop/1, loop/2]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, []}.

start() -> start(1234).

start(Port) ->
  gen_server:call(?MODULE, {start, Port}).

pre_loop(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      io:format("Data: ~p~n", [binary_to_list(Data)]),
      Message = binary_to_list(Data),
      {Command, [_|NickAndPassword]} = lists:splitwith(fun(T) -> [T] =/= ":" end, Message),
      {Nick, [_|Password]} = lists:splitwith(fun(T) -> [T] =/= ":" end, NickAndPassword),
      case Command of
        "CONNECT" ->
          case chat_controller:check_credentials(clean(Nick), clean(Password)) of
            {ok, auth_ok} ->
              case chat_controller:get_socket(clean(Nick)) of
                {ok, undefined} ->
                  chat_controller:set_socket(clean(Nick), Socket),
                  gen_tcp:send(Socket, "CONNECT:OK\n"),
                  loop(clean(Nick), Socket);
                {ok, _Socket} ->
                  gen_tcp:send(Socket, {error, always_connection}),
                  gen_tcp:close(Socket)
              end;
            {ok, user_does_not_exist} ->
              chat_controller:add_user(clean(Nick), clean(Password)),
              chat_controller:set_socket(clean(Nick), Socket),
              gen_tcp:send(Socket, "CONNECT:OK\n"),
              loop(clean(Nick), Socket);
            {error, Reason} ->
              gen_tcp:send(Socket, "CONNECT:ERROR:" ++ atom_to_list(Reason) ++ "\n"),
              gen_tcp:close(Socket)
          end;
        _ ->
          gen_tcp:send(Socket, "Unknown command!\n"),
          gen_tcp:close(Socket)
      end;
    {error, closed} ->
      ok;
    {error, Reason} ->
      io:format("Error receiving data: ~p~n", [Reason]),
      gen_tcp:close(Socket)
  end.

loop(Nick, Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      io:format("Data: ~p~n", [binary_to_list(Data)]),
      Message = binary_to_list(Data),
      {Command, [_|Content]} = lists:splitwith(fun(T) -> [T] =/= ":" end, Message),
      case Command of
        "SAY" ->
          broadcast(Nick, clean(Content)),
          loop(Nick, Socket);
        "QUIT" ->
          chat_controller:remove_socket(Nick),
          gen_tcp:send(Socket, "Bye.\n"),
          gen_tcp:close(Socket),
          ok;
        _ ->
          gen_tcp:send(Socket, "Unknown command!\n"),
          loop(Nick, Socket)
      end;
    {error, closed} ->
      chat_controller:remove_socket(Nick),
      ok;
    {error, Reason} ->
      io:format("Error receiving data: ~p~n", [Reason]),
      gen_tcp:close(Socket),
      chat_controller:remove_socket(Nick)
  end.

accept_loop(ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  spawn(fun() -> pre_loop(Socket) end),
  accept_loop(ListenSocket).

broadcast(Nick, Msg) ->
  {ok, Users} = chat_controller:get_users(),
  UsersList = maps:to_list(Users),
  lists:foreach(fun({OtherNick, {_Password, Socket}}) when OtherNick =/= Nick ->
    gen_tcp:send(Socket, Nick ++ ":" ++ Msg ++ "\n");
    (_) -> ok
  end, UsersList).

clean(Data) ->
  string:strip(Data, both, $\n).

handle_call({start, Port}, _From, State) ->
  {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {packet, 2}, {active, false}, {reuseaddr, true}]),
  spawn(fun() -> accept_loop(ListenSocket) end),
  {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
