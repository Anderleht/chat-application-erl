-module(chat_controller).
-behaviour(gen_server).

-export([start_link/0, add_user/2, check_credentials/2, get_socket/1, get_users/0, set_socket/2, remove_socket/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  Users = #{},
  {ok, Users}.

add_user(Nick, Password) ->
  gen_server:call(?MODULE, {add_user, Nick, Password}).

check_credentials(Nick, Password) ->
  gen_server:call(?MODULE, {check_credentials, Nick, Password}).

get_socket(Nick) ->
  gen_server:call(?MODULE, {get_socket, Nick}).

get_users() ->
  gen_server:call(?MODULE, {get_users}).

set_socket(Nick, Socket) ->
  gen_server:call(?MODULE, {set_socket, Nick, Socket}).

remove_socket(Nick) ->
  gen_server:call(?MODULE, {remove_socket, Nick}).

handle_call({add_user, Nick, Password}, _From, Users) ->
  {reply, ok, Users#{Nick => {Password, undefined}}};

handle_call({check_credentials, Nick, Password}, _From, Users) ->
  case maps:get(Nick, Users, undefined) of
    {ExpectedPassword, undefined} when ExpectedPassword =:= Password -> {reply, {ok, auth_ok}, Users};
    {ExpectedPassword, _} when ExpectedPassword =:= Password -> {reply, {error, always_connection}, Users};
    undefined -> {reply, {ok, user_does_not_exist}, Users};
    _ -> {reply, {error, auth_error}, Users}
  end;

handle_call({get_socket, Nick}, _From, Users) ->
  case maps:get(Nick, Users, undefined) of
    {_, Socket} -> {reply, {ok, Socket}, Users};
    _ -> {reply, {error, user_not_found}, Users}
  end;

handle_call({get_users}, _From, Users) ->
  {reply, {ok, Users}, Users};

handle_call({set_socket, Nick, Socket}, _From, Users) ->
  case maps:get(Nick, Users, undefined) of
    {Password, _} -> {reply, ok, Users#{Nick => {Password, Socket}}};
    _ -> {reply, {error, user_not_found}, Users}
  end;

handle_call({remove_socket, Nick}, _From, Users) ->
  case maps:get(Nick, Users, undefined) of
    {Password, _} -> {reply, ok, Users#{Nick => {Password, undefined}}};
    _ -> {reply, {error, user_not_found}, Users}
  end;

handle_call(_Msg, _From, State) -> {reply, {error, unknown_request}, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
