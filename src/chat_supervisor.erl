-module(chat_supervisor).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ControllerSpec = #{id => chat_controller,
        start => {chat_controller, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [chat_controller]},

    ServerSpec = #{id => chat_server,
        start => {chat_server, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [chat_server]},

    {ok, {{one_for_one, 5, 10}, [ControllerSpec, ServerSpec]}}.
