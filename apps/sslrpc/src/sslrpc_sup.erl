
-module(sslrpc_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Client) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Client]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([true]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(sslrpc, worker)]} };
init([false]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

