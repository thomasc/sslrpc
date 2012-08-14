-module(sslrpc_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(cowboy),
    application:start(sslrpc).

start(_StartType, _StartArgs) ->
    Client = case application:get_env(client) of
        {ok, true} -> true;
        _ -> false
    end,
    Server = case application:get_env(server) of
        {ok, true} -> start_server();
        _ -> undefined
    end,
    {ok, Pid} = sslrpc_sup:start_link(Client),
    {ok, Pid, Server}.

stop(undefined) ->
    ok;
stop(Server) ->
    cowboy:stop_listener(Server),
    ok.

start_server() ->
    Port = case application:get_env(port) of
        undefined -> 8443;
        {ok, P} -> P
    end,
    CertFile = case application:get_env(certfile) of
        undefined -> filename:join([code:priv_dir(sslrpc), "cert.pem"]);
        {ok, C} -> C
    end,
    KeyFile = case application:get_env(keyfile) of
        undefined -> filename:join([code:priv_dir(sslrpc), "key.pem"]);
        {ok, K} -> K
    end,
    cowboy:start_listener(sslrpc_listener, 100,
        cowboy_ssl_transport, [
            {port, Port},
            {certfile, CertFile},
            {keyfile, KeyFile}],
            sslrpc_protocol, []).

