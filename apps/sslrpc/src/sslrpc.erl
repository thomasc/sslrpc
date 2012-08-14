-module(sslrpc).
-behaviour(gen_server).

%% API
-export([start/0, start_link/0, stop/0, stop/1,
         add_host/1, add_host/2, call/4, get_auth/2]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {
    hosts = [] :: list(),
    reqs = [] :: list(),
    buffs = [] :: list(),
    authmod = ?MODULE :: module()
}).

start() ->
    gen_server:start({local,?MODULE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

stop() ->
    stop(?MODULE).

stop(SslRpc) ->
    gen_server:call(SslRpc, stop, infinity).

add_host(Hostname) ->
    add_host(Hostname, []).
add_host(Hostname, Opts) ->
    gen_server:call(?MODULE, {add_host, Hostname, Opts}, infinity).

call(Target, Mod, Fun, Args) ->
    case gen_server:call(?MODULE, {call, Target, Mod, Fun, Args}) of
        {error, unknown_host} ->
            case add_host(Target) of
                ok ->
                    call(Target, Mod, Fun, Args);
                E -> E
            end;
        E -> E
    end.

init(Args) ->
    Mod = proplists:get_value(authmod, Args, ?MODULE),
    {ok, #state{authmod=Mod}}.

get_auth(_Hostname, _Port) ->
    erlang:get_cookie().

default_port() ->
    case application:get_env(port) of
        {ok, Port} -> Port;
        _ -> 8443
    end.

handle_call({add_host, Hostname, Opts}, To,
             #state{reqs=Rs,buffs=Bs,authmod=Am}=S) ->
    Port = proplists:get_value(port, Opts, default_port()),
    Auth = proplists:get_value(auth_token, Opts, Am:get_auth(Hostname, Port)),
    case ssl:connect(Hostname, Port, []) of
        {ok, Socket} ->
            case ssl:send(Socket, term_to_binary({sslrpc, {auth, Auth}})) of
                ok ->
                    R = {Socket, To, Hostname, Port, Auth, auth},
                    B = {Socket, <<>>},
                    {noreply, S#state{reqs=[R|Rs],buffs=[B|Bs]}};
                E ->
                    ssl:close(Socket),
                    {reply, E, S}
            end;
        E -> {reply, E, S}
    end;
handle_call({call, Host, M, F, A}, To, #state{reqs=Rs,hosts=Hs}=S) ->
    case lists:keyfind(Host, 1, Hs) of
        {Host, P, Auth, Socket} ->
            case ssl:send(Socket, term_to_binary({sslrpc, {call, M, F, A}})) of
                ok ->
                    R = {Socket, To, Host, P, Auth},
                    {noreply, S#state{reqs=[R|Rs]}};
                E ->
                    {reply, E, S}
            end;
         _ ->  {reply, {error, unknown_host}, S}
    end;
handle_call(stop, _To, S) ->
    {stop, normal, stopped, S};
handle_call(_, _To, S) ->
    {noreply, S}.

handle_cast(_, S) ->
    {noreply, S}.

handle_info({ssl, Socket, Data}, #state{buffs=Bs}=S) ->
    case lists:keyfind(Socket, 1, Bs) of
        {Socket, B} ->
            D = case Data of
                D2 when is_list(D2) -> list_to_binary(D2);
                _ -> Data
            end,
            B2 = <<B/binary, D/binary>>,
            case try_parse(B2) of
                more ->
                    Bs2 = lists:keyreplace(Socket, 1, Bs, {Socket, B2}),
                    {noreply, S#state{buffs=Bs2}};
                {error, _} = E ->
                    {noreply, handle_error(Socket, E, S)};
                R ->
                    {noreply, handle_reply(Socket, R, S)}
            end;
        false ->
            {noreply, S}
    end;
handle_info({ssl_closed, Socket}, S) ->
    {noreply, handle_error(Socket, {error, ssl_closed}, S)};
handle_info({ssl_error, Socket, _Reason}, S) ->
    {noreply, handle_error(Socket, {error, ssl_error}, S)};
handle_info(_, S) ->
    {noreply, S}.

terminate(_, _S) ->
    ok.

code_change(_, S, _) ->
    {ok, S}.

handle_error(Socket, Error, #state{reqs=Rs,hosts=Hs,buffs=Bs}=S) ->
    case lists:keyfind(Socket, 1, Rs) of
        {Socket, To, _H, _P, _A, auth} ->
            gen_server:reply(To, Error),
            Bs2 = lists:keydelete(Socket, 1, Bs),
            Rs2 = lists:keydelete(To, 2, Rs),
            S#state{reqs=Rs2,buffs=Bs2};
        {Socket, To, H, _P, _A} ->
            gen_server:reply(To, Error),
            Bs2 = lists:keydelete(Socket, 1, Bs),
            Rs2 = lists:keydelete(To, 2, Rs),
            Hs2 = lists:keydelete(H, 1, Hs),
            S#state{reqs=Rs2,buffs=Bs2,hosts=Hs2};
        false ->
            S
    end.

handle_reply(Socket, R, #state{reqs=Rs,buffs=Bs,hosts=Hs}=S) ->
    case lists:keyfind(Socket, 1, Rs) of
        {Socket, To, H, P, A, auth} ->
            case R of
                authenticated ->
                    gen_server:reply(To, ok),
                    Rs2 = lists:keydelete(To, 2, Rs),
                    Bs2 = lists:keyreplace(Socket, 1, Bs, {Socket, <<>>}),
                    Hs2 = [{H,P,A,Socket}|Hs],
                    S#state{reqs=Rs2,buffs=Bs2,hosts=Hs2};
                E ->
                    gen_server:reply(To, E),
                    Rs2 = lists:keydelete(To, 2, Rs),
                    Bs2 = lists:keyreplace(Socket, 1, Bs, {Socket, <<>>}),
                    S#state{reqs=Rs2,buffs=Bs2}
            end;
        {Socket, To, _H, _P, _A} ->
            gen_server:reply(To, R),
            Rs2 = lists:keydelete(To, 2, Rs),
            Bs2 = lists:keyreplace(Socket, 1, Bs, {Socket, <<>>}),
            S#state{reqs=Rs2,buffs=Bs2};
        _ -> S
    end.

try_parse(B) ->
    try binary_to_term(B) of
        {sslrpc, R} -> R;
        _ -> {error, invalid_format}
    catch error:badarg ->
        more
end.

