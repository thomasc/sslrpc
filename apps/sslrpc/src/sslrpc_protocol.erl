-module(sslrpc_protocol).
-behaviour(cowboy_protocol).

-export([start_link/4]).
-export([init/4, get_auth_token/0]).

-record(state, {
    listener :: pid(),
    socket :: inet:socket(),
    transport :: module(),
    buffer = <<>> :: binary(),
    b2t_opts = [safe],
    authmod = ?MODULE :: module()

}).

start_link(ListenerPid, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
    {ok, Pid}.

init(ListenerPid, Socket, Transport, Opts) ->
    AuthMod = proplists:get_value(authmod, Opts, ?MODULE),
    ok = cowboy:accept_ack(ListenerPid),
    wait_request(#state{listener=ListenerPid,socket=Socket,
                        transport=Transport,authmod=AuthMod}).

get_auth_token() ->
    erlang:get_cookie().

wait_request(#state{transport=Transport,socket=Socket,
                    buffer=Buffer,b2t_opts=Opts}=State) ->
    case Transport:recv(Socket, 0, infinity) of
        {ok, Data} ->
            B = <<Buffer/binary, Data/binary>>,
            try binary_to_term(B, Opts) of
                {sslrpc, Request} -> handle_request(Request, State);
                R ->
                    error_logger:error_msg("invalid_request: ~p\n", [R]),
                    terminate(State)
            catch error:badarg ->
                wait_request(State#state{buffer=B})
            end;
        {error, Reason} ->
            error_logger:error_msg("Socket recv error: ~p\n", [Reason]),
            terminate(State)
    end.

handle_request({auth, Auth}, #state{authmod=Am}=S) ->
    case Am:get_auth_token() of
        Auth ->
            do_reply(authenticated, S#state{b2t_opts=[]});
        _ ->
            do_reply({error, authentication_error}, S)
    end;
handle_request({call, _Mod, _Fun, _Args}, #state{b2t_opts=[safe]}=S) ->
    do_reply({error, unauthenticated}, S);
handle_request({call, Mod, Fun, Args}, #state{b2t_opts=[]}=S) ->
    do_reply(apply(Mod, Fun, Args), S);
handle_request(_, S) ->
    do_reply({error, uninmplemented}, S).

do_reply(R, #state{transport=Transport,socket=Socket}=S) ->
    Transport:send(Socket, term_to_binary({sslrpc, R})),
    wait_request(S#state{buffer=(<<>>)}).

terminate(#state{transport=Transport,socket=Socket}) ->
    Transport:close(Socket),
    ok.
