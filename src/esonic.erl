-module(esonic).


-behaviour(gen_server).

% Managment API
-export([start_link/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-export([
    ping/1,
    push/5,
    query/4,
    count/2,
    flushc/2,
    trigger/2
]).


% Example state record
-record(state, {
    connection,
    mode,
    host,
    port,
    password
}).

%% ===================================================================
%% Management API
%% ===================================================================

start_link(Args) ->
    gen_server:start_link( ?MODULE, Args, []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init(Args) ->
    process_flag(trap_exit, true),

    State = #state{
        host     = proplists:get_value(host, Args),
        port     = proplists:get_value(port, Args),
        password = proplists:get_value(password, Args),
        mode     = proplists:get_value(mode, Args)
    },

    {ok, Connection} = connect(State),

    gen_server:cast(self(), join_channel),

    {ok, State#state{ connection = Connection }}.


handle_call(ping, _From, State) ->
    ok = gen_tcp:send(State#state.connection, <<"PING\n">>),

    case gen_tcp:recv(State#state.connection, 0) of
        {ok, <<"PONG\r\n">>} ->
            {reply, {ok, <<"PONG">>}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;


handle_call({query, Collection, Bucket, Term}, _From, State) ->
    Command = to_command([
        <<"QUERY">>,
        Collection,
        Bucket,
        <<"\"", Term/binary, "\"">>
    ]),

    ok = gen_tcp:send(State#state.connection, Command),

    {ok, <<"PENDING ", QueryId/binary>>} = tcp_recv(State#state.connection),
    io:format("QUERY ID: ~p~n", [QueryId]),

    Size = size(QueryId),
    {ok, <<"EVENT QUERY ", QueryId:Size/binary, " ", Result/binary>>} = tcp_recv(State#state.connection),
    io:format("RESULT: ~p~n", [Result]),
    {reply, {ok, Result}, State};


handle_call({push, Collection, Bucket, ObjectId, Content}, _From, State) ->
    Command = to_command([
        <<"PUSH">>,
        Collection,
        Bucket,
        ObjectId,
        <<"\"", Content/binary, "\"">>
    ]),

    ok = gen_tcp:send(State#state.connection, Command),

    case gen_tcp:recv(State#state.connection, 0) of
        {ok, <<"OK", _/binary>>} ->
            io:format("PUSHED ~n"),
            {reply, ok, State};
        WTF ->
            io:format("WTF ~p~n", [WTF]),
            {stop, WTF, State}
    end;


handle_call({count, Collection}, _From, State) ->
    Command = to_command([<<"COUNT">>, Collection]),

    ok = gen_tcp:send(State#state.connection, Command),

    case tcp_recv(State#state.connection) of
        {ok, <<"RESULT ", Count/binary>>} ->
            {reply, {ok, Count}, State};
        Error ->
            {reply, Error, State}
    end;


handle_call({flushc, Collection}, _From, State) ->
    Command = to_command([<<"FLUSHC">>, Collection]),

    ok = gen_tcp:send(State#state.connection, Command),

    case tcp_recv(State#state.connection) of
        {ok, <<"RESULT ", Count/binary>>} ->
            {reply, {ok, Count}, State};
        Error ->
            {reply, Error, State}
    end;


handle_call(consolidate, _From, State) ->
    Command = to_command([<<"TRIGGER">>, <<"CONSOLIDATE">>]),

    ok = gen_tcp:send(State#state.connection, Command),

    case tcp_recv(State#state.connection) of
        {ok, <<"OK">>} ->
            {reply, ok, State};
        Error ->
            {reply, Error, State}
    end.


handle_cast(join_channel, #state{ mode = Mode, password = Password} = State) ->
    Command = to_command(["START", Mode, Password]),

    ok = gen_tcp:send(State#state.connection, Command),

    case gen_tcp:recv(State#state.connection, 0) of
        {ok, <<"STARTED ", _/binary>>} ->
            io:format("ESTABLISHED ~n"),
            {noreply, State};
        WTF ->
            io:format("WTF ~p~n", [WTF]),
            {stop, WTF, State}
    end;


handle_cast(_Msg, State) ->
    {stop, normal, State}.


handle_info({tcp, _Port, <<"PENDING ", _/binary>>}, State) ->
    {noreply, State};

handle_info({tcp, _Port, <<"CONNECTED ", _/binary>>}, State) ->

    {noreply, State};



handle_info(Info, State) ->
    io:format("INFO: ~p~n", [Info]),
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, _State) ->
    io:format("STOPPED~n"),
    shutdown.

%% ===================================================================
%% Public API
%% ===================================================================

ping(Pid) ->
    gen_server:call(Pid, ping).


push(Pid, Collection, Bucket, ObjectId, Content) ->
    gen_server:call(Pid, {push, Collection, Bucket, ObjectId, Content}).


query(Pid, Collection, Bucket, Term) ->
    gen_server:call(Pid, {query, Collection, Bucket, Term}).


count(Pid, Collection) ->
    gen_server:call(Pid, {count, Collection}).


flushc(Pid, Collection) ->
    gen_server:call(Pid, {flushc, Collection}).


trigger(Pid, consolidate) ->
    gen_server:call(Pid, consolidate).


%% ===================================================================
%% Private API
%% ===================================================================


connect(#state{ host = Host, port = Port }) ->

    {ok, Connection} = gen_tcp:connect(
        Host, Port, [{active, false}, {packet, line}, binary], 1000
    ),

    case gen_tcp:recv(Connection, 0) of
        {ok, <<"CONNECTED ", _/binary>>} ->
            io:format("CONNECTED ~n"),
            {ok, Connection};
        {error, Reason} ->
            io:format("Sonic Connection Error: ~p~n", [Reason]),
            {error, Reason}
    end.


tcp_recv(Connection) ->
    case gen_tcp:recv(Connection, 0) of
        {ok, Response} -> {ok, chomp(Response)};
        Error          -> Error
    end.


to_command(CommandList) ->
    CommandWithSpaces = lists:join(<<" ">>, CommandList),

    erlang:iolist_to_binary([CommandWithSpaces, <<"\r\n">>]).


chomp(Bitstring) ->
    [Result|_] = binary:split(Bitstring, <<"\r\n">>),

    Result.
