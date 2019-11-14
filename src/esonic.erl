-module(esonic).


-behaviour(gen_server).
-behaviour(poolboy_worker).

% Managment API
-export([start_link/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-export([
    ping/1,
    ingest/2,
    query/2
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


handle_call({query, Term}, _From, State) ->
    Query = <<"QUERY shortcuts default \"", Term/binary, "\" LIMIT(40)\n" >>,
    io:format("QUERY: ~p~n", [Query]),
    ok = gen_tcp:send(
        State#state.connection,
        Query
    ),

    {ok, <<"PENDING", QuertId/binary>>} = gen_tcp:recv(State#state.connection, 0),
    io:format("QUERY ID: ~p~n", [QuertId]),

    {ok, <<"EVENT", Result/binary>>} = gen_tcp:recv(State#state.connection, 0),
    io:format("RESULT: ~p~n", [Result]),
    {reply, ok, State}.


handle_cast(join_channel, #state{ mode = Mode, password = Password} = State) ->

    Command = lists:join(" ", ["Start", Mode, Password, "\n"]),
    CommandBin = erlang:iolist_to_binary(Command),
    io:format("COMMAND ~p~n" , [CommandBin]),

    ok = gen_tcp:send(State#state.connection, CommandBin),

    case gen_tcp:recv(State#state.connection, 0) of
        {ok, <<"STARTED ", _/binary>>} ->
            io:format("ESTABLISHED ~n"),
            {noreply, State};
        WTF ->
            io:format("WTF ~p~n", [WTF]),
            {stop, WTF, State}
    end;



    %{ok, Response} = gen_tcp:recv(Connection, 0, 1000),


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
    whatever.

%% ===================================================================
%% Public API
%% ===================================================================

ping(Pid) ->
    gen_server:call(Pid, ping).


ingest(Bucket, Document) ->
    ok.

query(Pid, Term) ->
    gen_server:call(Pid, {query, Term}).


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
