-module(esonic_test).

-compile(export_all).

% Include etest's assertion macros.
-include_lib("etest/include/etest.hrl").


test_establishing_a_connection() ->
    ConnectionArgs = [
        {host,     "localhost"},
        {port,     1491},
        {password, "SecretPassword"},
        {mode,     "control"}
    ],

    {ok, SonicPid} = esonic:start_link(ConnectionArgs),
    ?assert_equal({ok, <<"PONG">>}, esonic:ping(SonicPid)).


test_ingesting_without_specifying_a_locale() ->
    ConnectionArgs = [
        {host,     "localhost"},
        {port,     1491},
        {password, "SecretPassword"},
        {mode,     "ingest"}
    ],

    {ok, SonicPid} = esonic:start_link(ConnectionArgs),

    Collection = <<"user_1">>,
    Bucket     = <<"default">>,
    ObjectId   = <<"1">>,
    Title      = <<"What a Beautiful Title!">>,
    Body       = <<"It wasn't meant to be as far as I can see.">>,
    Content    = erlang:iolist_to_binary(
        lists:join(<<" ">>, [Title, Body])
    ),

    ?assert_equal(ok, esonic:push(SonicPid, Collection, Bucket, ObjectId, Content)).
