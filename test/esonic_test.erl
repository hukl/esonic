-module(esonic_test).

-compile(export_all).

% Include etest's assertion macros.
-include_lib("etest/include/etest.hrl").


-define(CONTROL_ARGS, [
        {host,     "localhost"},
        {port,     1491},
        {password, "SecretPassword"},
        {mode,     "control"}
    ]).


-define(INGEST_ARGS, [
        {host,     "localhost"},
        {port,     1491},
        {password, "SecretPassword"},
        {mode,     "ingest"}
    ]).


-define(SEARCH_ARGS, [
        {host,     "localhost"},
        {port,     1491},
        {password, "SecretPassword"},
        {mode,     "search"}
    ]).


test_establishing_a_connection() ->
    {ok, SonicPid} = esonic:start_link(?CONTROL_ARGS),
    ?assert_equal({ok, <<"PONG">>}, esonic:ping(SonicPid)).


test_ingesting_without_specifying_a_locale() ->
    {ok, SonicPid} = esonic:start_link(?INGEST_ARGS),

    Collection = <<"user_1">>,
    Bucket     = <<"default">>,
    ObjectId   = <<"1">>,
    Title      = <<"What a Beautiful Title!">>,
    Body       = <<"It wasn't meant to be as far as I can see.">>,
    Content    = erlang:iolist_to_binary(
        lists:join(<<" ">>, [Title, Body])
    ),

    ?assert_equal(ok, esonic:push(SonicPid, Collection, Bucket, ObjectId, Content)).


test_basic_query() ->
    {ok, SonicPid}  = esonic:start_link(?CONTROL_ARGS),
    {ok, IngestPid}   = esonic:start_link(?INGEST_ARGS),
    {ok, SearchPid} = esonic:start_link(?SEARCH_ARGS),

    Collection = <<"user_1">>,
    Bucket     = <<"default">>,
    ObjectId   = <<"42">>,
    Term       = <<"trees">>,
    Title      = <<"Stop Building Rockets">>,
    Body       = <<"Start planting trees">>,
    Content    = erlang:iolist_to_binary(
        lists:join(<<" ">>, [Title, Body])
    ),

    {ok, <<"1">>} = esonic:flushc(IngestPid, Collection),
    ok = esonic:trigger(SonicPid, consolidate),

    ?assert_equal({ok, <<"0">>}, esonic:count(IngestPid, Collection)),

    ok = esonic:push(IngestPid, Collection, Bucket, ObjectId, Content),
    ok = esonic:trigger(SonicPid, consolidate),

    ?assert_equal({ok, <<"1">>}, esonic:count(IngestPid, Collection)),
    ?assert_equal({ok, <<"42">>}, esonic:query(SearchPid, Collection, Bucket, Term)).


test_consolidate() ->
    {ok, SonicPid} = esonic:start_link(?CONTROL_ARGS),

    ?assert_equal(ok, esonic:trigger(SonicPid, consolidate)).


test_count_collection() ->
    {ok, IngestPid}   = esonic:start_link(?INGEST_ARGS),

    Collection = <<"random">>,

    ?assert_equal({ok, <<"0">>}, esonic:count(IngestPid, Collection)).


test_flushing_a_collection() ->
    {ok, IngestPid}   = esonic:start_link(?INGEST_ARGS),

    Collection = <<"random_42">>,

    ?assert_equal({ok, <<"0">>}, esonic:flushc(IngestPid, Collection)).

