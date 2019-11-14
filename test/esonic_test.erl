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
