-module(ipinfo_resproxy_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    resproxy_test/1,
    resproxy_empty_test/1
]).

all() ->
    [resproxy_test, resproxy_empty_test].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ipinfo),
    {ok, _} = application:ensure_all_started(bookish_spork),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(ipinfo),
    ok = application:stop(bookish_spork).

init_per_testcase(_TestCase, Config) ->
    {ok, _} = bookish_spork:start_server(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok = bookish_spork:stop_server().

resproxy_test(_Config) ->
    % Mock response for resproxy
    MockBody = jsx:encode(#{
        <<"ip">> => <<"175.107.211.204">>,
        <<"last_seen">> => <<"2025-01-20">>,
        <<"percent_days_seen">> => 0.85,
        <<"service">> => <<"example_service">>
    }),
    bookish_spork:stub_request([200, #{}, MockBody]),

    % Create IPinfo client pointing to mock server (default bookish_spork port is 32002)
    {ok, IpInfo} = ipinfo:create(<<"test_token">>, [{base_url, <<"http://127.0.0.1:32002">>}]),
    {ok, Resproxy} = ipinfo:resproxy(IpInfo, <<"175.107.211.204">>),

    ?assertEqual(<<"175.107.211.204">>, maps:get(ip, Resproxy)),
    ?assertEqual(<<"2025-01-20">>, maps:get(last_seen, Resproxy)),
    ?assertEqual(0.85, maps:get(percent_days_seen, Resproxy)),
    ?assertEqual(<<"example_service">>, maps:get(service, Resproxy)).

resproxy_empty_test(_Config) ->
    % Mock empty response for resproxy
    MockBody = jsx:encode(#{}),
    bookish_spork:stub_request([200, #{}, MockBody]),

    % Create IPinfo client pointing to mock server (default bookish_spork port is 32002)
    {ok, IpInfo} = ipinfo:create(<<"test_token">>, [{base_url, <<"http://127.0.0.1:32002">>}]),
    {ok, Resproxy} = ipinfo:resproxy(IpInfo, <<"8.8.8.8">>),

    ?assertEqual(#{}, Resproxy).
