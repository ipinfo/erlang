-module(ipinfo_resproxy_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    resproxy_test/1,
    resproxy_empty_test/1
]).

all() ->
    [resproxy_test, resproxy_empty_test].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ipinfo),
    Token = case os:getenv("IPINFO_TOKEN") of
        false -> nil;
        TokenStr -> list_to_binary(TokenStr)
    end,
    [{token, Token} | Config].

end_per_suite(_Config) ->
    ok = application:stop(ipinfo).

resproxy_test(Config) ->
    Token = proplists:get_value(token, Config),
    {ok, IpInfo} = ipinfo:create(Token),
    {ok, Resproxy} = ipinfo:resproxy(IpInfo, <<"175.107.211.204">>),
    ?assertEqual(<<"175.107.211.204">>, maps:get(ip, Resproxy)),
    ?assertNotEqual(undefined, maps:get(last_seen, Resproxy)),
    ?assertNotEqual(undefined, maps:get(percent_days_seen, Resproxy)),
    ?assertNotEqual(undefined, maps:get(service, Resproxy)).

resproxy_empty_test(Config) ->
    Token = proplists:get_value(token, Config),
    {ok, IpInfo} = ipinfo:create(Token),
    {ok, Resproxy} = ipinfo:resproxy(IpInfo, <<"8.8.8.8">>),
    ?assertEqual(#{}, Resproxy).
