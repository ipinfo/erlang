-module(ipinfo_lite_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    details_ip_v4_test/1,
    details_ip_v6_test/1,
    details_current_ip_test/1
]).

all() ->
    [
        details_ip_v4_test,
        details_ip_v6_test,
        details_current_ip_test
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ipinfo),
    Token =
        case os:getenv("IPINFO_TOKEN") of
            false ->
                nil;
            TokenStr ->
                list_to_binary(TokenStr)
        end,
    [{token, Token} | Config].

end_per_suite(_Config) ->
    ok = application:stop(ipinfo).

details_ip_v4_test(Config) ->
    Token = proplists:get_value(token, Config),
    {ok, IpInfoLite} = ipinfo_lite:create(Token),
    {ok, Details} = ipinfo_lite:details(IpInfoLite, <<"8.8.8.8">>),
    ?assertEqual(<<"8.8.8.8">>, maps:get(ip, Details)),
    ?assertEqual(<<"US">>, maps:get(country_code, Details)),
    ?assertEqual(<<"United States">>, maps:get(country, Details)),
    ?assertEqual(false, maps:get(is_eu, Details)),
    ?assertEqual(<<"https://cdn.ipinfo.io/static/images/countries-flags/US.svg">>, maps:get(country_flag_url, Details)),
    ?assertEqual(<<"North America">>, maps:get(continent, Details)),
    ?assertEqual(<<"NA">>, maps:get(continent_code, Details)),
    ?assertEqual(<<"AS15169">>, maps:get(asn, Details)),
    ?assertEqual(<<"google.com">>, maps:get(<<"as_domain">>, Details)),
    ?assertEqual(<<"Google LLC">>, maps:get(<<"as_name">>, Details)).

details_ip_v6_test(Config) ->
    Token = proplists:get_value(token, Config),
    {ok, IpInfoLite} = ipinfo_lite:create(Token),
    {ok, Details} = ipinfo_lite:details(IpInfoLite, <<"2001:4860:4860::8888">>),
    ?assertEqual(<<"2001:4860:4860::8888">>, maps:get(ip, Details)),
    ?assertEqual(<<"US">>, maps:get(country_code, Details)),
    ?assertEqual(<<"United States">>, maps:get(country, Details)),
    ?assertEqual(false, maps:get(is_eu, Details)),
    ?assertEqual(<<"https://cdn.ipinfo.io/static/images/countries-flags/US.svg">>, maps:get(country_flag_url, Details)),
    ?assertEqual(<<"North America">>, maps:get(continent, Details)),
    ?assertEqual(<<"NA">>, maps:get(continent_code, Details)),
    ?assertEqual(<<"AS15169">>, maps:get(asn, Details)),
    ?assertEqual(<<"google.com">>, maps:get(<<"as_domain">>, Details)),
    ?assertEqual(<<"Google LLC">>, maps:get(<<"as_name">>, Details)).

details_current_ip_test(Config) ->
    Token = proplists:get_value(token, Config),
    {ok, IpInfoLite} = ipinfo_lite:create(Token),
    {ok, Details} = ipinfo_lite:details(IpInfoLite),
    ?assertNotEqual(nil, maps:get(ip, Details)),
    ?assertNotEqual(nil, maps:get(is_eu, Details)),
    ?assertNotEqual(nil, maps:get(country_flag_url, Details)),
    ?assertNotEqual(nil, maps:get(country, Details)),
    ?assertNotEqual(nil, maps:get(continent, Details)),
    ?assertNotEqual(nil, maps:get(<<"as_domain">>, Details)),
    ?assertNotEqual(nil, maps:get(<<"as_name">>, Details)),
    ?assertNotEqual(nil, maps:get(asn, Details)),
    ?assertNotEqual(nil, maps:get(continent_code, Details)),
    ?assertNotEqual(nil, maps:get(country_code, Details)).
