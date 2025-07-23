-module(ipinfo_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    details_test/1
]).

all() ->
    [details_test].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ipinfo),
    Token = case os:getenv("IPINFO_TOKEN") of
        false -> nil;
        TokenStr -> list_to_binary(TokenStr)
    end,
    [{token, Token} | Config].

end_per_suite(_Config) ->
    ok = application:stop(ipinfo).

details_test(Config) ->
    Token = proplists:get_value(token, Config),
    {ok, IpInfo} = ipinfo:create(Token),
    {ok, Details}= ipinfo:details(IpInfo, <<"8.8.8.8">>),
    ?assertEqual(<<"Mountain View">>, maps:get(city, Details)),
    ?assertEqual(#{
        <<"code">> => <<"NA">>,
        <<"name">> => <<"North America">>
    }, maps:get(continent, Details)),
    ?assertEqual(<<"US">>, maps:get(country, Details)),
    ?assertEqual(#{
        <<"code">> => <<"USD">>,
        <<"symbol">> => <<"$">>
    }, maps:get(country_currency, Details)),
    ?assertEqual(#{
        <<"emoji">> => <<240,159,135,186,240,159,135,184>>,
        <<"unicode">> => <<"U+1F1FA U+1F1F8">>
    }, maps:get(country_flag, Details)),
    ?assertEqual(<<"https:/cdn.ipinfo.io/static/images/countries-flags/US.svg">>, maps:get(country_flag_url, Details)),
    ?assertEqual(<<"United States">>, maps:get(country_name, Details)),
    ?assertEqual(<<"dns.google">>, maps:get(hostname, Details)),
    ?assertEqual(<<"8.8.8.8">>, maps:get(ip, Details)),
    ?assertEqual(false, maps:get(is_anonymous, Details)),
    ?assertEqual(false, maps:get(is_eu, Details)),
    ?assertEqual(<<"38.0088">>, maps:get(latitude, Details)),
    ?assertEqual(<<"38.0088,-122.1175">>, maps:get(loc, Details)),
    ?assertEqual(<<"-122.1175">>, maps:get(longitude, Details)),
    ?assertEqual(<<"AS15169 Google LLC">>, maps:get(org, Details)),
    ?assertEqual(<<"94043">>, maps:get(postal, Details)),
    ?assertEqual(<<"California">>, maps:get(region, Details)),
    ?assertEqual(<<"America/Los_Angeles">>, maps:get(timezone, Details)),
    ?assertEqual(#{ 
        address => <<"US, CA, Mountain View, 1600 Amphitheatre Parkway, 94043">>,
        country => <<"US">>,
        email => <<"network-abuse@google.com">>,
        name => <<"Abuse">>,<<"network">> => <<"8.8.8.0/24">>,
        <<"phone">> => <<"+1-650-253-0000">>
    }, maps:get(<<"abuse">>, Details)),
    ?assertEqual(#{
        domain => <<"google.com">>,
        name => <<"Google LLC">>,
        type => <<"hosting">>,
        <<"asn">> => <<"AS15169">>,
        <<"route">> => <<"8.8.8.0/24">>
    }, maps:get(<<"asn">>, Details)),
    ?assertEqual(#{ 
        domain => <<"google.com">>,
        name => <<"Google LLC">>,
        type => <<"hosting">>
    }, maps:get(<<"company">>, Details)),
    ?assertEqual(true, maps:get(<<"is_anycast">>, Details)),
    ?assertEqual(true, maps:get(<<"is_hosting">>, Details)),
    ?assertEqual(false, maps:get(<<"is_mobile">>, Details)),
    ?assertEqual(false, maps:get(<<"is_satellite">>, Details)),
    ?assertEqual(#{
        proxy => false,
        relay => false,
        service => <<>>,
        <<"hosting">> => true,
        <<"tor">> => false,
        <<"vpn">> => false
    }, maps:get(<<"privacy">>, Details)).