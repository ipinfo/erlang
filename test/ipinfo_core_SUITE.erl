-module(ipinfo_core_SUITE).

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
    {ok, IpInfoCore} = ipinfo_core:create(Token),
    {ok, Details} = ipinfo_core:details(IpInfoCore, <<"8.8.8.8">>),

    % Verify required fields
    ?assertEqual(<<"8.8.8.8">>, maps:get(ip, Details)),

    % Check geo object
    #{geo := Geo} = Details,
    ?assertNotEqual(nil, maps:get(city, Geo)),
    ?assertNotEqual(nil, maps:get(region, Geo)),
    ?assertNotEqual(nil, maps:get(region_code, Geo)),
    ?assertNotEqual(nil, maps:get(country, Geo)),
    ?assertNotEqual(nil, maps:get(country_code, Geo)),
    ?assertNotEqual(nil, maps:get(continent, Geo)),
    ?assertNotEqual(nil, maps:get(continent_code, Geo)),
    ?assertNotEqual(nil, maps:get(latitude, Geo)),
    ?assertNotEqual(nil, maps:get(longitude, Geo)),
    ?assertNotEqual(nil, maps:get(timezone, Geo)),
    ?assertNotEqual(nil, maps:get(postal_code, Geo)),
    ?assertNotEqual(nil, maps:get(country_name, Geo)),
    ?assertNotEqual(nil, maps:get(is_eu, Geo)),
    ?assertNotEqual(nil, maps:get(country_flag, Geo)),
    ?assertNotEqual(nil, maps:get(country_currency, Geo)),
    ?assertNotEqual(nil, maps:get(country_flag_url, Geo)),

    % Check as object
    As = maps:get(as, Details),
    ?assertNotEqual(nil, maps:get(asn, As)),
    ?assertNotEqual(nil, maps:get(name, As)),
    ?assertNotEqual(nil, maps:get(domain, As)),
    ?assertNotEqual(nil, maps:get(type, As)),

    % Check network flags
    ?assertNotEqual(nil, maps:get(is_anonymous, Details)),
    ?assertNotEqual(nil, maps:get(is_anycast, Details)),
    ?assertNotEqual(nil, maps:get(is_hosting, Details)),
    ?assertNotEqual(nil, maps:get(is_mobile, Details)),
    ?assertNotEqual(nil, maps:get(is_satellite, Details)).
