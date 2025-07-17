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
    {ok, #{
        ip := <<"8.8.8.8">>,
        is_eu := false,
        country_flag_url := <<"https:/cdn.ipinfo.io/static/images/countries-flags/United States.svg">>,
        country := <<"United States">>,
        continent := <<"North America">>,
        <<"as_domain">> := <<"google.com">>,
        <<"as_name">> := <<"Google LLC">>,
        <<"asn">> := <<"AS15169">>,
        <<"continent_code">> := <<"NA">>,
        <<"country_code">> := <<"US">>}
    } = ipinfo_lite:details(IpInfoLite, <<"8.8.8.8">>).

details_ip_v6_test(Config) ->
    Token = proplists:get_value(token, Config),
    {ok, IpInfoLite} = ipinfo_lite:create(Token),
    {ok, #{
        ip := <<"2001:4860:4860::8888">>,
        is_eu := false,
        country_flag_url := <<"https:/cdn.ipinfo.io/static/images/countries-flags/United States.svg">>,
        country := <<"United States">>,
        continent := <<"North America">>,
        <<"as_domain">> := <<"google.com">>,
        <<"as_name">> := <<"Google LLC">>,
        <<"asn">> := <<"AS15169">>,
        <<"continent_code">> := <<"NA">>,
        <<"country_code">> := <<"US">>}
    } = ipinfo_lite:details(IpInfoLite, <<"2001:4860:4860::8888">>).

details_current_ip_test(Config) ->
    Token = proplists:get_value(token, Config),
    {ok, IpInfoLite} = ipinfo_lite:create(Token),
    {ok, #{
        ip := Ip,
        is_eu := IsEu,
        country_flag_url := CountryFlagUrl,
        country := Country,
        continent := Continent,
        <<"as_domain">> := AsDomain,
        <<"as_name">> := AsName,
        <<"asn">> := Asn,
        <<"continent_code">> := ContinentCode,
        <<"country_code">> := CountryCode}
    } = ipinfo_lite:details(IpInfoLite),
    ?assertNotEqual(nil, Ip),
    ?assertNotEqual(nil, IsEu),
    ?assertNotEqual(nil, CountryFlagUrl),
    ?assertNotEqual(nil, Country),
    ?assertNotEqual(nil, Continent),
    ?assertNotEqual(nil, AsDomain),
    ?assertNotEqual(nil, AsName),
    ?assertNotEqual(nil, Asn),
    ?assertNotEqual(nil, ContinentCode),
    ?assertNotEqual(nil, CountryCode).
