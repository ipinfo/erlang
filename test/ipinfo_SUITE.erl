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
    {ok, #{
        ip := <<"8.8.8.8">>,
        loc := <<"37.4056,-122.0775">>,
        hostname := <<"dns.google">>,
        city := <<"Mountain View">>,
        region := <<"California">>,
        country := <<"US">>,
        org := <<"AS15169 Google LLC">>,
        postal := <<"94043">>,
        timezone := <<"America/Los_Angeles">>,
        country_name := <<"United States">>,
        latitude := <<"37.4056">>,
        longitude := <<"-122.0775">>,
        is_eu := false,
        country_flag := #{
            <<"emoji">> := <<240,159,135,186,240,159,135,184>>,
            <<"unicode">> := <<"U+1F1FA U+1F1F8">>
        },
        country_currency := #{
            <<"code">> := <<"USD">>,
            <<"symbol">> := <<"$">>
        },
        continent := #{
            <<"code">> := <<"NA">>,
            <<"name">> := <<"North America">>
        },
        country_flag_url := <<"https:/cdn.ipinfo.io/static/images/countries-flags/US.svg">>,
        is_anonymous := false,
        <<"abuse">> := #{
            name := <<"Abuse">>,
            address := <<"US, CA, Mountain View, 1600 Amphitheatre Parkway, 94043">>,
            country := <<"US">>,
            email := <<"network-abuse@google.com">>,
            <<"network">> := <<"8.8.8.0/24">>,
            <<"phone">> := <<"+1-650-253-0000">>
        },
        <<"asn">> := #{
            name := <<"Google LLC">>,
            type := <<"hosting">>,
            domain := <<"google.com">>,
            <<"asn">> := <<"AS15169">>,
            <<"route">> := <<"8.8.8.0/24">>
        },
        <<"company">> := #{
            name := <<"Google LLC">>,
            type := <<"hosting">>,
            domain := <<"google.com">>
        },
        <<"domains">> := #{
            total := 15669,ip := <<"8.8.8.8">>,
            <<"domains">> :=[
                <<"hdchina.org">>,
                <<"musicool.cn">>,
                <<"ztgg.org">>,
                <<"itempurl.com">>,
                <<"authrock.com">>
            ]
        },
        <<"is_anycast">> := true,
        <<"is_hosting">> := true,
        <<"is_mobile">> := false,
        <<"is_satellite">> := false,
        <<"privacy">> := #{
            service := <<>>,
            proxy := false,
            relay := false,
            <<"hosting">> := true,
            <<"tor">> := false,
            <<"vpn">> := false
        }
    }} = ipinfo:details(IpInfo, <<"8.8.8.8">>).
