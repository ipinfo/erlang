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

-define(DETAILS, #{
    ip => <<"176.106.253.152">>,
    city => <<"Mytishchi">>,
    region => <<"Moscow Oblast">>,
    country => <<"RU">>,
    loc => <<"55.9116,37.7308">>,
    org => <<"AS57712 NPF SOFTVIDEO Ltd.">>,
    postal => <<"141000">>,
    timezone => <<"Europe/Moscow">>
}).
-define(API_TOKEN, "29a75d15").

all() ->
    [details_test].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ipinfo),
    {ok, _} = bookish_spork:start_server(),
    Config.

end_per_suite(_Config) ->
    ok = bookish_spork:stop_server(),
    ok = application:stop(ipinfo).

details_test(_Config) ->
    ok = bookish_spork:stub_request([200, [], jsx:encode(?DETAILS)]),
    {ok, IpInfo} = ipinfo:create(?API_TOKEN),
    {ok, #{
        country_name := <<"Russia">>,
        latitude     := <<"55.9116">>,
        longitude    := <<"37.7308">>,
        is_eu        := false,
        country_flag := #{
            <<"emoji">>   := <<240,159,135,183,240,159,135,186>>,
            <<"unicode">> := <<"U+1F1F7 U+1F1FA">>
        },
        country_currency := #{
            <<"code">>   := <<"RUB">>,
            <<"symbol">> := <<226,130,189>>
        }

    }} = ipinfo:details(IpInfo, <<"176.106.253.152">>),
    {ok, Request} = bookish_spork:capture_request(),
    ?assertEqual(<<"/176.106.253.152">>, bookish_spork_request:uri(Request)),
    ?assertEqual(<<"Bearer ", ?API_TOKEN>>,
        bookish_spork_request:header(Request, <<"Authorization">>)).
