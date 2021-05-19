-module (ipinfo_cache_SUITE).

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
    add_get_test/1,
    eviction_test/1
]).

-define(KEY, <<"127.0.0.1">>).

all() ->
    [add_get_test, eviction_test].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ipinfo),
    Config.

init_per_testcase(_Testcase, Config) ->
    {ok, Cache} = ipinfo_cache:create(1),
    [{cache, Cache} | Config].

end_per_testcase(_Testcase, _Config) ->
    ok.

end_per_suite(_Config) ->
    application:stop(ipinfo).

add_get_test(Config) ->
    Cache = ?config(cache, Config),
    error = ipinfo_cache:get(Cache, nil),
    error = ipinfo_cache:get(Cache, <<"127.0.0.1">>),
    ok = ipinfo_cache:add(Cache, nil, #{test => <<"Test1">>}),
    ok = ipinfo_cache:add(Cache, ?KEY, #{test => <<"Test2">>}),
    {ok, Val1} = ipinfo_cache:get(Cache, nil),
    {ok, Val2} = ipinfo_cache:get(Cache, ?KEY),
    ?assertMatch(#{test := <<"Test1">>}, Val1),
    ?assertMatch(#{test := <<"Test2">>}, Val2).

eviction_test(Config) ->
    Cache = ?config(cache, Config),
    ok = ipinfo_cache:add(Cache, ?KEY, #{test => <<"Test">>}),
    {ok, _} = ipinfo_cache:get(Cache, ?KEY),
    ok = ct:sleep({seconds, 3}),
    error = ipinfo_cache:get(Cache, ?KEY).
