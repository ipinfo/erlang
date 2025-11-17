-module(ipinfo_plus).
-include_lib("kernel/include/logger.hrl").

-export([
    '__struct__'/0,
    '__struct__'/1
]).

-export([
    create/0,
    create/1,
    create/2,
    details/1,
    details/2
]).

-define(DEFAULT_COUNTRY_FILE, "countries.json").
-define(DEFAULT_EU_COUNTRY_FILE, "eu.json").
-define(DEFAULT_COUNTRY_FLAG_FILE, "flags.json").
-define(DEFAULT_COUNTRY_CURRENCY_FILE, "currency.json").
-define(DEFAULT_CONTINENT_FILE, "continent.json").
-define(DEFAULT_COUNTRY_FLAG_BASE_URL,
    <<"https://cdn.ipinfo.io/static/images/countries-flags/">>).
-define(DEFAULT_BASE_URL, <<"https://api.ipinfo.io/lookup">>).
-define(DEFAULT_TIMEOUT, timer:seconds(5)).
-define(DEFAULT_CACHE_TTL_SECONDS, (24 * 60 * 60)).

-export_type([t/0]).

-type t() :: #{
    '__struct__'          := ?MODULE,
    access_token          := nil | binary(),
    base_url              := nil | binary(),
    timeout               := nil | timeout(),
    cache                 := nil | pid(),
    countries             := map(),
    countries_flags       := map(),
    country_flag_base_url := nil | binary(),
    countries_currencies  := map(),
    continents            := map(),
    eu_countries          := list()
}.

-spec new() -> t().
%% @private
new() ->
    #{
        '__struct__'          => ?MODULE,
        access_token          => nil,
        base_url              => nil,
        timeout               => nil,
        cache                 => nil,
        countries             => #{},
        countries_currencies  => #{},
        countries_flags       => #{},
        country_flag_base_url => nil,
        continents            => #{},
        eu_countries          => []
    }.

-spec '__struct__'() -> t().
%% @private
'__struct__'() ->
    new().

-spec '__struct__'(From :: list() | map()) -> t().
%% @private
'__struct__'(From) ->
    new(From).

-spec new(From :: list() | map()) -> t().
%% @private
new(List) when is_list(List) ->
    new(maps:from_list(List));
new(Map) when is_map(Map) ->
    maps:fold(fun maps:update/3, new(), Map).

create() ->
    create(application:get_env(ipinfo, access_token, nil)).

create(AccessToken) when is_list(AccessToken) ->
    create(list_to_binary(AccessToken));
create(AccessToken) ->
    create(AccessToken, []).

-spec create(AccessToken, Settings) -> Result when
    AccessToken :: binary() | nil,
    Settings    :: proplists:proplist(),
    Result      :: {ok, t()} | {error, term()}.
create(AccessToken, Settings) ->
    CountriesFile = get_config(countries, Settings,
        filename:join(code:priv_dir(ipinfo), ?DEFAULT_COUNTRY_FILE)),
    EuCountriesFile = get_config(eu_countries, Settings,
        filename:join(code:priv_dir(ipinfo), ?DEFAULT_EU_COUNTRY_FILE)),
    CountriesFlagsFile = get_config(countries_flags, Settings,
        filename:join(code:priv_dir(ipinfo), ?DEFAULT_COUNTRY_FLAG_FILE)),
    CountriesCurrenciesFile = get_config(countries_currencies, Settings,
        filename:join(code:priv_dir(ipinfo), ?DEFAULT_COUNTRY_CURRENCY_FILE)),
    ContinentsFile = get_config(continents, Settings,
        filename:join(code:priv_dir(ipinfo), ?DEFAULT_CONTINENT_FILE)),
    CountryFlagBaseUrl = get_config(country_flag_base_url, Settings,
        ?DEFAULT_COUNTRY_FLAG_BASE_URL),
    BaseUrl = get_config(base_url, Settings, ?DEFAULT_BASE_URL),
    Timeout = get_config(timeout, Settings, ?DEFAULT_TIMEOUT),
    CacheTtl = get_config(cache_ttl, Settings, ?DEFAULT_CACHE_TTL_SECONDS),
    create_with_files(AccessToken, BaseUrl, Timeout, CacheTtl, CountriesFile,
        EuCountriesFile, CountriesFlagsFile, CountriesCurrenciesFile,
        ContinentsFile, CountryFlagBaseUrl).

create_with_files(AccessToken, BaseUrl, Timeout, CacheTtl, CountriesFile,
    EuCountriesFile, CountriesFlagsFile, CountriesCurrenciesFile,
    ContinentsFile, CountryFlagBaseUrl) ->
    Files = [
        CountriesFile,
        EuCountriesFile,
        CountriesFlagsFile,
        CountriesCurrenciesFile,
        ContinentsFile
    ],
    case read_json_files(Files) of
        {ok, [Countries, EuCountries, CountriesFlags, CountriesCurrencies, Continents]} ->
            create_ipinfo_plus_struct(
                AccessToken,
                BaseUrl,
                Timeout,
                CacheTtl,
                Countries,
                EuCountries,
                CountriesFlags,
                CountriesCurrencies,
                Continents,
                CountryFlagBaseUrl
            );
        {error, Reason} ->
            {error, Reason}
    end.

read_json_files(Files) ->
    read_json_files(Files, []).

read_json_files([], Acc) ->
    {ok, lists:reverse(Acc)};
read_json_files([File | Rest], Acc) ->
    case read_json(File) of
        {ok, Data} ->
            read_json_files(Rest, [Data | Acc]);
        {error, Reason} ->
            {error, Reason}
    end.

create_ipinfo_plus_struct(AccessToken, BaseUrl, Timeout, CacheTtl, Countries,
    EuCountries, CountriesFlags, CountriesCurrencies, Continents,
    CountryFlagBaseUrl) ->
    {ok, Cache} = ipinfo_cache:create(CacheTtl),
    {ok, new(#{
        access_token          => AccessToken,
        base_url              => BaseUrl,
        timeout               => Timeout,
        cache                 => Cache,
        countries             => Countries,
        eu_countries          => EuCountries,
        countries_flags       => CountriesFlags,
        country_flag_base_url => CountryFlagBaseUrl,
        countries_currencies  => CountriesCurrencies,
        continents            => Continents
    })}.

details(IpInfoPlus) ->
    details(IpInfoPlus, nil).

details(#{cache := Cache,
    countries := Countries,
    eu_countries := EuCountries,
    countries_flags := CountriesFlags,
    country_flag_base_url:= CountryFlagBaseUrl,
    countries_currencies := CountriesCurrencies,
    continents := Continents
} = IpInfo, IpAddress) ->
    ActualIpAddress = case IpAddress of
        nil -> <<"me">>;
        _ -> IpAddress
    end,
    case get_details(Cache, IpInfo, ActualIpAddress) of
        {ok, Details} ->
            {ok, enrich_details(Details, Countries, EuCountries,
                CountriesFlags, CountryFlagBaseUrl, CountriesCurrencies,
                Continents)};
        {error, Reason} ->
            {error, Reason}
    end.

get_details(Cache, IpInfo, IpAddress) ->
    case ipinfo_cache:get(Cache, IpAddress) of
        {ok, Details} ->
            {ok, Details};
        error ->
            case ipinfo_http:request_details(IpInfo, IpAddress) of
                {ok, Details} ->
                    ok = ipinfo_cache:add(Cache, IpAddress, Details),
                    {ok, Details};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

enrich_details(Details, Countries, EuCountries, CountriesFlags,
    CountryFlagBaseUrl, CountriesCurrencies, Continents) ->
    Enrichers = [
        fun enrich_geo/1,
        fun(D) -> put_geo_enrichments(D, Countries, EuCountries, CountriesFlags,
            CountryFlagBaseUrl, CountriesCurrencies, Continents) end
    ],
    lists:foldl(fun(F, Acc) -> F(Acc) end, Details, Enrichers).

enrich_geo(#{geo := Geo} = Details) when is_map(Geo) ->
    Details;
enrich_geo(Details) ->
    Details.

put_geo_enrichments(#{geo := #{country_code := CountryCode} = Geo} = Details,
    Countries, EuCountries, CountriesFlags, CountryFlagBaseUrl,
    CountriesCurrencies, Continents) ->
    EnrichedGeo = Geo#{
        country_name => maps:get(CountryCode, Countries, CountryCode),
        is_eu => lists:member(CountryCode, EuCountries),
        country_flag => maps:get(CountryCode, CountriesFlags, #{}),
        country_currency => maps:get(CountryCode, CountriesCurrencies, #{}),
        continent => maps:get(CountryCode, Continents, #{}),
        country_flag_url => <<CountryFlagBaseUrl/binary, CountryCode/binary, ".svg">>
    },
    Details#{geo => EnrichedGeo};
put_geo_enrichments(Details, _Countries, _EuCountries, _CountriesFlags,
    _CountryFlagBaseUrl, _CountriesCurrencies, _Continents) ->
    Details.

get_config(Key, Settings, Default) ->
    proplists:get_value(Key, Settings,
        application:get_env(ipinfo, Key, Default)).

read_json(JsonFile) ->
    case file:read_file(JsonFile) of
        {ok, Binary} ->
            case jsx:is_json(Binary) of
                true ->
                    {ok, jsx:decode(Binary, [return_maps])};
                false ->
                    {error, invalid_json}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
