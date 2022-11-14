-module(ipinfo).
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
-define(DEFAULT_BASE_URL, <<"https://ipinfo.io">>).
-define(DEFAULT_TIMEOUT, timer:seconds(5)).
-define(DEFAULT_CACHE_TTL_SECONDS, (24 * 60 * 60)).

-export_type([t/0]).

-type t() :: #{
    '__struct__'         := ?MODULE,
    access_token         := nil | binary(),
    base_url             := nil | binary(),
    timeout              := nil | timeout(),
    cache                := nil | pid(),
    countries            := map(),
    countries_flags      := map(),
    countries_currencies := map(),
    continents           := map(),
    eu_countries         := list()
}.

-spec new() -> t().
%% @private
new() ->
    #{
        '__struct__'         => ?MODULE,
        access_token         => nil,
        base_url             => nil,
        timeout              => nil,
        cache                => nil,
        countries            => #{},
        countries_currencies => #{},
        countries_flags      => #{},
        continents           => #{},
        eu_countries         => []
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
    BaseUrl = get_config(base_url, Settings, ?DEFAULT_BASE_URL),
    Timeout = get_config(timeout, Settings, ?DEFAULT_TIMEOUT),
    CacheTtl = get_config(cache_ttl, Settings, ?DEFAULT_CACHE_TTL_SECONDS),
    case read_json(CountriesFile) of
        {ok, Countries} ->
            case read_json(EuCountriesFile) of
                {ok, EuCountries} ->
                    case read_json(CountriesFlagsFile) of 
                        {ok, CountriesFlags} ->
                            case read_json(CountriesCurrenciesFile) of
                                {ok, CountriesCurrencies} ->
                                    case read_json(ContinentsFile) of 
                                        {ok, Continents} ->
                                            {ok, Cache} = ipinfo_cache:create(CacheTtl),
                                            {ok, new(#{
                                                access_token         => AccessToken,
                                                base_url             => BaseUrl,
                                                timeout              => Timeout,
                                                cache                => Cache,
                                                countries            => Countries,
                                                eu_countries         => EuCountries,
                                                countries_flags      => CountriesFlags,
                                                countries_currencies => CountriesCurrencies,
                                                continents           => Continents
                                            })};
                                        {error, Error} ->
                                            {error, Error}
                                        end;
                                {error, Error} ->
                                    {error, Error}
                                end;
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                        {error, Error}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

details(IpInfo) ->
    details(IpInfo, nil).

details(#{cache := Cache, 
    countries := Countries, 
    eu_countries := EuCountries, 
    countries_flags := CountriesFlags, 
    countries_currencies := CountriesCurrencies,
    continents := Continents
} = IpInfo, IpAddress) ->
    case ipinfo_cache:get(Cache, IpAddress) of
        {ok, Details} ->
            {ok, put_geo(put_country_name(put_is_eu(put_country_flag(put_country_currency(put_continent(Details, Continents),CountriesCurrencies), CountriesFlags), EuCountries), Countries))};
        error ->
            case ipinfo_http:request_details(IpInfo, IpAddress) of
                {ok, Details} ->
                    ok = ipinfo_cache:add(Cache, IpAddress, Details),
                    {ok, put_geo(put_country_name(put_is_eu(put_country_flag(put_country_currency(put_continent(Details, Continents),CountriesCurrencies), CountriesFlags), EuCountries), Countries))};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

put_country_name(#{country := Country} = Details, Countries) ->
    case maps:find(Country, Countries) of
        {ok, CountryName} ->
            maps:put(country_name, CountryName, Details);
        error ->
            Details
    end;
put_country_name(Details, _Countries) ->
    Details.

put_country_flag(#{country := Country} = Details, CountriesFlags) ->
    case maps:find(Country, CountriesFlags) of
        {ok, CountryFlag} ->
            maps:put(country_flag, CountryFlag, Details);
        error ->
            Details
    end;
put_country_flag(Details, _CountriesFlags) ->
    Details.

put_country_currency(#{country := Country} = Details, CountriesCurrencies) ->
    case maps:find(Country, CountriesCurrencies) of
        {ok, CountryCurrency} ->
            maps:put(country_currency, CountryCurrency, Details);
        error ->
            Details
    end;
put_country_currency(Details, _CountriesCurrencies) ->
    Details.

put_continent(#{country := Country} = Details, Continents) ->
    case maps:find(Country, Continents) of
        {ok, Continent} ->
            maps:put(continent, Continent, Details);
        error ->
            Details
    end;
put_continent(Details, _Continents) ->
    Details.

put_is_eu(#{country := Country} = Details, EuCountries) ->
    case lists:member(Country, EuCountries) of
        true ->
            maps:put(is_eu, true, Details);
        false ->
            maps:put(is_eu, false, Details)
    end;
put_is_eu(Details, _EuCountries) ->
    Details.

put_geo(#{loc := Loc} = Details) ->
    [Lat, Lon] = string:split(Loc, <<",">>),
    maps:merge(Details, #{
        latitude => Lat,
        longitude => Lon
    });
put_geo(Details) ->
    Details.

get_config(Key, Settings, Default) ->
    proplists:get_value(Key, Settings, application:get_env(ipinfo, Key, Default)).

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
