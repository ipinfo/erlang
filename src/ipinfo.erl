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
-define(DEFAULT_BASE_URL, <<"https://ipinfo.io">>).
-define(DEFAULT_TIMEOUT, timer:seconds(5)).
-define(DEFAULT_CACHE_TTL_SECONDS, (24 * 60 * 60)).

-export_type([t/0]).

-type t() :: #{
    '__struct__' := ?MODULE,
    access_token := nil | binary(),
    base_url     := nil | binary(),
    timeout      := nil | timeout(),
    cache        := nil | pid(),
    countries    := map(),
    eu_countries := list()
}.

-spec new() -> t().
%% @private
new() ->
    #{
        '__struct__' => ?MODULE,
        access_token => nil,
        base_url     => nil,
        timeout      => nil,
        cache        => nil,
        countries    => #{},
        eu_countries => []
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
    BaseUrl = get_config(base_url, Settings, ?DEFAULT_BASE_URL),
    Timeout = get_config(timeout, Settings, ?DEFAULT_TIMEOUT),
    CacheTtl = get_config(cache_ttl, Settings, ?DEFAULT_CACHE_TTL_SECONDS),
    EuCountries = case prepare_countries(EuCountriesFile) of 
        {ok, EuCountriesOutput} ->
            EuCountriesOutput;
        {error, ErrorReason} ->
            {error, ErrorReason}
    end,
    case prepare_countries(CountriesFile) of
        {ok, Countries} ->
            {ok, Cache} = ipinfo_cache:create(CacheTtl),
            {ok, new(#{
                access_token => AccessToken,
                base_url     => BaseUrl,
                timeout      => Timeout,
                cache        => Cache,
                countries    => Countries,
                eu_countries => EuCountries
            })};
        {error, Reason} ->
            {error, Reason}
    end.

details(IpInfo) ->
    details(IpInfo, nil).

details(#{cache := Cache, countries := Countries, eu_countries := EuCountries} = IpInfo, IpAddress) ->
    case ipinfo_cache:get(Cache, IpAddress) of
        {ok, Details} ->
            {ok, put_geo(put_country_name(put_is_eu(Details, EuCountries), Countries))};
        error ->
            case ipinfo_http:request_details(IpInfo, IpAddress) of
                {ok, Details} ->
                    ok = ipinfo_cache:add(Cache, IpAddress, Details),
                    {ok, put_geo(put_country_name(put_is_eu(Details, EuCountries), Countries))};
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

prepare_countries(CountriesFile) ->
    case file:read_file(CountriesFile) of
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
