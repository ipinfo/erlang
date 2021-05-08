-module(ipinfo_http).
-include_lib("kernel/include/logger.hrl").

-export([request_details/2]).

-define(RATE_LIMIT_MESSAGE, <<
    "To increase your limits, please review our ",
    "paid plans at https://ipinfo.io/pricing"
>>).

-spec request_details(ipinfo:t(), binary()) -> {ok, map()} | {error, term()}.
request_details(#{access_token := AccessToken, base_url := BaseUrl, timeout := Timeout}, Ip) ->
    Url = build_url(BaseUrl, Ip),
    ReqHeaders = build_req_headers(AccessToken),
    Request = {Url, ReqHeaders},
    HTTPOptions = [
        {timeout, Timeout}
    ],
    Options = [
        {body_format, binary}
    ],
    case httpc:request(get, Request, HTTPOptions, Options) of
        {ok, {{_, 200, _}, _RespHeaders, Body}} ->
            Result = jsx:decode(Body, [{labels, attempt_atom}, return_maps]),
            {ok, Result};
        {ok, {{_, 429, _}, _RespHeaders, _Body}} ->
            ?LOG_WARNING(?RATE_LIMIT_MESSAGE),
            {error, rate_limit_error};
        {ok, {{_, Status, _}, _RespHeaders, _Body}} ->
            ?LOG_ERROR("Unexpected status: ~p", [Status]),
            {error, unexpected_status};
        {error, Reason} ->
            ?LOG_ERROR("Error occured: ~p", [Reason]),
            {error, Reason}
    end.

build_url(BaseUrl, Ip) when Ip =:= nil orelse Ip =:= undefined ->
    BaseUrl;
build_url(BaseUrl, Ip) when is_binary(BaseUrl) andalso is_binary(Ip) ->
    <<BaseUrl/binary, "/", Ip/binary>>.

build_req_headers(nil) ->
    base_req_headers();
build_req_headers(AccessToken) when is_binary(AccessToken) ->
    build_req_headers(binary_to_list(AccessToken));
build_req_headers(AccessToken) when is_list(AccessToken) ->
    Authorization = "Bearer " ++ AccessToken,
    [{"Authorization", Authorization} | base_req_headers()].

base_req_headers() ->
    [
        {"User-Agent", user_agent()},
        {"Accept", "application/json"}
    ].

user_agent() ->
    "IPinfoClient/Erlang/" ++ version().

version() ->
    case lists:keyfind(ipinfo, 1, application:which_applications()) of
        {ipinfo, _, Version} ->
            Version;
        false ->
            ""
    end.
