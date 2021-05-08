-module(ipinfo).
-include_lib("kernel/include/logger.hrl").

-export([
    details/0,
    details/1
]).

-define(DEFAULT_BASE_URL, <<"https://ipinfo.io">>).
-define(DEFAULT_TIMEOUT, timer:seconds(5)).

details() ->
    #{}.

details(Ip) ->
    BaseUrl = application:get_env(ipinfo, base_url, ?DEFAULT_BASE_URL),
    Timeout = application:get_env(ipinfo, timeout, ?DEFAULT_TIMEOUT),
    Url = <<BaseUrl/binary, "/", Ip/binary>>,
    ReqHeaders = [
        {"User-Agent", user_agent()},
        {"Accept", "application/json"}
    ],
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
        {ok, {{_, Status, _}, _RespHeaders, _Body}} ->
            ?LOG_INFO("Unexpected status: ~p", [Status]),
            {error, unexpected_status};
        {error, Reason} ->
            ?LOG_ERROR("Error occured: ~p", [Reason]),
            {error, Reason}
    end.

version() ->
    case lists:keyfind(ipinfo, 1, application:which_applications()) of
        {ipinfo, _, Version} ->
            Version;
        false ->
            ""
    end.

user_agent() ->
    "IPinfoClient/Erlang/" ++ version().
