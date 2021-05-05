-module(ipinfo).
-include_lib("kernel/include/logger.hrl").

-export([
    details/0,
    details/1
]).

-define(DEFAULT_BASE_URL, <<"https://ipinfo.io">>).

details() ->
    #{}.

details(Ip) ->
    BaseUrl = application:get_env(ipinfo, base_url, ?DEFAULT_BASE_URL),
    Url = <<BaseUrl/binary, "/", Ip/binary>>,
    %% TODO: Detect app version programmatically
    ReqHeaders = [
        {"User-Agent", "IPinfoClient/Erlang/0.1.0"},
        {"Accept", "application/json"}
    ],
    Request = {Url, ReqHeaders},
    %% TODO: Get timeout from config
    HTTPOptions = [
        {timeout, timer:seconds(5)}
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
