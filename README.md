# [<img src="https://ipinfo.io/static/ipinfo-small.svg" alt="IPinfo" width="24"/>](https://ipinfo.io/) IPinfo Erlang Client Library

This is the official Erlang/Elixir client library for the [IPinfo.io](https://ipinfo.io) IP data API, allowing you to lookup your own IP address, or get any of the following details for an IP:

- [IP geolocation data](https://ipinfo.io/ip-geolocation-api) (city, region, country, postal code, latitude and longitude)
- [ASN details](https://ipinfo.io/asn-api) (ISP or network operator, associated domain name, and type, such as business, hosting or company)
- [Company data](https://ipinfo.io/ip-company-api) (the name and domain of the business that uses the IP address)
- [Carrier information](https://ipinfo.io/ip-carrier-api) (the name of the mobile carrier and MNC and MCC for that carrier if the IP is used exclusively for mobile traffic)

Check all the data we have for your IP address [here](https://ipinfo.io/what-is-my-ip).

## Getting Started

You'll need an IPinfo API access token, which you can get by signing up for a free account at [https://ipinfo.io/signup](https://ipinfo.io/signup).

The free plan is limited to 50,000 requests per month, and doesn't include some of the data fields such as IP type and company data. To enable all the data fields and additional request volumes see [https://ipinfo.io/pricing](https://ipinfo.io/pricing).

## Installation

Add this line to your application's rebar.config:

```erlang
{deps, [ipinfo]}.
```

## Usage

```erlang
{ok, IpInfo} = ipinfo:create(<<"29a75d15">>),
{ok, Details} = ipinfo:details(IpInfo),
#{ip := Ip, city := City, loc := Loc} = Details,
%% ...
```

### Details Data

`ipinfo:details/1,2` will return a map that contains all fields
listed in the [IPinfo developerdocs](https://ipinfo.io/developers/responses#full-response)
with a few minor additions. Properties can be accessed directly.

### Configuration

TBD

## For elixir folks

```elixir
alias :ipinfo, as: IPinfo

def current_ip() do
  with {:ok, %IPinfo{} = handler} <- IPinfo.create(),
       {:ok, details} <- IPinfo.details(handler) do
    details.ip
    |> String.to_charlist()
    |> :inet_parse.address()
  end
end
```
