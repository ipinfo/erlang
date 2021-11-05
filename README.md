# [<img src="https://ipinfo.io/static/ipinfo-small.svg" alt="IPinfo" width="24"/>](https://ipinfo.io/) IPinfo Erlang Client Library

[![Hex.pm Version](https://img.shields.io/hexpm/v/ipinfo.svg)](https://hex.pm/packages/ipinfo)

[![Build Status](https://github.com/ipinfo/erlang/workflows/CI/badge.svg)](https://github.com/ipinfo/erlang/actions)

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

Add this line to your application's `rebar.config`:

```erlang
{deps, [ipinfo]}.
```

## Usage

```erlang
1> {ok, Ipinfo} = ipinfo:create("TOKEN").
{ok,#{'__struct__' => ipinfo,access_token => <<"TOKEN">>,
      base_url => <<"https://ipinfo.io">>,cache => <0.225.0>,
      countries =>
          #{<<"PF">> => <<"French Polynesia">>,
            <<"CV">> => <<"Cape Verde">>,
            <<"CG">> => <<"Republic of the Congo">>,
            <<"KZ">> => <<"Kazakhstan">>,
            <<"VI">> => <<"U.S. Virgin Islands">>,
            <<"ML">> => <<"Mali">>,<<"AM">> => <<"Armenia">>,
            <<"PN">> => <<"Pitcairn">>,<<"FR">> => <<"France">>,
            <<"MK">> => <<"Macedonia">>,<<"NE">> => <<"Niger">>,
            <<"ID">> => <<"Indonesia">>,<<"QA">> => <<"Qatar">>,
            <<"LC">> => <<"Saint Lucia">>,<<"BR">> => <<"Brazil">>,
            <<"DJ">> => <<"Djibouti">>,<<"NU">> => <<"Niue">>,
            <<"BB">> => <<"Barbados">>,<<"NZ">> => <<"New Zealand">>,
            <<"MF">> => <<"Saint Martin">>,
            <<"AS">> => <<"American Samoa">>,<<"IL">> => <<"Israel">>,
            <<"GH">> => <<"Ghana">>,<<"SL">> => <<"Sierra Leone">>,
            <<"NI">> => <<"Nicaragua">>,<<"ZM">> => <<"Zambia">>,...},
      timeout => 5000}}
2> {ok, Details} = ipinfo:details(Ipinfo, <<"8.8.8.8">>).
{ok,#{city => <<"Mountain View">>,country => <<"US">>,
      country_name => <<"United States">>,
      hostname => <<"dns.google">>,ip => <<"8.8.8.8">>,
      latitude => <<"37.4056">>,loc => <<"37.4056,-122.0775">>,
      longitude => <<"-122.0775">>,
      org => <<"AS15169 Google LLC">>,
      <<"abuse">> =>
          #{address =>
                <<"US, CA, Mountain View, 1600 Amphitheatre Parkway, 94043">>,
            country => <<"US">>,email => <<"network-abuse@google.com">>,
            name => <<"Abuse">>,<<"network">> => <<"8.8.8.0/24">>,
            <<"phone">> => <<"+1-650-253-0000">>},
      <<"anycast">> => true,
      <<"asn">> =>
          #{domain => <<"google.com">>,name => <<"Google LLC">>,
            type => <<"business">>,<<"asn">> => <<"AS15169">>,
            <<"route">> => <<"8.8.8.0/24">>},
      <<"company">> =>
          #{domain => <<"google.com">>,name => <<"Google LLC">>,
            type => <<"business">>},
      <<"domains">> =>
          #{ip => <<"8.8.8.8">>,total => 12353,
            <<"domains">> =>
                [<<"41.cn">>,<<"itempurl.com">>,<<"ftempurl.com">>,
                 <<"dns.google">>,<<"proxyie.cn">>]},
      <<"postal">> => <<"94043">>,
      <<"privacy">> =>
          #{proxy => false,<<"hosting">> => false,<<"tor">> => false,
            <<"vpn">> => false,<<"relay">> => false},
      <<"region">> => <<"California">>,
      <<"timezone">> => <<"America/Los_Angeles">>}}
```

### Details Data

`ipinfo:details/1,2` will return a map that contains all fields
listed in the [IPinfo developer documentation](https://ipinfo.io/developers/responses#full-response)
with a few minor additions. Properties can be accessed directly.

### Configuration

TBD

## Elixir Usage

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

## Other Libraries

There are official [IPinfo client libraries](https://ipinfo.io/developers/libraries) available for many languages including PHP, Go, Java, Ruby, and many popular frameworks such as Django, Rails and Laravel. There are also many third party libraries and integrations available for our API.

## About IPinfo

Founded in 2013, IPinfo prides itself on being the most reliable, accurate, and in-depth source of IP address data available anywhere. We process terabytes of data to produce our custom IP geolocation, company, carrier, VPN detection, hosted domains, and IP type data sets. Our API handles over 40 billion requests a month for 100,000 businesses and developers.

[![image](https://avatars3.githubusercontent.com/u/15721521?s=128&u=7bb7dde5c4991335fb234e68a30971944abc6bf3&v=4)](https://ipinfo.io/)
