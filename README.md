# [<img src="https://ipinfo.io/static/ipinfo-small.svg" alt="IPinfo" width="24"/>](https://ipinfo.io/) IPinfo Erlang Client Library

[![Hex.pm Version](https://img.shields.io/hexpm/v/ipinfo.svg)](https://hex.pm/packages/ipinfo)

[![Build Status](https://github.com/ipinfo/erlang/workflows/CI/badge.svg)](https://github.com/ipinfo/erlang/actions)

This is the official Erlang/Elixir client library for the [IPinfo.io](https://ipinfo.io) IP data API, allowing you to look up your own IP address, or get any of the following details for an IP:

- [IP geolocation data](https://ipinfo.io/ip-geolocation-api) (city, region, country, postal code, latitude, and longitude)
- [ASN details](https://ipinfo.io/asn-api) (ISP or network operator, associated domain name, and type, such as business, hosting, or company)
- [Company data](https://ipinfo.io/ip-company-api) (the name and domain of the business that uses the IP address)
- [Carrier information](https://ipinfo.io/ip-carrier-api) (the name of the mobile carrier and MNC and MCC for that carrier if the IP is used exclusively for mobile traffic)

Check all the data we have for your IP address [here](https://ipinfo.io/what-is-my-ip).

## Getting Started

You'll need an IPinfo API access token, which you can get by signing up for a free account at [https://ipinfo.io/signup](https://ipinfo.io/signup).

The free plan is limited to 50,000 requests per month, and doesn't include some of the data fields such as IP type and company data. To enable all the data fields and additional request volumes see [https://ipinfo.io/pricing](https://ipinfo.io/pricing).

⚠️ Note: This library does not currently support our newest free API https://ipinfo.io/lite. If you’d like to use IPinfo Lite, you can call the [endpoint directly](https://ipinfo.io/developers/lite-api) using your preferred HTTP client. Developers are also welcome to contribute support for Lite by submitting a pull request.

## Installation

Add this line to your application's `rebar.config`:

```erlang
{deps, [ipinfo]}.
```

## Usage

```erlang
1> {ok, Ipinfo} = ipinfo:create("TOKEN").
{ok,#{'__struct__' => ipinfo,access_token => <<"TOKEN">>,
      base_url => <<"https://ipinfo.io">>,cache => <0.197.0>,
      continents =>
          #{<<"PF">> =>
                #{<<"code">> => <<"OC">>,<<"name">> => <<"Oceania">>},
            <<"CV">> =>
                #{<<"code">> => <<"AF">>,<<"name">> => <<"Africa">>},
            <<"CG">> =>
                #{<<"code">> => <<"AF">>,<<"name">> => <<"Africa">>},
            <<"KZ">> =>
                #{<<"code">> => <<"AS">>,<<"name">> => <<"Asia">>},
            <<"VI">> =>
                #{<<"code">> => <<"NA">>,<<"name">> => <<"North America">>},
            <<"ML">> =>
                #{<<"code">> => <<"AF">>,<<"name">> => <<"Africa">>},
            <<"AM">> =>
                #{<<"code">> => <<"AS">>,<<"name">> => <<"Asia">>},
            <<"PN">> =>
                #{<<"code">> => <<"OC">>,<<"name">> => <<"Oceania">>},
            <<"FR">> =>
                #{<<"code">> => <<"EU">>,<<"name">> => <<"Europe">>},
            <<"MK">> =>
                #{<<"code">> => <<"EU">>,<<"name">> => <<"Europe">>},
            <<"NE">> =>
                #{<<"code">> => <<"AF">>,<<"name">> => <<"Africa">>},
            <<"ID">> =>
                #{<<"code">> => <<"AS">>,<<"name">> => <<"Asia">>},
            <<"QA">> => 
                #{<<"code">> => <<"AS">>,<<"name">> => <<"Asia">>},
            <<"LC">> =>
                #{<<"code">> => <<"NA">>,<<"name">> => <<"North America">>},
            <<"BR">> =>
                #{<<"code">> => <<"SA">>,<<"name">> => <<"South America">>},
            <<"DJ">> =>
                #{<<"code">> => <<"AF">>,<<"name">> => <<"Africa">>},
            <<"NU">> =>
                #{<<"code">> => <<"OC">>,<<"name">> => <<"Oceania">>},
            <<"BB">> =>
                #{<<"code">> => <<"NA">>,<<"name">> => <<"North America">>},
            <<"NZ">> =>
                #{<<"code">> => <<"OC">>,<<"name">> => <<"Oceania">>},
            <<"MF">> =>
                #{<<"code">> => <<"NA">>,<<"name">> => <<"North America">>},
            <<"AS">> =>
                #{<<"code">> => <<"OC">>,<<"name">> => <<"Oceania">>},
            <<"IL">> =>
                #{<<"code">> => <<"AS">>,<<"name">> => <<"Asia">>},
            <<"GH">> =>
                #{<<"code">> => <<"AF">>,<<"name">> => <<"Africa">>},
            <<"SL">> =>
                #{<<"code">> => <<"AF">>,<<"name">> => <<"Africa">>},
            <<"NI">> =>
                #{<<"code">> => <<"NA">>,<<"name">> => <<"North America">>},
            <<"ZM">> =>
                #{<<"code">> => <<"AF">>,<<"name">> => <<"Africa">>},...},
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
      countries_currencies =>
          #{<<"PF">> =>
                #{<<"code">> => <<"XPF">>,<<"symbol">> => <<226,130,163>>},
            <<"CV">> =>
                #{<<"code">> => <<"CVE">>,<<"symbol">> => <<"$">>},
            <<"CG">> =>
                #{<<"code">> => <<"XAF">>,<<"symbol">> => <<"FCFA">>},
            <<"KZ">> =>
                #{<<"code">> => <<"KZT">>,<<"symbol">> => <<226,130,184>>},
            <<"VI">> =>
                #{<<"code">> => <<"USD">>,<<"symbol">> => <<"$">>},
            <<"ML">> =>
                #{<<"code">> => <<"XOF">>,<<"symbol">> => <<"CFA">>},
            <<"AM">> =>
                #{<<"code">> => <<"AMD">>,<<"symbol">> => <<214,143>>},
            <<"PN">> =>
                #{<<"code">> => <<"NZD">>,<<"symbol">> => <<"$">>},
            <<"FR">> =>
                #{<<"code">> => <<"EUR">>,<<"symbol">> => <<226,130,172>>},
            <<"MK">> =>
                #{<<"code">> => <<"MKD">>,
                  <<"symbol">> => <<208,180,208,181,208,189>>},
            <<"NE">> =>
                #{<<"code">> => <<"XOF">>,<<"symbol">> => <<"CFA">>},
            <<"ID">> =>
                #{<<"code">> => <<"IDR">>,<<"symbol">> => <<"Rp">>},
            <<"QA">> =>
                #{<<"code">> => <<"QAR">>,<<"symbol">> => <<239,183,188>>},
            <<"LC">> =>
                #{<<"code">> => <<"XCD">>,<<"symbol">> => <<"$">>},
            <<"BR">> =>
                #{<<"code">> => <<"BRL">>,<<"symbol">> => <<"R$">>},
            <<"DJ">> =>
                #{<<"code">> => <<"DJF">>,<<"symbol">> => <<"Fdj">>},
            <<"NU">> =>
                #{<<"code">> => <<"NZD">>,<<"symbol">> => <<"$">>},
            <<"BB">> =>
                #{<<"code">> => <<"BBD">>,<<"symbol">> => <<"$">>},
            <<"NZ">> =>
                #{<<"code">> => <<"NZD">>,<<"symbol">> => <<"$">>},
            <<"MF">> =>
                #{<<"code">> => <<"EUR">>,<<"symbol">> => <<226,130,172>>},
            <<"AS">> =>
                #{<<"code">> => <<"USD">>,<<"symbol">> => <<"$">>},
            <<"IL">> =>
                #{<<"code">> => <<"ILS">>,<<"symbol">> => <<226,130,170>>},
            <<"GH">> =>
                #{<<"code">> => <<"GHS">>,
                  <<"symbol">> => <<71,72,226,130,181>>},
            <<"SL">> =>
                #{<<"code">> => <<"SLL">>,<<"symbol">> => <<"Le">>},
            <<"NI">> =>
                #{<<"code">> => <<"NIO">>,<<"symbol">> => <<"C$">>},
            <<"ZM">> =>
                #{<<"code">> => <<"ZMK">>,<<"symbol">> => <<"ZK">>},...},
      countries_flags =>
          #{<<"PF">> =>
                #{<<"emoji">> => <<240,159,135,181,240,159,135,171>>,
                  <<"unicode">> => <<"U+1F1F5 U+1F1EB">>},
            <<"CV">> =>
                #{<<"emoji">> => <<240,159,135,168,240,159,135,187>>,
                  <<"unicode">> => <<"U+1F1E8 U+1F1FB">>}, 
            <<"CG">> =>
                #{<<"emoji">> => <<240,159,135,168,240,159,135,172>>,
                  <<"unicode">> => <<"U+1F1E8 U+1F1EC">>},
            <<"KZ">> =>
                #{<<"emoji">> => <<240,159,135,176,240,159,135,191>>,
                  <<"unicode">> => <<"U+1F1F0 U+1F1FF">>},
            <<"VI">> =>
                #{<<"emoji">> => <<240,159,135,187,240,159,135,174>>,
                  <<"unicode">> => <<"U+1F1FB U+1F1EE">>},
            <<"ML">> =>
                #{<<"emoji">> => <<240,159,135,178,240,159,135,177>>,
                  <<"unicode">> => <<"U+1F1F2 U+1F1F1">>},
            <<"AM">> =>
                #{<<"emoji">> => <<240,159,135,166,240,159,135,178>>,
                  <<"unicode">> => <<"U+1F1E6 U+1F1F2">>},
            <<"PN">> =>
                #{<<"emoji">> => <<240,159,135,181,240,159,135,179>>,
                  <<"unicode">> => <<"U+1F1F5 U+1F1F3">>},
            <<"FR">> =>
                #{<<"emoji">> => <<240,159,135,171,240,159,135,183>>,
                  <<"unicode">> => <<"U+1F1EB U+1F1F7">>},
            <<"MK">> =>
                #{<<"emoji">> => <<240,159,135,178,240,159,135,176>>,
                  <<"unicode">> => <<"U+1F1F2 U+1F1F0">>},
            <<"NE">> =>
                #{<<"emoji">> => <<240,159,135,179,240,159,135,170>>,
                  <<"unicode">> => <<"U+1F1F3 U+1F1EA">>},
            <<"ID">> =>
                #{<<"emoji">> => <<240,159,135,174,240,159,135,169>>,
                  <<"unicode">> => <<"U+1F1EE U+1F1E9">>},
            <<"QA">> =>
                #{<<"emoji">> => <<240,159,135,182,240,159,135,166>>,
                  <<"unicode">> => <<"U+1F1F6 U+1F1E6">>},
            <<"LC">> =>
                #{<<"emoji">> => <<240,159,135,177,240,159,135,168>>,
                  <<"unicode">> => <<"U+1F1F1 U+1F1E8">>},
            <<"BR">> =>
                #{<<"emoji">> => <<240,159,135,167,240,159,135,183>>,
                  <<"unicode">> => <<"U+1F1E7 U+1F1F7">>},
            <<"DJ">> =>
                #{<<"emoji">> => <<240,159,135,169,240,159,135,175>>,
                  <<"unicode">> => <<"U+1F1E9 U+1F1EF">>},
            <<"NU">> =>
                #{<<"emoji">> => <<240,159,135,179,240,159,135,186>>,
                  <<"unicode">> => <<"U+1F1F3 U+1F1FA">>},
            <<"BB">> =>
                #{<<"emoji">> => <<240,159,135,167,240,159,135,167>>,
                  <<"unicode">> => <<"U+1F1E7 U+1F1E7">>},
            <<"NZ">> =>
                #{<<"emoji">> => <<240,159,135,179,240,159,135,191>>,
                  <<"unicode">> => <<"U+1F1F3 U+1F1FF">>},
            <<"MF">> =>
                #{<<"emoji">> => <<240,159,135,178,240,159,135,171>>,
                  <<"unicode">> => <<"U+1F1F2 U+1F1EB">>},
            <<"AS">> =>
                #{<<"emoji">> => <<240,159,135,166,240,159,135,184>>,
                  <<"unicode">> => <<"U+1F1E6 U+1F1F8">>},
            <<"IL">> =>
                #{<<"emoji">> => <<240,159,135,174,240,159,135,177>>,
                  <<"unicode">> => <<"U+1F1EE U+1F1F1">>},
            <<"GH">> =>
                #{<<"emoji">> => <<240,159,135,172,240,159,135,173>>,
                  <<"unicode">> => <<"U+1F1EC U+1F1ED">>},
            <<"SL">> =>
                #{<<"emoji">> => <<240,159,135,184,240,159,135,177>>,
                  <<"unicode">> => <<"U+1F1F8 U+1F1F1">>},
            <<"NI">> =>
                #{<<"emoji">> => <<240,159,135,179,240,159,135,174>>,
                  <<"unicode">> => <<"U+1F1F3 U+1F1EE">>},
            <<"ZM">> =>
                #{<<"emoji">> => <<240,159,135,191,240,159,135,178>>,
                  <<"unicode">> => <<"U+1F1FF U+1F1F2">>},...},
      country_flag_base_url =>
          <<"https://cdn.ipinfo.io/static/images/countries-flags/">>, 
      eu_countries =>
          [<<"IE">>,<<"AT">>,<<"LT">>,<<"LU">>,<<"LV">>,<<"DE">>,
           <<"DK">>,<<"SE">>,<<"SI">>,<<"SK">>,<<"CZ">>,<<"CY">>,
           <<"NL">>,<<"FI">>,<<"FR">>,<<"MT">>,<<"ES">>,<<"IT">>,
           <<"EE">>,<<"PL">>,<<"PT">>,<<"HU">>,<<"HR">>,<<"GR">>,
           <<"RO">>,<<...>>|...],
      timeout => 5000}}
2> {ok, Details} = ipinfo:details(Ipinfo, <<"8.8.8.8">>).
{ok,#{city => <<"Mountain View">>,country => <<"US">>,
      country_currency =>
          #{<<"code">> => <<"USD">>,<<"symbol">> => <<"$">>},
      country_flag =>
          #{<<"emoji">> => <<240,159,135,186,240,159,135,184>>,
            <<"unicode">> => <<"U+1F1FA U+1F1F8">>},
      country_flag_url =>
          <<"https:/cdn.ipinfo.io/static/images/countries-flags/US.svg">>,
      country_name => <<"United States">>,
      hostname => <<"dns.google">>,ip => <<"8.8.8.8">>,
      is_eu => false,latitude => <<"37.4056">>,
      loc => <<"37.4056,-122.0775">>,longitude => <<"-122.0775">>,
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
            type => <<"hosting">>,<<"asn">> => <<"AS15169">>,
            <<"route">> => <<"8.8.8.0/24">>},
      <<"company">> =>
          #{domain => <<"google.com">>,name => <<"Google LLC">>,
            type => <<"hosting">>},
      <<"domains">> =>
          #{ip => <<"8.8.8.8">>,total => 13737,
            <<"domains">> =>
                [<<"bits-hyderabad.ac.in">>,<<"grandpashabet1203.com">>,
                 <<"grandpashabet1204.com">>,<<"itempurl.com">>,
                 <<"apkplz.org">>]},
      <<"postal">> => <<"94043">>,
      <<"privacy">> =>
          #{proxy => false,relay => false,service => <<>>,
            <<"hosting">> => true,<<"tor">> => false,<<"vpn">> => false},
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

There are official [IPinfo client libraries](https://ipinfo.io/developers/libraries) available for many languages including PHP, Go, Java, Ruby, and many popular frameworks such as Django, Rails, and Laravel. There are also many third-party libraries and integrations available for our API.

## About IPinfo

Founded in 2013, IPinfo prides itself on being the most reliable, accurate, and in-depth source of IP address data available anywhere. We process terabytes of data to produce our custom IP geolocation, company, carrier, VPN detection, hosted domains, and IP type data sets. Our API handles over 40 billion requests a month for 100,000 businesses and developers.

[![image](https://avatars3.githubusercontent.com/u/15721521?s=128&u=7bb7dde5c4991335fb234e68a30971944abc6bf3&v=4)](https://ipinfo.io/)
