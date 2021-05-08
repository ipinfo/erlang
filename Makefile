.PHONY: test
.DEFAULT_GOAL := all

all: elvis test dializer

test:
	@rebar3 ct

lint: elvis dialyzer

elvis:
	@rebar3 lint

dializer:
	@rebar3 dialyzer
