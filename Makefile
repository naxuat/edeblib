.PHONY: test eunit xref deps

REBAR := $(shell which rebar || echo ./rebar)

all: compile test

erl: compile
	erl -pa ebin deps/*/ebin

compile: deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

test: eunit

eunit: compile
	@$(REBAR) skip_deps=true eunit

xref: compile
	@$(REBAR) xref

clean:
	@$(REBAR) clean
