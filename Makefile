.PHONY: test eunit xref

REBAR := $(shell which rebar || echo ./rebar)

all: compile test

compile: deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

test: eunit

eunit: compile
	@$(REBAR) eunit

xref: compile
	@$(REBAR) xref
