.PHONY: test eunit xref

REBAR := $(shell which rebar || echo ./rebar)

all: compile test

compile:
	@$(REBAR) compile

test: eunit

eunit: compile
	@$(REBAR) eunit

xref: compile
	@$(REBAR) xref
