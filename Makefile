REBAR := rebar

all: deps compile

deps:
	$(REBAR) -v get-deps

compile: deps
	$(REBAR) -v compile

test: compile
	$(REBAR) -v skip_deps=true eunit
