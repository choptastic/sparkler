all: get-deps compile

get-deps:
	./rebar3 deps

compile:
	./rebar3 compile
