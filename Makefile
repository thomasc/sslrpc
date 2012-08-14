all: compile

compile: deps
	./rebar compile

deps:
	./rebar get-deps

test:
	./rebar ct skip_deps=true

rel:
	rm -Rf rel/client/client
	rm -Rf rel/server/server
	./rebar generate

clean:
	./rebar clean
.PHONY: test rel
