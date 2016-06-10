.PHONY: all clean compile console types typecheck test
REBAR = ./rebar3

ERL_ARGS = -pa _build/default/lib/*/ebin -noshell

all: compile

clean:
	@$(REBAR) clean
	@rm -f erl_crash.dump

compile: src/*.erl
	@$(REBAR) compile

console:
	@$(REBAR) shell

types: compile
	@typer src/*.erl

typecheck:
	@$(REBAR) dialyzer

test:
	@$(REBAR) eunit

cover:
	@$(REBAR) cover
	@open _build/test/cover/index.html

src/*.erl:
	$(error Nothing to compile)
