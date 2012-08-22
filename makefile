SHELL := /bin/bash

build: tags
	rebar compile

test: tags
	for app in lib/*; do cd $$app; test/init.sh; cd -; done
	ERL_LIBS=lib/:deps/ rebar eunit skip_deps=true

check: build
	dialyzer -r lib/**/src/ --src --no_check_plt

type:
	typer lib/**/src/

shell: build data
	erl -env ERL_LIBS "lib/:deps/" -mnesia dir "data" +A 4 -eval "startup:all()."

tags: 
	@ctags -R -f .tags --exclude="ebin|\.test|\.eunit" lib/

data:
	mkdir data

plt:
	dialyzer --build_plt --apps kernel erts stdlib mnesia crypto

deps:
	rebar get-deps

clean: 
	rebar clean
	rm -rf data
