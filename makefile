SHELL := /bin/bash

build: tags
	rebar compile

test: tags
	for app in lib/*; do cd $$app; test/init.sh; cd -; done
	rebar eunit

check: build
	dialyzer -r lib/**/src/ --src --no_check_plt

type:
	typer lib/**/src/

shell: build data
	erl -env ERL_LIBS lib/ -eval "mnesia:start()" -mnesia dir "data"

tags: 
	@ctags -R -f .tags --exclude="ebin|\.test|\.eunit" lib/

data:
	mkdir data

init:
	rebar get-deps
	dialyzer --build_plt --apps kernel erts stdlib mnesia crypto

clean: 
	rebar clean
	rm -rf data
