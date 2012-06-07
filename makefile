
build: tags
	rebar compile

tests: build
	@rm -rf .eunit/Mnesia.nonode@nohost
	@mkdir -p .eunit/Mnesia.nonode@nohost
	@escript test/init

	rebar eunit

check: build
	dialyzer -r src --src --no_check_plt

types:
	typer src

shell: build data
	erl -pa ebin -I include -eval "mnesia:start()" -mnesia dir "data"

tags: 
	@ctags -R -f .tags

data:
	mkdir data

clean: 
	rebar clean
