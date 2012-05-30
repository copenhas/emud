
build: tags
	rebar compile

tests:
	rebar eunit

check:
	dialyzer -r src --src --no_check_plt

shell: build
	erl -pa ebin -I include

tags:
	ctags -R -f .tags

clean:
	rebar clean
