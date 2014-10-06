all:
	test -d deps || rebar get-deps
	rebar compile
	@erl -pa './ebin' -pa './deps/mochiweb/ebin' -pa './deps/jiffy/ebin' -s server start