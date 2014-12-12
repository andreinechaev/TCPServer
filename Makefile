all:
	test -d deps || rebar get-deps
	rebar compile
	@erl +debug_info -name auth -mnesia dir '"./tmp/mnesia_store"' -pa './ebin' -pa './deps/mochiweb/ebin' -pa './deps/jiffy/ebin' -s ikwit_server start