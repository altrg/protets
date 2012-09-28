all:
	make clean
	make compile

clean:
	rm -rf ebin/*

compile:
	rebar compile
	erl -pa ./ebin -eval "application:start(protets)"