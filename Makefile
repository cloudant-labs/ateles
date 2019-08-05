.PHONY: generate_proto


all: compile


compile:
	rebar3 compile


run:
	rebar3 shell --apps ateles --config ./config/sys.config
	# rebar3 shell --apps ateles


generate_proto:
	rebar3 grpc gen
