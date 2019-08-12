.PHONY: generate_proto, run


all: compile


compile:
	rebar3 compile


run:
	rebar3 shell --apps grpcbox --config ./config/sys.config


generate_proto:
	rebar3 grpc gen
