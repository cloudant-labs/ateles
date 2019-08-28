# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

.PHONY: all format init build test coverage clean

all: deps
	@rebar compile


deps:
	@test -d deps || rebar get-deps


format:
	clang-format -style=file -i c_src/*


init:
	@test -f _build/CMakeCache.txt || (mkdir -p _build && cmake -S . -B _build)


compile: init
	@make -C _build
	@mkdir -p priv/
	@cp _build/ateles priv/ateles


generate: src/ateles_client.erl src/ateles_pb.erl


src/ateles_client.erl src/ateles_pb.erl: proto/ateles.proto
	@REBAR_COLOR=none rebar3 grpc gen

eunit: export ERL_AFLAGS = -config $(shell pwd)/test/eunit.config
eunit:
	@rebar eunit


coverage:
	mkdir -p coverage/
	rm -rf coverage/*
	/Library/Developer/CommandLineTools/usr/bin/llvm-profdata merge -sparse default.profraw -o default.profdata
	/Library/Developer/CommandLineTools/usr/bin/llvm-cov show build/ateles -instr-profile=default.profdata -format=html -output-dir=coverage/
	open coverage/index.html


clean:
	rm -rf build
