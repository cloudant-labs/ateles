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

all: server
	@rebar compile


deps:
	false
	@test -d deps || rebar get-deps


format:
	clang-format -style=file -i c_src/*


init: export PKG_CONFIG_PATH = /usr/local/opt/openssl@1.1/lib/pkgconfig/
init:
	@test -f _build/CMakeCache.txt || (mkdir -p _build && cd _build && cmake ../)


ca.pem:
	@certstrap init --common-name somewhere.over.the.rainbow --passphrase ""
	@certstrap request-cert -ip 127.0.0.1 --passphrase "" -domain localhost
	@certstrap sign localhost --CA somewhere.over.the.rainbow --passphrase ""
	@openssl pkcs8 -topk8 -nocrypt -in out/localhost.key -out key.pem
	@cp out/localhost.crt cert.pem
	@cp out/somewhere.over.the.rainbow.crt ca.pem


server: init ca.pem
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
	/Library/Developer/CommandLineTools/usr/bin/llvm-cov show priv/ateles -instr-profile=default.profdata -format=html -output-dir=coverage/
	open coverage/index.html


clean:
	rm -rf _build
