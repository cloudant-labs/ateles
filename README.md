# ateles-erlang
 JS worker runtime erlang bindings


## ToDo

- [x] Add CLI options
- [x] Configurable number of threads via CLI option
- [x] Configurable `max_bytes` value from CLI option
- [x] Configurable listen address to bind
- [ ] Implement script timeouts via watchdog threads - see shell/js.cpp
- [ ] Add a configure action to execute for per context timeouts


## Generating Coverage Reports

This is currently only supported on OS X with Clang++.

These are the steps to generate coverage reports:

    $ rm -rf ./build
    $ cmake -DENABLE_COVERAGE=ON -S . -B build/
    $ make check
    $ make coverage
