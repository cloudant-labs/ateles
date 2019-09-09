# ateles-erlang
 JS worker runtime erlang bindings


## ToDo

- [ ] Add CLI options
- [ ] Configurable number of threads via CLI option
- [ ] Configurable `max_bytes` value from CLI option
- [ ] Implement script timeouts via watchdog threads
- [ ] Add a configure action to execute for per context timeouts


## Generating Coverage Reports

This is currently only supported on OS X with Clang++.

These are the steps to generate coverage reports:

    $ rm -rf ./build
    $ cmake -DENABLE_COVERAGE=ON -S . -B build/
    $ make check
    $ make coverage
