# ateles-erlang
 JS worker runtime erlang bindings


## Generating Coverage Reports

This is currently only supported on OS X with Clang++.

These are the steps to generate coverage reports:

    $ rm -rf ./build
    $ cmake -DENABLE_COVERAGE=ON -S . -B build/
    $ make check
    $ make coverage
