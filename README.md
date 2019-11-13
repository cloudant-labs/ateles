Ateles - gRPC interface to SpiderMonkey
===

This project provides a gRPC interface to SpiderMonkey and includes Erlang
bindings for that interface.

Required Dependencies
---

For macOS:

```shell
$ brew install grpc yasm autoconf@2.13
$ mkdir -p ~/tmp/spidermonkey && cd ~/tmp/spidermonkey
$ wget http://ftp.mozilla.org/pub/firefox/releases/60.3.0esr/source/firefox-60.3.0esr.source.tar.xz
$ tar -xf firefox-60.3.0esr.source.tar.xz
$ cd firefox-60.3.0/js/src
$ mkdir obj
$ cd obj
$ CXXFLAGS=-stdlib=libc++ ../configure \
    --disable-ctypes \
    --disable-ion \
    --disable-jemalloc \
    --enable-optimize \
    --enable-posix-nspr-emulation \
    --enable-hardening \
    --with-system-zlib \
    --with-intl-api
$ make -j4
$ make install
```

or

```
brew tap cloudant/homebrew-dbcore
brew install cloudant/dbcore/mozjs-60
```

For Debian

```shell
$ wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key| apt-key add -
$ echo "deb http://deb.debian.org/debian stretch-backports main" > /etc/apt/sources.list.d/backports.list
$ apt-get update
$ apt-get install -y -t stretch-backports libgrpc++-dev libgrpc++1 libgrpc6 libgrpc-dev protobuf-compiler-grpc
$ apt-get install -y autoconf2.13
$ apt-get install -y yasm
$ mkdir -p ~/tmp/spidermonkey && cd ~/tmp/spidermonkey
$ wget http://ftp.mozilla.org/pub/firefox/releases/60.3.0esr/source/firefox-60.3.0esr.source.tar.xz
$ tar -xf firefox-60.3.0esr.source.tar.xz
$ cd firefox-60.3.0/js/src
$ mkdir obj
$ cd obj
$ ../configure \
    --disable-ctypes \
    --disable-ion \
    --disable-jemalloc \
    --enable-optimize \
    --enable-posix-nspr-emulation \
    --enable-hardening \
    --with-system-zlib \
    --with-intl-api
$ make -j4
$ make install
```


Building
---

```shell
$ make
```


Testing
---
```shell
$ make check
```

## ToDo

- [ ] Implement script timeouts via watchdog threads - see shell/js.cpp
- [ ] Add a configure action to execute for per context timeouts


## Generating Coverage Reports

This is currently only supported on OS X with Clang++.

These are the steps to generate coverage reports:

    $ rm -rf ./build
    $ cmake -DENABLE_COVERAGE=ON -S . -B build/
    $ make check
    $ make coverage
