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

cmake_minimum_required (VERSION 2.6)
project (ateles)

# Configure dependencies
# Eventually we'll want to expand the mozjs version range
set(Boost_USE_STATIC_LIBS ON)
set(Boost_USE_MULTITHREADED ON)
set(Boost_USE_STATIC_RUNTIME OFF)
find_package(Boost 1.67 REQUIRED COMPONENTS thread)
find_package(PkgConfig REQUIRED)
pkg_check_modules(MOZJS REQUIRED mozjs-60)
pkg_check_modules(OPENSSL REQUIRED openssl)
pkg_check_modules(PROTOBUF REQUIRED protobuf)

find_program(PROTOC protoc)
find_program(PYTHON python)

option(ENABLE_COVERAGE "Build for code coverage reports" OFF)

if(ENABLE_COVERAGE)
    set(COVERAGE_OPTS -fprofile-instr-generate -fcoverage-mapping)
else()
    set(COVERGE_OPTS)
endif()

set(PROTO_DIR ${CMAKE_CURRENT_SOURCE_DIR}/proto)
set(ATELES_PB_SRC ${PROTO_DIR}/ateles.proto)
set(ATELES_PB ${CMAKE_CURRENT_BINARY_DIR}/ateles.pb.cc)

add_custom_command (
    OUTPUT ${ATELES_PB}
    COMMAND ${PROTOC}
        -I ${PROTO_DIR}
        --cpp_out=${CMAKE_CURRENT_BINARY_DIR}
        ateles.proto
    DEPENDS ${ATELES_PB_SRC}
)

file(GLOB SRC "c_src/*.cc")
add_executable(
    ateles
    ${ATELES_PB}
    ${SRC})

set(CFLAGS
    -g
    -std=c++14
    -fvisibility=hidden
    -Wall
    -Werror
    ${MOZJS_CFLAGS}
    ${OPENSSL_CFLAGS}
    ${PROTOBUF_CFLAGS}
    ${COVERAGE_OPTS})

# set_source_files_properties(
#     c_src/server.cc
#     PROPERTIES
#        COMPILE_FLAGS -Wno-deprecated-declarations
# )

set(INCLUDES
    ${CMAKE_CURRENT_BINARY_DIR}
    ${Boost_INCLUDE_DIRS}
    ${MOZJS_INCLUDE_DIRS}
    ${OPENSSL_INCLUDE_DIRS}
    ${PROTOBUF_INCLUDE_DIRS})

set(LDFLAGS
    ${MOZJS_LDFLAGS}
    ${OPENSSL_LDFLAGS}
    ${PROTOBUF_LDFLAGS}
    ${COVERAGE_OPTS})

target_compile_options(ateles PUBLIC ${CFLAGS})
target_include_directories(ateles PUBLIC ${INCLUDES})
target_link_libraries(ateles ${LDFLAGS} ${Boost_LIBRARIES})

