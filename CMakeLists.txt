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
find_package(PkgConfig REQUIRED)
pkg_check_modules(MOZJS REQUIRED mozjs-60)
pkg_check_modules(PROTOBUF REQUIRED protobuf)
pkg_check_modules(GRPC REQUIRED grpc++)
pkg_check_modules(GPR REQUIRED gpr)

find_program(PROTOC protoc)
find_program(GRPC_PLUGIN grpc_cpp_plugin)
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
set(ATELES_GRPC ${CMAKE_CURRENT_BINARY_DIR}/ateles.grpc.pb.cc)
set(HEALTH_PB_SRC ${PROTO_DIR}/health.proto)
set(HEALTH_PB ${CMAKE_CURRENT_BINARY_DIR}/health.pb.cc)
set(HEALTH_GRPC ${CMAKE_CURRENT_BINARY_DIR}/health.grpc.pb.cc)

add_custom_command (
    OUTPUT ${ATELES_PB}
    COMMAND ${PROTOC}
        -I ${PROTO_DIR}
        --cpp_out=${CMAKE_CURRENT_BINARY_DIR}
        ateles.proto
    DEPENDS ${ATELES_PB_SRC}
)

add_custom_command(
    OUTPUT ${ATELES_GRPC}
    COMMAND ${PROTOC}
        -I ${PROTO_DIR}
        --grpc_out=${CMAKE_CURRENT_BINARY_DIR}
        --plugin=protoc-gen-grpc=${GRPC_PLUGIN}
        ateles.proto
    DEPENDS ${ATELES_PB_SRC}
)

add_custom_command (
    OUTPUT ${HEALTH_PB}
    COMMAND ${PROTOC}
        -I ${PROTO_DIR}
        --cpp_out=${CMAKE_CURRENT_BINARY_DIR}
        health.proto
    DEPENDS ${HEALTH_PB_SRC}
)

add_custom_command(
    OUTPUT ${HEALTH_GRPC}
    COMMAND ${PROTOC}
        -I ${PROTO_DIR}
        --grpc_out=${CMAKE_CURRENT_BINARY_DIR}
        --plugin=protoc-gen-grpc=${GRPC_PLUGIN}
        health.proto
    DEPENDS ${HEALTH_PB_SRC}
)

file(GLOB SRC "c_src/*.cc")
add_executable(
    ateles
    ${ATELES_PB}
    ${ATELES_GRPC}
    ${HEALTH_PB}
    ${HEALTH_GRPC}
    ${SRC})

set(CFLAGS
    -g
    -std=c++14
    -Wall
    -Werror
    ${MOZJS_CFLAGS}
    ${PROTOBUF_CFLAGS}
    ${GRPC_CFLAGS}
    ${GPR_CFLAGS}
    ${COVERAGE_OPTS})

set(INCLUDES
    ${CMAKE_CURRENT_BINARY_DIR}
    ${MOZJS_INCLUDE_DIRS}
    ${PROTOBUF_INCLUDE_DIRS}
    ${GRPC_INCLUDE_DIRS}
    ${GPR_INCLUDE_DIRS})

set(LDFLAGS
    ${MOZJS_LDFLAGS}
    ${PROTOBUF_LDFLAGS}
    ${GRPC_LDFLAGS}
    ${GPR_LDFLAGS}
    ${COVERAGE_OPTS})

target_compile_options(ateles PUBLIC ${CFLAGS})
target_include_directories(ateles PUBLIC ${INCLUDES})
target_link_libraries(ateles ${LDFLAGS})
