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
find_package(Boost 1.72 COMPONENTS thread)

if(Boost_FOUND)
    message(STATUS "Boost include dir ${Boost_INCLUDE_DIRS}")
    message(STATUS "Boost libraries ${Boost_LIBRARIES}")
    add_custom_target(boost)
else()
    message(STATUS "Building boost 1.72")
    include(ExternalProject)
    ExternalProject_add(boost
        URL "https://boostorg.jfrog.io/artifactory/main/release/1.72.0/source/boost_1_72_0.tar.bz2"
        URL_HASH SHA256=59c9b274bc451cf91a9ba1dd2c7fdcaf5d60b1b3aa83f2c9fa143417cc660722
        DOWNLOAD_NO_PROGRESS ON
        CONFIGURE_COMMAND ./bootstrap.sh --with-libraries=thread
        BUILD_COMMAND ./b2
        BUILD_IN_SOURCE ON
        INSTALL_COMMAND ""
        UPDATE_COMMAND ""
        BUILD_BYPRODUCTS <SOURCE_DIR>/stage/lib/libboost_thread.a
    )

    ExternalProject_Get_property(boost SOURCE_DIR)
    ExternalProject_Get_Property(boost BINARY_DIR)

    set(Boost_INCLUDE_DIRS ${SOURCE_DIR})
    set(Boost_LIBRARIES ${BINARY_DIR}/stage/lib/libboost_thread.a)

    message(STATUS "Boost include dir ${Boost_INCLUDE_DIRS}")
    message(STATUS "Boost libraries ${Boost_LIBRARIES}")
endif()

find_package(PkgConfig REQUIRED)
pkg_check_modules(MOZJS REQUIRED mozjs-60)
pkg_check_modules(PROTOBUF REQUIRED protobuf)

find_program(PROTOC protoc)
find_program(PYTHON python3)

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
        ${ATELES_PB_SRC}
    DEPENDS ${ATELES_PB_SRC}
)


# Generate our JS data files
set(MKHEADER ${CMAKE_CURRENT_SOURCE_DIR}/tools/mkheader.py)
set(ESCODEGEN_SRC ${CMAKE_CURRENT_SOURCE_DIR}/priv/escodegen.js)
set(ESCODEGEN_HDR ${CMAKE_CURRENT_BINARY_DIR}/escodegen.h)
set(ESPRIMA_SRC ${CMAKE_CURRENT_SOURCE_DIR}/priv/esprima.js)
set(ESPRIMA_HDR ${CMAKE_CURRENT_BINARY_DIR}/esprima.h)
set(REWRITE_SRC ${CMAKE_CURRENT_SOURCE_DIR}/priv/rewrite_fun.js)
set(REWRITE_HDR ${CMAKE_CURRENT_BINARY_DIR}/rewrite_fun.h)
set(VALIDATE_SRC ${CMAKE_CURRENT_SOURCE_DIR}/priv/validate.js)
set(VALIDATE_HDR ${CMAKE_CURRENT_BINARY_DIR}/validate.h)


add_custom_command(
    OUTPUT ${ESCODEGEN_HDR}
    COMMAND ${PYTHON} ${MKHEADER} ${ESCODEGEN_SRC} ${ESCODEGEN_HDR} escodegen
    DEPENDS ${ESCODEGEN_SRC}
)

add_custom_command(
    OUTPUT ${ESPRIMA_HDR}
    COMMAND ${PYTHON} ${MKHEADER} ${ESPRIMA_SRC} ${ESPRIMA_HDR} esprima
    DEPENDS ${ESPRIMA_SRC}
)

add_custom_command(
    OUTPUT ${REWRITE_HDR}
    COMMAND ${PYTHON} ${MKHEADER} ${REWRITE_SRC} ${REWRITE_HDR} rewrite_fun
    DEPENDS ${REWRITE_SRC}
)

add_custom_command(
        OUTPUT ${VALIDATE_HDR}
        COMMAND ${PYTHON} ${MKHEADER} ${VALIDATE_SRC} ${VALIDATE_HDR} validate
        DEPENDS ${VALIDATE_SRC}
)


file(GLOB SRC "c_src/*.cc")
add_executable(
    ateles
    ${ATELES_PB}
    ${SRC}
    ${ESCODEGEN_HDR}
    ${ESPRIMA_HDR}
    ${REWRITE_HDR}
    ${VALIDATE_HDR})

set(CFLAGS
    -g
    -std=c++14
    -fvisibility=hidden
    -Wall
    -Werror
    ${MOZJS_CFLAGS}
    ${PROTOBUF_CFLAGS}
    ${COVERAGE_OPTS})

set(INCLUDES
    ${CMAKE_CURRENT_BINARY_DIR}
    ${Boost_INCLUDE_DIRS}
    ${MOZJS_INCLUDE_DIRS}
    ${PROTOBUF_INCLUDE_DIRS})

set(LDFLAGS
    ${MOZJS_LDFLAGS}
    ${PROTOBUF_LDFLAGS}
    ${COVERAGE_OPTS})

target_compile_options(ateles PUBLIC ${CFLAGS})
target_include_directories(ateles PUBLIC ${INCLUDES})
target_link_libraries(ateles ${LDFLAGS} ${Boost_LIBRARIES})
add_dependencies(ateles boost)
