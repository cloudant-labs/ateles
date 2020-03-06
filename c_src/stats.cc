// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

#include <time.h>
#include <unistd.h>

#include <thread>
#include <memory>

#include "stats.h"


std::atomic<uint64_t> ATELES_STAT_ACCEPTS{0};
std::atomic<uint64_t> ATELES_STAT_OPEN_CONNS{0};

std::atomic<uint64_t> ATELES_STAT_HTTP_200{0};
std::atomic<uint64_t> ATELES_STAT_HTTP_400{0};
std::atomic<uint64_t> ATELES_STAT_HTTP_404{0};
std::atomic<uint64_t> ATELES_STAT_HTTP_405{0};
std::atomic<uint64_t> ATELES_STAT_HTTP_UNK{0};

std::atomic<uint64_t> ATELES_STAT_JS_CONTEXTS{0};
std::atomic<uint64_t> ATELES_STAT_JS_SUCCESS{0};
std::atomic<uint64_t> ATELES_STAT_JS_ERROR{0};
std::atomic<uint64_t> ATELES_STAT_JS_GC_BYTES{0};


static void
report()
{
    // Counters
    uint64_t accepts = ATELES_STAT_ACCEPTS;

    uint64_t http_200 = ATELES_STAT_HTTP_200;
    uint64_t http_400 = ATELES_STAT_HTTP_400;
    uint64_t http_404 = ATELES_STAT_HTTP_404;
    uint64_t http_405 = ATELES_STAT_HTTP_405;
    uint64_t http_unk = ATELES_STAT_HTTP_UNK;

    uint64_t js_success = ATELES_STAT_JS_SUCCESS;
    uint64_t js_error = ATELES_STAT_JS_ERROR;

    // Gauges, do not reset after reporting
    uint64_t open_conns = ATELES_STAT_OPEN_CONNS;
    uint64_t js_contexts = ATELES_STAT_JS_CONTEXTS;
    uint64_t js_gc_bytes = ATELES_STAT_JS_GC_BYTES;

    const char* fmt = "stats: "
            "accepts:%llu open:%llu "
            "http 200:%llu 400:%llu 404:%llu 405:%llu unk:%llu "
            "js ctx:%llu success:%llu error:%llu gc_bytes:%llu\n";

    fprintf(
            stderr,
            fmt,
            accepts,
            open_conns,
            http_200,
            http_400,
            http_404,
            http_405,
            http_unk,
            js_contexts,
            js_success,
            js_error,
            js_gc_bytes
        );

    // Reset counters. Notice that setting to zero would
    // lose track of anything that happened while writing
    // the report.
    ATELES_STAT_ACCEPTS -= accepts;
    ATELES_STAT_HTTP_200 -= http_200;
    ATELES_STAT_HTTP_400 -= http_400;
    ATELES_STAT_HTTP_404 -= http_404;
    ATELES_STAT_HTTP_405 -= http_405;
    ATELES_STAT_HTTP_UNK -= http_unk;
    ATELES_STAT_JS_SUCCESS -= js_success;
    ATELES_STAT_JS_ERROR -= js_error;
}

void
stats_reporter(size_t freq)
{
    while(true) {
        sleep(freq);
        report();
    }
}

void
start_stats_reporter(size_t freq)
{
    auto thread = std::make_unique<std::thread>(stats_reporter, freq);
    thread->detach();
}