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

#include "cxxopts.h"
#include "js/Initialization.h"
#include "jsapi.h"
#include "server.h"
#include "stats.h"
#include "util.h"

int
main(int argc, char* argv[])
{
    // Docs say we have to create at least one JSContext
    // in a single threaded manner. So here we are.
    JS_Init();
    JS_NewContext(8L * 1024 * 1024);

    cxxopts::Options opts(argv[0], "A JavaScript engine for Apache CouchDB\n");

    // clang-format off
    opts.add_options("", {
        {
            "h,help",
            "Display this help message and exit.",
            cxxopts::value<bool>()->default_value("false")
        },
        {
            "a,address",
            "Server ip to bind.",
            cxxopts::value<std::string>()->default_value("0.0.0.0")
        },
        {
            "p,port",
            "Server port to bind.",
            cxxopts::value<std::string>()->default_value("50051")
        },
        {
            "n,num-threads",
            "Number of HTTP server threads to run.",
            cxxopts::value<size_t>()->default_value("1")
        },
        {
            "m,max-mem",
            "Maximum number of megabytes for each JavaScript thread.",
            cxxopts::value<size_t>()->default_value("64")
        },
        {
            "parent_pid",
            "Parent pid to monitor.",
            cxxopts::value<int>()
        },
        {
            "f,stats_frequency",
            "Stats report frequency",
            cxxopts::value<size_t>()
        }
    });
    // clang-format on

    try {
        auto cfg = opts.parse(argc, argv);

        if(cfg["help"].as<bool>()) {
            fprintf(stderr, "%s\n", opts.help().c_str());
            exit(0);
        }

        ServerOpts opts;
        opts.address = cfg["address"].as<std::string>();
        opts.port = cfg["port"].as<std::string>();
        opts.num_threads = cfg["num-threads"].as<size_t>();
        opts.max_mem = cfg["max-mem"].as<size_t>();

        init_signals();

        if(cfg.count("parent_pid")) {
            start_parent_monitor(cfg["parent_pid"].as<int>());
        }

        if(cfg.count("stats_frequency")) {
            start_stats_reporter(cfg["stats_frequency"].as<size_t>());
        }

        Server s(opts);
        s.run();

    } catch(cxxopts::OptionException& exc) {
        fprintf(stderr, "ERROR: %s\n", exc.what());
        exit(1);
    } catch(std::exception& exc) {
        fprintf(stderr, "ERROR: %s\n", exc.what());
        show_stack(255);
    } catch(...) {
        fprintf(stderr, "UNKNOWN ERROR\r\n");
        show_stack(255);
    }

    exit(0);
}
