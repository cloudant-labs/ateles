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

#include "util.h"

#include <memory>
#include <thread>

#include <execinfo.h>
#include <signal.h>
#include <unistd.h>

void
show_stack(int sig)
{
    void* frames[64];
    size_t size;

    fprintf(stderr, "Error: Signal %d\n", sig);
    size = backtrace(frames, 64);
    backtrace_symbols_fd(frames, size, STDERR_FILENO);

    if(sig == 0) {
        return;
    }

    exit(255);
}

void
init_signals()
{
    signal(SIGINT, show_stack);
    signal(SIGQUIT, show_stack);
    signal(SIGABRT, show_stack);
    signal(SIGBUS, show_stack);
    signal(SIGSEGV, show_stack);
}

void
parent_monitor(pid_t ppid)
{
    while(true) {
        if(kill(ppid, 0) != 0) {
            exit(4);
        }
        sleep(1);
    }
}

void
start_parent_monitor(int ppid)
{
    auto thread = std::make_unique<std::thread>(parent_monitor, ppid);
    thread->detach();
}


void
report_error(const char* opname, boost::system::error_code& ec)
{
    const char* fmt = "error: %s [%d] %s - %s\n";
    fprintf(
            stderr,
            fmt,
            opname,
            ec.value(),
            ec.category().name(),
            ec.message().c_str()
        );
}
