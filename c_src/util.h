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

#ifndef ATELES_UTIL_H
#define ATELES_UTIL_H

#include <boost/system/error_code.hpp>

void init_signals();
void show_stack(int sig);
void start_parent_monitor(int ppid);

void report_error(const char* opname, boost::system::error_code& ec);

#endif  // Included util.h
