#!/usr/bin/env python
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

import sys


def make_bytes(src):
    for line in src:
        for c in line:
            yield "0x{:02x}".format(ord(c))


def mkheader(base_varname, src, tgt):
    tgt.write("unsigned char %s_data[] = {\n" % base_varname)
    total_size = 0
    curr_row = 0
    for byte in make_bytes(src):
        if total_size > 0 and curr_row != 12:
            tgt.write(",")
        elif total_size > 0:
            tgt.write(",")
        if curr_row == 12:
            tgt.write("\n")
            curr_row = 0
        tgt.write(byte)
        total_size += 1
        curr_row += 1
    if curr_row != 12:
        tgt.write("\n")
    tgt.write("};\n")
    tgt.write("unsigned int %s_len = %d;\n" % (base_varname, total_size))


def main():
    if len(sys.argv) != 4:
        print "usage: %s infile outfile base_varname" % sys.argv[0]
        exit(1)

    with open(sys.argv[1]) as src:
        with open(sys.argv[2], "wb") as tgt:
            mkheader(sys.argv[3], src, tgt)


if __name__ == "__main__":
    main()
