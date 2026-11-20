#!/bin/python3

# This script is derived on #4409 opened by @theteatoast.

import sys

n="input.js"

f=open(n,"w")

depth=int(sys.argv[1])

f.write(f"# depth: {depth}\n")

for i in range(depth):
    f.write(f"var f{i} = function() {{\n")
for i in range(depth):
    f.write("};\n")

f.close()
