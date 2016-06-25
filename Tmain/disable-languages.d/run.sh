# Copyright: 2016 Masatake YAMATO
# License: GPL-2
CTAGS=$1

echo '# FILE NAME ONLY'
# extension `m' matches both matlab and objc.
# matlab wins by the alphabetical order of parser names
${CTAGS} -G --print-language input.m

# extension `m' matches only objc because matlab is disabled.
${CTAGS} -G --languages=-MatLab --print-language input.m

# extension `m' matches only matlab because objc is disabled.
${CTAGS} -G --languages=-ObjectiveC --print-language input.m

# extension `m' matches no parser because the both objc and matlab
# are disabled.
${CTAGS} -G --languages=-ObjectiveC,-MatLab --print-language input.m
${CTAGS} -G --languages=-MatLab,-ObjectiveC --print-language input.m

echo '# EMACS MODE: MATLAB'
# extension `m' matches both matlab and objc.
# matlab wins by emacs modeline written in the input file.
${CTAGS} -G --print-language input-matlab.m

# extension `m' matches only objc. That's all.
${CTAGS} -G --languages=-MatLab --print-language input-matlab.m

# extension `m' matches only matlab. That's all.
${CTAGS} -G --languages=-ObjectiveC --print-language input-matlab.m

# extension `m' matches no parser because the both objc and matlab
# are disabled. That's all. ctags has no chance to read the file contents.
${CTAGS} -G --languages=-ObjectiveC,-MatLab --print-language input-matlab.m
${CTAGS} -G --languages=-MatLab,-ObjectiveC --print-language input-matlab.m

echo '# EMACS MODE: OBJC'
# extension `m' matches both matlab and objc.
# objc wins by emacs modeline written in the input file.
${CTAGS} -G --print-language input-objc.m

# extension `m' matches only objc. That's all.
${CTAGS} -G --languages=-MatLab --print-language input-objc.m

# extension `m' matches only matlab. That's all.
${CTAGS} -G --languages=-ObjectiveC --print-language input-objc.m

# extension `m' matches no parser because the both objc and matlab
# are disabled. That's all. ctags has no chance to read the file contents.
${CTAGS} -G --languages=-ObjectiveC,-MatLab --print-language input-objc.m
${CTAGS} -G --languages=-MatLab,-ObjectiveC --print-language input-objc.m
