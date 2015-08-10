CTAGS=$1

# input.js is extracted from
# https://sourceforge.net/p/ctags/bugs/_discuss/thread/c4664575/fa38/attachment/Tokenizer.js
# as reported in https://sourceforge.net/p/ctags/bugs/308/

${CTAGS} --quiet --options=NONE -o - input.js
