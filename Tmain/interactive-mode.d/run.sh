# Copyright: 2016 Aman Gupta
# License: GPL-2

CTAGS=$1
. ../utils.sh

is_feature_available ${CTAGS} interactive

echo identification message on startup
echo =======================================
${CTAGS} --interactive < /dev/null

echo
echo error on invalid command
echo =======================================
echo '{"command":"foobar"}' | ${CTAGS} --interactive

echo
echo error on missing arguments
echo =======================================
echo '{"command":"generate-tags"}' | ${CTAGS} --interactive

echo
echo error on invalid file
echo =======================================
echo '{"command":"generate-tags", "filename":"test.foo"}' | ${CTAGS} --interactive

echo
echo generate tags from file
echo =======================================
echo '{"command":"generate-tags", "filename":"test.rb"}' | ${CTAGS} --interactive

echo
echo process multiple commands
echo =======================================
(
  echo '{"command":"generate-tags", "filename":"test.rb"}'
  echo '{"command":"generate-tags", "filename":"test.c"}'
) | ${CTAGS} --interactive

echo
echo generate tags from data
echo =======================================
size=$(filesize test.rb)
(
  echo '{"command":"generate-tags", "filename":"foobar.rb", "size":'$size'}'
  cat test.rb
) | ${CTAGS} --interactive
