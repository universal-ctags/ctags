# Copyright: 2016 Aman Gupta
# License: GPL-2

CTAGS=$1
. ../utils.sh

is_feature_available ${CTAGS} interactive

# It seems that the output format is slightly different between libjansson versions
s()
{
	sed -e s/':"'/': "'/g
}

CTAGS="$CTAGS --options=NONE"

echo identification message on startup
echo =======================================
${CTAGS} --_interactive < /dev/null |s

echo
echo error on invalid command
echo =======================================
echo '{"command":"foobar"}' | ${CTAGS} --_interactive |s

echo
echo error on missing arguments
echo =======================================
echo '{"command":"generate-tags"}' | ${CTAGS} --_interactive |s

echo
echo error on invalid file
echo =======================================
echo '{"command":"generate-tags", "filename":"test.foo"}' | ${CTAGS} --_interactive |s

echo
echo generate tags from file
echo =======================================
echo '{"command":"generate-tags", "filename":"test.rb"}' | ${CTAGS} --_interactive |s

echo
echo process multiple commands
echo =======================================
(
  echo '{"command":"generate-tags", "filename":"test.rb"}'
  echo '{"command":"generate-tags", "filename":"test.c"}'
) | ${CTAGS} --_interactive |s

echo
echo generate tags from data
echo =======================================
size=$(filesize test.rb)
(
  echo '{"command":"generate-tags", "filename":"foobar.rb", "size":'$size'}'
  cat test.rb
) | ${CTAGS} --_interactive |s
