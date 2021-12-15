set -x
$1 --verbose --list-extras; echo $? 2>&1
