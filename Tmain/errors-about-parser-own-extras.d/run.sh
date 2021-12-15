set -x
$1 --with-list-header=no --extras-NOSUCHLANG=-'{whitespaceSwapped}' --list-extras; echo $? 2>&1
$1 --with-list-header=no --list-extras; echo $? 2>&1
$1 --extras-NOSUCHLANG=-'{whitespaceSwapped}' --list-extras; echo $? 2>&1
$1 --list-extras; echo $? 2>&1
