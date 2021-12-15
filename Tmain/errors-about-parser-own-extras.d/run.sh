set -x
$1 --quiet --options=NONE --with-list-header=no --extras-NOSUCHLANG=-'{whitespaceSwapped}' --list-extras; echo $? 2>&1
$1 --quiet --options=NONE --with-list-header=no --extras-NOSUCHLANG=-'{whitespaceSwapped}'; echo $? 2>&1
$1 --quiet --options=NONE --with-list-header=no; echo $? 2>&1
$1 --quiet --options=NONE; echo $? 2>&1
$1 --quiet; echo $? 2>&1
$1 --options=NONE --with-list-header=no --extras-NOSUCHLANG=-'{whitespaceSwapped}' --list-extras; echo $? 2>&1
$1 --options=NONE --with-list-header=no --extras-NOSUCHLANG=-'{whitespaceSwapped}'; echo $? 2>&1
$1 --options=NONE --with-list-header=no; echo $? 2>&1
$1 --options=NONE; echo $? 2>&1
$1 --with-list-header=no --extras-NOSUCHLANG=-'{whitespaceSwapped}' --list-extras; echo $? 2>&1
$1 --with-list-header=no --extras-NOSUCHLANG=-'{whitespaceSwapped}'; echo $? 2>&1
$1 --with-list-header=no; echo $? 2>&1
$1 --verbose --options=NONE --with-list-header=no --extras-NOSUCHLANG=-'{whitespaceSwapped}' --list-extras; echo $? 2>&1
$1 --verbose --options=NONE --with-list-header=no --extras-NOSUCHLANG=-'{whitespaceSwapped}'; echo $? 2>&1
$1 --verbose --options=NONE --with-list-header=no; echo $? 2>&1
$1 --verbose --options=NONE; echo $? 2>&1
$1 --verbose --with-list-header=no --extras-NOSUCHLANG=-'{whitespaceSwapped}' --list-extras; echo $? 2>&1
$1 --verbose --with-list-header=no --extras-NOSUCHLANG=-'{whitespaceSwapped}'; echo $? 2>&1
$1 --verbose --with-list-header=no; echo $? 2>&1
$1 --verbose; echo $? 2>&1
$1; echo $? 2>&1
