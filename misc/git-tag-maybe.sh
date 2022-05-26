#!/bin/sh

set -e

##################################### util #######################################

COLOR_RED='\033[0;31m'          # Red
COLOR_GREEN='\033[0;32m'        # Green
COLOR_PURPLE='\033[0;35m'       # Purple
COLOR_OFF='\033[0m'             # Reset

note() {
    printf '\n%b\n' "${COLOR_RED}$*${COLOR_OFF}"
}

run() {
    printf '%b\n' "${COLOR_PURPLE}==>${COLOR_OFF} ${COLOR_GREEN}$*${COLOR_OFF}"
    eval "$*"
}

##################################### main #######################################

base=5.9
cal=$(date +%Y%m%d)
chicken=0
new_tagname="p${base}.${cal}.${chicken}"

run git --version

case "$(git describe --tags --exact-match HEAD 2>/dev/null || true)" in
    p*.0)
        note "do nothing. because there are no commits since the latest tag."
        exit 0
        ;;
    '') ;;
    *)  echo "found a tag but it is not periodical one; create a periodical tag"
esac

for old_tagname in $(git tag --list)
do
    if [ "$old_tagname" = "$new_tagname" ] ; then
        note "do nothing. because $new_tagname tag already exists."
        exit 0
    fi
done

run git tag "$new_tagname"
run git push origin "$new_tagname"
