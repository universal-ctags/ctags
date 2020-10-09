#!/bin/sh

base=5.9
cal=$(date +%Y%m%d)
chicken=0
tag="p${base}.${cal}.${chicken}"

if desc=$(git describe --tags --exact-match); then
	case "${desc}" in
		p*.0)
			echo "do nothing because nothing happens since last tagging"
			exit 0
			;;
		*)
			echo "found a tag but it is not periodical one; create a periodical tag"
			;;
	esac
fi

r=0
desc=$(git describe --tags --always)
printf "%s (%s <= %s)..." "create tag" "${desc}" "${tag}"
if git tag "${tag}"; then
	echo "done"
	printf "push the tag..."
	if git push origin --tags; then
		echo "done"
	else
		echo "failed"
		r=2
	fi
else
	echo "failed"
	r=1
fi
exit $r
