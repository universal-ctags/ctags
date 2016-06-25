#!/bin/sh
limit=96
calls=

l=$limit
for i in $(seq $l); do
    echo -n ' '
done
echo "func$l()"
echo "{"
echo
echo "}"
calls="$calls func$l"

l=$(expr $limit - 1)
for i in $(seq $l); do
    echo -n ' '
done
echo "func$l()"
echo "{"
echo
echo "}"
calls="$calls func$l"

l=$(expr $limit + 1)
for i in $(seq $l); do
    echo -n ' '
done
echo "func$l()"
echo "{"
echo
echo "}"
calls="$calls func$l"

echo

for c in $calls; do
    echo $c
done
