# Taken from #1409 submitted by @sgraham

CTAGS="$1 --quiet --options=NONE --fields=+l"

echo '#' "input: input.cc, optlib: shared"
${CTAGS} --options=./mojom-shared.ctags -o - input.cc
echo '#' "input: input.mojom, optlib: shared"
${CTAGS} --options=./mojom-shared.ctags -o - input.mojom

echo
echo '#' "input: input.cc, optlib: dedicated"
${CTAGS} --options=./mojom-dedicated.ctags -o - input.cc
echo '#' "input: input.mojom, optlib: dedicated"
${CTAGS} --options=./mojom-dedicated.ctags -o - input.mojom

echo
echo '#' "input: input.cc, optlib: bidirectional"
${CTAGS} --options=./mojom-bidirectional.ctags -o - input.cc
echo '#' "input: input.mojom, optlib: bidirectional"
${CTAGS} --options=./mojom-bidirectional.ctags -o - input.mojom

echo
echo '#' "input: input.cc, optlib: <default>"
${CTAGS} --options=./mojom-default.ctags -o - input.cc
echo '#' "input: input.mojom, optlib: <default>"
${CTAGS} --options=./mojom-default.ctags -o - input.mojom
