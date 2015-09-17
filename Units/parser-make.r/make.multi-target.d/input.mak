
all: foo bar

foo bar: baz
	echo $@

baz:
	echo $@
