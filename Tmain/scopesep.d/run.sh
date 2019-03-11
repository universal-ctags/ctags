# Copyright: 2018 Masatake YAMATO
# License: GPL-2

CTAGS=$1

echo "# root sep and wildcard"
${CTAGS} --quiet --options=NONE					\
		 -o -									\
		 --sort=no								\
		 --extras=+q							\
		 --_scopesep-Tcl='/*:/'					\
		 --_scopesep-Tcl='*/*:+'				\
		 input.tcl

echo "# override wildcard"
${CTAGS} --quiet --options=NONE					\
		 -o -									\
		 --sort=no								\
		 --extras=+q							\
		 --_scopesep-Tcl='/*:/'					\
		 --_scopesep-Tcl='*/*:+'				\
		 --_scopesep-Tcl='n/n:->'				\
		 --_scopesep-Tcl='n/p:^'				\
		 input.tcl

echo "# override rootsep"
${CTAGS} --quiet --options=NONE					\
		 -o -									\
		 --sort=no								\
		 --extras=+q							\
		 --_scopesep-Tcl='/*:/'					\
		 --_scopesep-Tcl='*/*:+'				\
		 --_scopesep-Tcl='/n:@'					\
		 --_scopesep-Tcl='/p:%'					\
		 input.tcl

echo "# no default"
${CTAGS} --quiet --options=NONE					\
		 -o -									\
		 --sort=no								\
		 --extras=+q							\
		 --_scopesep-Tcl='n/n:->'				\
		 --_scopesep-Tcl='n/p:^'				\
		 --_scopesep-Tcl='/n:@'					\
		 --_scopesep-Tcl='/p:%'					\
		 input.tcl
