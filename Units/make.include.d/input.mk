include A
sinclude B
-include C
includeX
sincludeY
-includeZ
=include I
=includeJ
define D
include E
endef

include $@ $* $< &AND a/b a_b.mak #ABC
include $(1) $* Z0 $(shell) $(wildcard *.h) $(SHELL)
sinclude $(2)
-include $(3)

include F G H
sinclude I J K
-include L M N

