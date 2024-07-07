# Taken from linux/tools/testing/selftests/powerpc/Makefile
override define RUN_TESTS
	+@for TARGET in $(SUB_DIRS); do \
		BUILD_TARGET=$(OUTPUT)/$$TARGET;	\
		$(MAKE) OUTPUT=$$BUILD_TARGET -C $$TARGET run_tests;\
	done;
endef

override define INSTALL_RULE
	+@for TARGET in $(SUB_DIRS); do \
		BUILD_TARGET=$(OUTPUT)/$$TARGET;	\
		$(MAKE) OUTPUT=$$BUILD_TARGET INSTALL_PATH=$$INSTALL_PATH/$$TARGET -C $$TARGET install;\
	done;
endef

ifneq ($(findstring s,$(short-opts)),)
quiet=silent_
override KBUILD_VERBOSE :=
endif
