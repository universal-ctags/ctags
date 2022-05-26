# Based on https://github.com/universal-ctags/ctags/pull/3353#issuecomment-1103403502
# posted by @eli-schwartz.
all:
	@if type gmake > /dev/null 2>&1; then							\
		gmake $@;									\
	else											\
		@echo "WARNING: GNU make is needed for updating win32 related build-script.";	\
		@echo "WARNING: skip the operations in this directory: gmake $@";		\
	fi

.DEFAULT:
	@if type gmake > /dev/null 2>&1; then							\
		gmake $@;									\
	else											\
		echo "WARNING: GNU make is needed for updating win32 related build-script.";	\
		echo "WARNING: skip the operations in this directory: gmake $@";		\
	fi

.PHONY: all
