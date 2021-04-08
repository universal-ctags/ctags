CC=clang
CFLAGS_D=-std=gnu89 -Wall -Wextra -Wno-unused-parameter -Wno-overlength-strings -Wno-long-long -Wno-format -pedantic -O0 -g2
CFLAGS_R=-std=gnu89 -Wall -Wextra -Wno-unused-parameter -Wno-overlength-strings -Wno-long-long -Wno-format -pedantic -O2 -DNDEBUG
LDFLAGS_D=
LDFLAGS_R=

SRCDIR=../../src
TESTDIR=../../tests
TMPDIR_D=debug/tmp
TMPDIR_R=release/tmp
BINDIR_D=debug/bin
BINDIR_R=release/bin

EXAMPLES:=$(patsubst $(SRCDIR)/examples/%.peg,examples/%,$(wildcard $(SRCDIR)/examples/*.peg))

BINS= \
  $(BINDIR_D)/packcc \
  $(BINDIR_R)/packcc \
  $(patsubst %,$(BINDIR_D)/%,$(EXAMPLES)) \
  $(patsubst %,$(BINDIR_R)/%,$(EXAMPLES))
SRCS= \
  $(patsubst %,$(TMPDIR_D)/%.c,$(EXAMPLES)) \
  $(patsubst %,$(TMPDIR_D)/%.h,$(EXAMPLES)) \
  $(patsubst %,$(TMPDIR_R)/%.c,$(EXAMPLES)) \
  $(patsubst %,$(TMPDIR_R)/%.h,$(EXAMPLES))

.PHONY: all clean

.SECONDARY: $(SRCS)

all: $(BINS)

$(BINDIR_D)/packcc: $(SRCDIR)/packcc.c
	mkdir -p $(dir $@) && $(CC) $(CFLAGS_D) -o $@ $< $(LDFLAGS_D)

$(BINDIR_R)/packcc: $(SRCDIR)/packcc.c
	mkdir -p $(dir $@) && $(CC) $(CFLAGS_R) -o $@ $< $(LDFLAGS_R)

$(BINDIR_D)/examples/%: $(TMPDIR_D)/examples/%.c $(TMPDIR_D)/examples/%.h
	mkdir -p $(dir $@) && $(CC) $(CFLAGS_D) -I. -o $@ $< $(LDFLAGS_D)

$(BINDIR_R)/examples/%: $(TMPDIR_R)/examples/%.c $(TMPDIR_R)/examples/%.h
	mkdir -p $(dir $@) && $(CC) $(CFLAGS_R) -I. -o $@ $< $(LDFLAGS_R)

$(TMPDIR_D)/examples/%.c $(TMPDIR_D)/examples/%.h: $(SRCDIR)/examples/%.peg $(BINDIR_D)/packcc
	mkdir -p $(dir $@) && $(BINDIR_D)/packcc -o $(basename $@) $<

$(TMPDIR_R)/examples/%.c $(TMPDIR_R)/examples/%.h: $(SRCDIR)/examples/%.peg $(BINDIR_R)/packcc
	mkdir -p $(dir $@) && $(BINDIR_R)/packcc -o $(basename $@) $<

check: $(BINDIR_R)/packcc
	PACKCC=$$PWD/$^ $(TESTDIR)/test.sh

clean:
	rm -f $(BINS) $(SRCS)
