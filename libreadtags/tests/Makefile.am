TESTS = \
	\
	test-api-tagsOpen \
	test-api-tagsFind \
	test-api-tagsFirstPseudoTag \
	\
	test-fix-unescaping \
	test-fix-null-deref \
	\
	$(NULL)

check_PROGRAMS = \
	\
	test-api-tagsOpen \
	test-api-tagsFind \
	test-api-tagsFirstPseudoTag \
	\
	test-fix-unescaping \
	test-fix-null-deref \
	\
	$(NULL)

EXTRA_DIST =

AM_CPPFLAGS = -I $(top_srcdir)
DEPS = $(top_builddir)/libreadtags.la
LDADD = $(top_builddir)/libreadtags.la

test_api_tagsOpen = test-api-tagsOpen.c
test_api_tagsOpen_DEPENDENCIES = $(DEPS)
EXTRA_DIST += api-tagsOpen-ectags.tags

test_api_tagsFind = test-api-tagsFind.c
test_api_tagsFind_DEPENDENCIES = $(DEPS)
EXTRA_DIST += duplicated-names.c
EXTRA_DIST += duplicated-names--sorted-yes.tags
EXTRA_DIST += duplicated-names--sorted-no.tags
EXTRA_DIST += duplicated-names--sorted-foldcase.tags

test_api_tagsFirstPseudoTag = test-api-tagsFirstPseudoTag.c
test_api_tagsFirstPseudoTag_DEPENDENCIES = $(DEPS)
EXTRA_DIST += ptag-sort-no.tags
EXTRA_DIST += ptag-sort-yes.tags

test_fix_unescaping = test-fix-unescaping.c
test_fix_unescaping_DEPENDENCIES = $(DEPS)
EXTRA_DIST += unescaping.tags

test_fix_null_deref = test-fix-null-deref.c
test_fix_null_deref_DEPENDENCIES = $(DEPS)
EXTRA_DIST += null-deref.tags
