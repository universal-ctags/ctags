TESTS = \
	\
	test-api-tagsOpen \
	test-api-tagsFind \
	test-api-tagsFindPseudoTag \
	test-api-tagsFirstPseudoTag \
	test-api-tagsFirst \
	test-api-tagsClose \
	test-api-tagsSetSortType \
	\
	test-fix-unescaping \
	test-fix-null-deref \
	test-fix-large-tags \
	test-fix-unescaping-input-fields \
	test-fix-unescaping-input-fields-exuberant \
	test-fix-unescaping-input-fields-no-mode \
	test-fix-unescaping-input-fields-backslash \
	test-fix-unescaping-input-fields-no-filesep \
	\
	$(NULL)

check_PROGRAMS = \
	\
	test-api-tagsOpen \
	test-api-tagsFind \
	test-api-tagsFindPseudoTag \
	test-api-tagsFirstPseudoTag \
	test-api-tagsFirst \
	test-api-tagsClose \
	test-api-tagsSetSortType \
	\
	test-fix-unescaping \
	test-fix-null-deref \
	test-fix-large-tags \
	test-fix-unescaping-input-fields \
	test-fix-unescaping-input-fields-exuberant \
	test-fix-unescaping-input-fields-no-mode \
	test-fix-unescaping-input-fields-backslash \
	test-fix-unescaping-input-fields-no-filesep \
	\
	$(NULL)

EXTRA_DIST = test-fields.h

AM_CPPFLAGS = -I $(top_srcdir) -DTAG_NO_COMPAT_SORT_TYPE
AM_CFLAGS = $(GCOV_CFLAGS)
DEPS = $(top_builddir)/libreadtags.la
LDADD = $(top_builddir)/libreadtags.la

test_api_tagsOpen = test-api-tagsOpen.c
test_api_tagsOpen_DEPENDENCIES = $(DEPS)
EXTRA_DIST += api-tagsOpen-ectags.tags
EXTRA_DIST += api-tagsOpen-wrong-format-nonum.tags
EXTRA_DIST += api-tagsOpen-wrong-format-num.tags
EXTRA_DIST += api-tagsOpen-wrong-sort-method-nonum.tags
EXTRA_DIST += api-tagsOpen-wrong-sort-method-num.tags
EXTRA_DIST += api-tagsOpen-incomplete-program-author-0.tags
EXTRA_DIST += api-tagsOpen-incomplete-program-author-1.tags
EXTRA_DIST += api-tagsOpen-incomplete-program-author-2.tags
EXTRA_DIST += api-tagsOpen-incomplete-program-author-3.tags
EXTRA_DIST += api-tagsOpen-incomplete-program-author-4.tags
EXTRA_DIST += api-tagsOpen-incomplete-program-author-5.tags

test_api_tagsFind = test-api-tagsFind.c
test_api_tagsFind_DEPENDENCIES = $(DEPS)
EXTRA_DIST += duplicated-names.c
EXTRA_DIST += duplicated-names--sorted-yes.tags
EXTRA_DIST += duplicated-names--sorted-no.tags
EXTRA_DIST += duplicated-names--sorted-foldcase.tags
EXTRA_DIST += broken-line-field-in-middle.tags

test_api_tagsFindPseudoTag = test-api-tagsFindPseudoTag.c
test_api_tagsFindPseudoTag_DEPENDENCIES = $(DEPS)

test_api_tagsFirstPseudoTag = test-api-tagsFirstPseudoTag.c
test_api_tagsFirstPseudoTag_DEPENDENCIES = $(DEPS)
EXTRA_DIST += ptag-sort-no.tags
EXTRA_DIST += ptag-sort-yes.tags

test_api_tagsFirst = test-api-tagsFirst.c
test_api_tagsFirst_DEPENDENCIES = $(DEPS)
EXTRA_DIST += empty.tags
EXTRA_DIST += empty-no-newline.tags
EXTRA_DIST += broken-line-field.tags
EXTRA_DIST += broken-line-field-other-than-first.tags

test_api_tagsClose = test-api-tagsClose.c
test_api_tagsClose_DEPENDENCIES = $(DEPS)

test_api_tagsSetSortType = test-api-tagsSetSortType.c
test_api_tagsSetSortType_DEPENDENCIES = $(DEPS)

test_fix_unescaping = test-fix-unescaping.c
test_fix_unescaping_DEPENDENCIES = $(DEPS)
EXTRA_DIST += unescaping.tags

test_fix_null_deref = test-fix-null-deref.c
test_fix_null_deref_DEPENDENCIES = $(DEPS)
EXTRA_DIST += null-deref.tags

test_fix_large_tags = test-fix-large-tags.c
test_fix_large_tags_DEPENDENCIES = $(DEPS)

test_fix_unescaping_input_fields = test-fix-unescaping-input-fields.c
test_fix_unescaping_input_fields_DEPENDENCIES = $(DEPS)
EXTRA_DIST += unescaping-input-fields.tags

test_fix_unescaping_input_fields_exuberant = test-fix-unescaping-input-fields-exuberant.c
test_fix_unescaping_input_fields_exuberant_DEPENDENCIES = $(DEPS)
EXTRA_DIST += unescaping-input-fields-exuberant.tags

test_fix_unescaping_input_fields_no_mode = test-fix-unescaping-input-fields-no-mode.c
test_fix_unescaping_input_fields_no_mode_DEPENDENCIES = $(DEPS)
EXTRA_DIST += unescaping-input-fields-no-mode.tags

test_fix_unescaping_input_fields_backslash = test-fix-unescaping-input-fields-backslash.c
test_fix_unescaping_input_fields_backslash_DEPENDENCIES = $(DEPS)
EXTRA_DIST += unescaping-input-fields-backslash.tags

test_fix_unescaping_input_fields_no_filesep = test-fix-unescaping-input-fields-no-filesep.c
test_fix_unescaping_input_fields_no_filesep_DEPENDENCIES = $(DEPS)
EXTRA_DIST += unescaping-input-fields-no-filesep.tags
