AUTOMAKE_OPTIONS = foreign
ACLOCAL_AMFLAGS = -I m4

AM_CFLAGS=-Wall

lib_LTLIBRARIES    = libreadtags.la
libreadtags_la_LDFLAGS = -no-undefined -version-info $(LT_VERSION)

libreadtags_la_SOURCES = readtags.c readtags.h
nobase_include_HEADERS = readtags.h

EXTRA_DIST = libreadtags.pc.in
pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libreadtags.pc

SUBDIRS = . tests
