#!/bin/bash

G()
{
    local f=$1
    shift

    local p

    echo '***' $f '***'
    for p in "$@"; do
	grep -h '\<'"$p"'\>' "$f"
    done
}

G configure.ac AC_INIT 
G main/ctags.h PROGRAM_VERSION PROGRAM_COPYRIGHT OUTPUT_VERSION_CURRENT OUTPUT_VERSION_AGE
G win32/ctags.rc FILEVERSION FILEVERSION FileVersion LegalCopyright LegalCopyright
G win32/ctags.exe.manifest assemblyIdentity
G win32/config_mvc.h PACKAGE_STRING PACKAGE_VERSION VERSION
G win32/config_mingw.h PACKAGE_STRING PACKAGE_VERSION VERSION
G misc/git-tag-maybe.sh BASE
G NEWS.rst 'Changes in '
