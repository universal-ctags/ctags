#!/bin/sh

set -e

COLOR_RED='\033[0;31m'          # Red
COLOR_GREEN='\033[0;32m'        # Green
COLOR_PURPLE='\033[0;35m'       # Purple
COLOR_OFF='\033[0m'             # Reset

success() {
    printf '%b\n' "${COLOR_GREEN}[✔] $*${COLOR_OFF}" >&2
}

die() {
    printf '%b\n' "${COLOR_RED}💔  $*${COLOR_OFF}" >&2
    exit 1
}

# usage:   die_due_to_command_not_found <COMMAND-NAME> <PROVIDED-BY-PACKAGE> <WEBSITE>
# example: die_due_to_command_not_found    rst2mam           docutils        https://docutils.sourceforge.io/
die_due_to_command_not_found() {
    printf '%b\n' "${COLOR_RED}💔  command not found:${COLOR_OFF} ${COLOR_GREEN}$1${COLOR_OFF}
    ${COLOR_RED}Please install${COLOR_OFF} ${COLOR_GREEN}$1${COLOR_OFF} ${COLOR_RED}utility which is usually shipped with${COLOR_OFF} ${COLOR_GREEN}$2${COLOR_OFF} ${COLOR_RED}package.${COLOR_OFF}
    ${COLOR_RED}To find more information, please go to website${COLOR_OFF} ${COLOR_GREEN}$3${COLOR_OFF}" >&2
    exit 1
}

run() {
    if [ "$V" = yes ] ; then
        printf '%b\n' "${COLOR_PURPLE}==>${COLOR_OFF} ${COLOR_GREEN}$*${COLOR_OFF}"
    fi

    eval "$*"
}

# usage:   find_exec <COMMAND-NAME>
# example: find_exec rst2man
find_exec() {
    case $1 in
        '') die "$_0 find-exec <COMMAND-NAME>, <COMMAND-NAME> must not be empty." ;;
        rst2man)
            for item in rst2man rst2man.py rst2man-3 rst2man-3.6 rst2man-3.7 rst2man-3.8 rst2man-3.9
            do
                command -v "$item" && break
            done
            ;;
        rst2html)
            for item in rst2html rst2html.py rst2html-3 rst2html-3.6 rst2html-3.7 rst2html-3.8 rst2html-3.9
            do
                command -v "$item" && break
            done
            ;;
        *)  command -v "$1"
    esac
}

gen_rst() {
    for RST_IN_FILENAME in *.rst.in
    do
        run "sed -e 's/@CTAGS_NAME_EXECUTABLE@/ctags/g' -e 's/@ETAGS_NAME_EXECUTABLE@/etags/g' -e 's/@VERSION@/5.9.0/g' ${RST_IN_FILENAME} > ${RST_IN_FILENAME%.in}"
    done
}

# usage:   gen_man <RST2MAN-COMMAND-NAME-OR-PATH> [RST2MAN-COMMAND-OPTOPNS]
# example: gen_man rst2man
# example: gen_man rst2man --syntax-highlight=none
# example: gen_man /usr/local/bin/rst2man --syntax-highlight=none
gen_man() {
    unset RST2MAN

    case $1 in
        rst2man)
            RST2MAN=$(find_exec rst2man) || die_due_to_command_not_found rst2man docutils https://docutils.sourceforge.io/ ;;
        '') [ -z "$1" ] && die "$_0 <RST2MAN-COMMAND-NAME-OR-PATH> [RST2MAN-COMMAND-OPTOPNS], <RST2MAN-COMMAND-NAME-OR-PATH> must be not empty." ;;
        *)  [ -e "$1" ] || die "$_0 <RST2MAN-COMMAND-NAME-OR-PATH> [RST2MAN-COMMAND-OPTIONS], file is not exist: $1"
            RST2MAN="$1"
    esac

    shift

    gen_rst

    RST2MAN_OPTIONS=$@

    if [ -z "$RST2MAN_OPTIONS" ] && [ -n "$($RST2MAN --help | sed -n '/--syntax-highlight/p')" ] ; then
        RST2MAN_OPTIONS='--syntax-highlight=none'
    fi

    for RST_IN_FILENAME in *.rst.in
    do
        run "$RST2MAN $RST2MAN_OPTIONS ${RST_IN_FILENAME%.in} > ${RST_IN_FILENAME%.rst.in}"
    done
}

# usage:   gen_html <RST2HTML-COMMAND-NAME-OR-PATH> [RST2HTML-COMMAND-OPTOPNS]
# example: gen_html rst2html
# example: gen_html /usr/local/bin/rst2html --syntax-highlight=none
gen_html() {
    unset RST2HTML

    case $1 in
        rst2html)
            RST2HTML=$(find_exec rst2html) || die_due_to_command_not_found rst2html docutils https://docutils.sourceforge.io/ ;;
        '') [ -z "$1" ] && die "$_0 <RST2HTML-COMMAND-NAME-OR-PATH> [RST2HTML-COMMAND-OPTOPNS], <RST2HTML-COMMAND-NAME-OR-PATH> must be not empty." ;;
        *)  [ -e "$1" ] || die "$_0 <RST2HTML-COMMAND-NAME-OR-PATH> [RST2HTML-COMMAND-OPTIONS], file is not exist: $1"
            RST2HTML="$1"
    esac

    shift

    gen_rst

    for RST_IN_FILENAME in *.rst.in
    do
        run "$RST2HTML $@ ${RST_IN_FILENAME%.in} > ${RST_IN_FILENAME%.rst.in}.html"
    done
}

# usage:   gen_pdf <RST2PDF-COMMAND-NAME-OR-PATH> [RST2PDF-COMMAND-OPTOPNS]
# example: gen_pdf rst2pdf
# example: gen_pdf /usr/local/bin/rst2pdf
gen_pdf() {
    unset RST2PDF

    case $1 in
        rst2pdf)
            RST2PDF=$(find_exec rst2pdf) || die_due_to_command_not_found rst2pdf rst2pdf https://rst2pdf.org/ ;;
        '') [ -z "$1" ] && die "$_0 <RST2PDF-COMMAND-NAME-OR-PATH> [RST2PDF-COMMAND-OPTOPNS], <RST2PDF-COMMAND-NAME-OR-PATH> must be not empty." ;;
        *)  [ -e "$1" ] || die "$_0 <RST2PDF-COMMAND-NAME-OR-PATH> [RST2PDF-COMMAND-OPTIONS], file is not exist: $1"
            RST2PDF="$1"
    esac

    shift

    gen_rst

    for RST_IN_FILENAME in *.rst.in
    do
        run "$RST2PDF $@ -o ${RST_IN_FILENAME%.rst.in}.pdf ${RST_IN_FILENAME%.in}"
    done
}

update_docs() {
    gen_rst

    # Delete the line "------" in the first 10 lines of ../docs/man/*.rst to suppress unnecessary section indices.
    SED_SCRIPT="-e '1,10s/^-*$//'"

    # ctags.1.rst.in -> ctags(1)
    for item in $(ls *.rst.in | sed 's|\(.*\)\.\([1-9]\)\.rst\.in|\1(\2)|')
    do
        # => -e 's/\<ctags(1)/:ref:`& <&>`/g' -e 's/\<ctags(5)/:ref:`& <&>`/g' ...
        SED_SCRIPT="$SED_SCRIPT -e 's/\<$item/:ref:\`& <&>\`/g'"
    done

    for RST_IN_FILENAME in *.rst.in
    do
        RST_OUT_FILENAME="${RST_IN_FILENAME%.in}"
        RST_OUT_FILEPATH="../docs/man/$RST_OUT_FILENAME"

        run "sed $SED_SCRIPT $RST_OUT_FILENAME > $RST_OUT_FILEPATH"
    done
}

# usage:   install_mans <DEST-DIR>
# example: install_mans /usr/local/share/man
install_mans() {
    if [ -z "$1" ] ; then
        die "$_0 install-mans <DEST-DIR>, <DEST-DIR> must be not empty."
    fi

    unset MAN_FILE_NAMES
    MAN_FILE_NAMES=$(ls *.[1-9].rst.in | sed 's|\.rst\.in$||')

    for MAN_FILE_NAME in $MAN_FILE_NAMES
    do
        if [ -f "$MAN_FILE_NAME" ] ; then
            continue
        else
            die "$PWD/$MAN_FILE_NAME not exists. you should run ${COLOR_GREEN}./build.sh rst2man [OPTIONS]${COLOR_OFF} ${COLOR_RED}first."
        fi
    done

    for MAN_FILE_NAME in $MAN_FILE_NAMES
    do
        unset MAN_FILE_SECTION_NUMBER
        MAN_FILE_SECTION_NUMBER="$(printf '%s\n' "$MAN_FILE_NAME" | sed 's|.*\.\([1-9]\)|\1|')"

        MAN_FILE_NAME_INSTALL_DIR="$1/man$MAN_FILE_SECTION_NUMBER"

        if [ !          -d "$MAN_FILE_NAME_INSTALL_DIR" ] ; then
            run install -d "$MAN_FILE_NAME_INSTALL_DIR"
        fi

        run install "$MAN_FILE_NAME" "$MAN_FILE_NAME_INSTALL_DIR/$MAN_FILE_NAME"
    done
}

print_files() {
    for item in $@
    do
        case $item in
            rst|man|pdf|html|doc|all) ;;
            *)  die "$_0 print-files <WHAT>..., unrecognized <WHAT>: $item. <WHAT> must be one or more of rst man pdf html doc all"
        esac
    done

    unset WHAT_LIST

    if [ -z "$1" ] ; then
        WHAT_LIST='rst man pdf html doc'
    else
        WHAT_LIST=$@
    fi

    for item in $WHAT_LIST
    do
        case $item in
            rst)  ls *.[1-9].rst.in | sed 's|\.rst\.in$|.rst|'  ;;
            man)  ls *.[1-9].rst.in | sed 's|\.rst\.in$||'      ;;
            pdf)  ls *.[1-9].rst.in | sed 's|\.rst\.in$|.pdf|'  ;;
            html) ls *.[1-9].rst.in | sed 's|\.rst\.in$|.html|' ;;
            doc)  ls *.[1-9].rst.in | sed 's|\.rst\.in$|.rst|'  | sed 's|^|../docs/man/|'
        esac
    done
}

# usage:   clean <WHAT>...
# example: clean man
# example: clean pdf
# example: clean html
# example: clean rst
# example: clean doc
# example: clean all
clean() {
    for item in $@
    do
        case $item in
            rst|man|pdf|html|doc|all) ;;
            *)  die "$_0 clean <WHAT>..., unrecognized <WHAT>: $item. <WHAT> must be one or more of rst man pdf html doc all"
        esac
    done

    unset WHAT_LIST

    if [ -z "$1" ] ; then
        WHAT_LIST='rst man pdf html doc'
    else
        WHAT_LIST=$@
    fi

    RM_OPTIONS=-f

    if [ "$V" = yes ] ; then
        RM_OPTIONS="$RM_OPTIONS -v"
    fi

    # here we use for loop , not use xargs , because xargs is not pre-installed in some os(As far as I known, RockyLinux, Fedora).
    for item in $WHAT_LIST
    do
        case $item in
            rst)  run "for file in \$(ls *.[1-9].rst.in | sed 's|\.rst\.in$|.rst|' ); do rm $RM_OPTIONS \$file; done" ;;
            man)  run "for file in \$(ls *.[1-9].rst.in | sed 's|\.rst\.in$||'     ); do rm $RM_OPTIONS \$file; done" ;;
            pdf)  run "for file in \$(ls *.[1-9].rst.in | sed 's|\.rst\.in$|.pdf|' ); do rm $RM_OPTIONS \$file; done" ;;
            html) run "for file in \$(ls *.[1-9].rst.in | sed 's|\.rst\.in$|.html|'); do rm $RM_OPTIONS \$file; done" ;;
            doc)  run "for file in \$(ls *.[1-9].rst.in | sed 's|\.rst\.in$|.rst|' | sed 's|^|../docs/man/|'); do rm $RM_OPTIONS \$file; done"
        esac
    done
}

help() {
    printf '%b\n' "
${COLOR_GREEN}$_0 [-x] [-v] <ACTION> [ARGUMENTS or OPTIONS]${COLOR_OFF}


${COLOR_GREEN}$_0 [-x] [-v] gen-rst${COLOR_OFF}
    ctags.1.rst.in -> ctags.1.rst

${COLOR_GREEN}$_0 [-x] [-v] rst2man  [OPTIONS]${COLOR_OFF}
    ctags.1.rst.in -> ctags.1.rst -> ctags.1

${COLOR_GREEN}$_0 [-x] [-v] rst2pdf  [OPTIONS]${COLOR_OFF}
    ctags.1.rst.in -> ctags.1.rst -> ctags.1.pdf

${COLOR_GREEN}$_0 [-x] [-v] rst2html [OPTIONS]${COLOR_OFF}
    ctags.1.rst.in -> ctags.1.rst -> ctags.1.html

${COLOR_GREEN}$_0 [-x] [-v] update-docs${COLOR_OFF}
    ctags.1.rst.in -> ctags.1.rst -> ../docs/man/ctags.1.rst

${COLOR_GREEN}$_0 [-x] [-v] install-mans <DEST-DIR>${COLOR_OFF}
    ctags.1 -> <DEST-DIR>/man1/ctags.1

${COLOR_GREEN}$_0 [-x] [-v] print-files <man|pdf|html|rst|doc|all>${COLOR_OFF}
    print the given type of to be generated files.

${COLOR_GREEN}$_0 [-x] [-v] clean <man|pdf|html|rst|doc|all>${COLOR_OFF}
    remove the given type of generated files.

${COLOR_GREEN}$_0 [-x] [-v] find-exec <COMMAND-NAME>${COLOR_OFF}
    find the given command name in PATH. if command not found, the result code will be 1.
    "
}

main() {
    while [ -n "$1" ]
    do
        case $1 in
            -x) set -x ;;
            -v) V=yes  ;;
            *)  break
        esac
        shift
    done

    case $1 in
        ''|-h|--help) help ; exit ;;
    esac

    if [ "$1" != find-exec ] ; then
        command -v sed > /dev/null || die_due_to_command_not_found sed sed https://www.gnu.org/software/sed/
        cd "$(dirname "$_0")"
    fi

    case $1 in
        gen-rst)
            gen_rst
            ;;
        *rst2man|*rst2man.py)
            gen_man $@
            ;;
        *rst2html|*rst2html.py)
            gen_html $@
            ;;
        *rst2pdf|*rst2pdf.py)
            gen_pdf $@
            ;;
        update-docs)
            update_docs
            ;;
        install-mans)
            shift
            install_mans $@
            ;;
        clean)
            shift
            clean $@
            ;;
        find-exec)
            shift
            find_exec $@
            ;;
        print-files)
            shift
            print_files $@
            ;;
        *)  die "unrecognized argument: $1"
    esac
}

_0=$0

main $@
