HXCOMM Use DEFHEADING() to define headings in both help text and texi
HXCOMM Text between STEXI and ETEXI are copied to texi version and
HXCOMM discarded from C version
HXCOMM DEF(command, args, callback, arg_string, help) is used to construct
HXCOMM monitor commands
HXCOMM HXCOMM can be used for comments, discarded from both texi and C

STEXI
@table @option
ETEXI

    {
        .name       = "help|?",
        .args_type  = "name:s?",
        .params     = "[cmd]",
        .help       = "show the help",
        .mhandler.cmd = do_help_cmd,
    },

STEXI
@item help or ? [@var{cmd}]
@findex help
Show the help for all commands or just for command @var{cmd}.
ETEXI

DEF("version", 0, QEMU_OPTION_version,
    "-version        display version information and exit\n", QEMU_ARCH_ALL)
STEXI
@item -version
@findex -version
Display version information and exit
ETEXI

DEF("check", img_check,
    "check [-q] [-f fmt] [--output=ofmt] [-r [leaks | all]] [-T src_cache] filename")
STEXI
@item check [-q] [-f @var{fmt}] [--output=@var{ofmt}] [-r [leaks | all]] [-T @var{src_cache}] @var{filename}
ETEXI

DEF("create", img_create,
    "create [-q] [-f fmt] [-o options] filename [size]")
STEXI
@item create [-q] [-f @var{fmt}] [-o @var{options}] @var{filename} [@var{size}]
ETEXI

SQMP
quit
----

Quit the emulator.

Arguments: None.

Example:

-> { "execute": "quit" }
<- { "return": {} }

EQMP
