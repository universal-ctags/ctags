HXCOMM QMP dispatch table and documentation
HXCOMM Text between SQMP and EQMP is copied to the QMP documention file and
HXCOMM does not show up in the other formats.

SQMP
                        QMP Supported Commands
                        ----------------------

This document describes all commands currently supported by QMP.

Most of the time their usage is exactly the same as in the user Monitor, this
means that any other document which also describe commands (the manpage,
QEMU's manual, etc) can and should be consulted.

QMP has two types of commands: regular and query commands. Regular commands
usually change the Virtual Machine's state someway, while query commands just
return information. The sections below are divided accordingly.

It's important to observe that all communication examples are formatted in
a reader-friendly way, so that they're easier to understand. However, in real
protocol usage, they're emitted as a single line.

Also, the following notation is used to denote data flow:

-> data issued by the Client
<- Server data response

Please, refer to the QMP specification (QMP/qmp-spec.txt) for detailed
information on the Server command and response formats.

NOTE: This document is temporary and will be replaced soon.

1. Stability Considerations
===========================

The current QMP command set (described in this file) may be useful for a
number of use cases, however it's limited and several commands have bad
defined semantics, specially with regard to command completion.

These problems are going to be solved incrementally in the next QEMU releases
and we're going to establish a deprecation policy for badly defined commands.

If you're planning to adopt QMP, please observe the following:

    1. The deprecation policy will take efect and be documented soon, please
       check the documentation of each used command as soon as a new release of
       QEMU is available

    2. DO NOT rely on anything which is not explicit documented

    3. Errors, in special, are not documented. Applications should NOT check
       for specific errors classes or data (it's strongly recommended to only
       check for the "error" key)

2. Regular Commands
===================

Server's responses in the examples below are always a success response, please
refer to the QMP specification for more details on error responses.

EQMP

    {
        .name       = "inject-nmi",
        .args_type  = "",
        .params     = "",
        .help       = "",
        .user_print = monitor_user_noop,
        .mhandler.cmd_new = do_inject_nmi,
    },

SQMP
inject-nmi
----------

Inject an NMI on guest's CPUs.

Arguments: None.

Example:

-> { "execute": "inject-nmi" }
<- { "return": {} }

Note: inject-nmi is only supported for x86 guest currently, it will
      returns "Unsupported" error for non-x86 guest.

EQMP

    {
        .name       = "system_reset",
        .args_type  = "",
        .params     = "",
        .help       = "reset the system",
        .user_print = monitor_user_noop,
        .mhandler.cmd_new = do_system_reset,
    },

SQMP
system_reset
------------

Reset the system.

Arguments: None.

Example:

-> { "execute": "system_reset" }
<- { "return": {} }

EQMP
