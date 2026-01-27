1HXCOMM Taken fom hmp-commands-info.hx in qemu
HXCOMM
HXCOMM See docs/devel/docs.rst for the format of this file.
HXCOMM
HXCOMM This file defines the contents of an array of HMPCommand structs
HXCOMM which specify the name, behaviour and help text for HMP commands.
HXCOMM Text between SRST and ERST is rST format documentation.
HXCOMM HXCOMM can be used for comments, discarded from both rST and C.
HXCOMM
HXCOMM In this file, generally SRST fragments should have two extra
HXCOMM spaces of indent, so that the documentation list item for "info foo"
HXCOMM appears inside the documentation list item for the top level
HXCOMM "info" documentation entry. The exception is the first SRST
HXCOMM fragment that defines that top level entry.

HXCOMM See docs/devel/docs.rst for the format of this file.
HXCOMM
HXCOMM This file defines the contents of an array of HMPCommand structs
HXCOMM which specify the name, behaviour and help text for HMP commands.
HXCOMM Text between SRST and ERST is rST format documentation.
HXCOMM HXCOMM can be used for comments, discarded from both rST and C.
HXCOMM
HXCOMM In this file, generally SRST fragments should have two extra
HXCOMM spaces of indent, so that the documentation list item for "info foo"
HXCOMM appears inside the documentation list item for the top level
HXCOMM "info" documentation entry. The exception is the first SRST
HXCOMM fragment that defines that top level entry.

SRST
``info`` *subcommand*
  Show various information about the system state.

ERST

    {
        .name       = "version",
        .args_type  = "",
        .params     = "",
        .help       = "show the version of QEMU",
        .cmd        = hmp_info_version,
        .flags      = "p",
    },

SRST
  ``info version``
    Show the version of QEMU.
ERST

    {
        .name       = "cryptodev",
        .args_type  = "",
        .params     = "",
        .help       = "show the crypto devices",
        .cmd        = hmp_info_cryptodev,
        .flags      = "p",
    },

SRST
  ``info cryptodev``
    Show the crypto devices.
ERST
