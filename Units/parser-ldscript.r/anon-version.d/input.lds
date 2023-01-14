/* Taken from linux/arch/um/kernel/uml.lds.S */
/* SPDX-License-Identifier: GPL-2.0 */
#include <asm/vmlinux.lds.h>
#include <asm/page.h>

OUTPUT_FORMAT(ELF_FORMAT)
OUTPUT_ARCH(ELF_ARCH)
ENTRY(_start)
jiffies = jiffies_64;

VERSION {
  {
    local: *;
  };
}

SECTIONS
{
  /* This must contain the right address - not quite the default ELF one.*/
  PROVIDE (__executable_start = START);
  /* Static binaries stick stuff here, like the sigreturn trampoline,
   * invisibly to objdump.  So, just make __binary_start equal to the very
   * beginning of the executable, and if there are unmapped pages after this,
   * they are forever unusable.
   */
  __binary_start = START;
  /* ... */
}
