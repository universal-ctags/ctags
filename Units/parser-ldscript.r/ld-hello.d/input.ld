OUTPUT_FORMAT("elf64-x86-64", "elf64-x86-64", "elf64-x86-64")
OUTPUT_ARCH(i386:x86-64)
ENTRY(_start)

SECTIONS
{
  . = 0x400000 + SIZEOF_HEADERS;
  .text : {
    _start = .;
    /* The syscall calling convention for linux on x86_64. */
    /* Set the following registers and call "syscall" instruction. */
    /*   %rax: syscall number */
    /*   %rdi: arg0 */
    /*   %rsi: arg1 */
    /*   %rdx: arg2 */
    /*   %r10: arg3 */
    /*   %r8:  arg4 */
    /*   %r9:  arg5 */

    /* set the syscall number of write(2) */
    /* mov $0x1,%rax */
    BYTE(0x48);
    BYTE(0xc7);
    BYTE(0xc0);
    BYTE(0x01);
    BYTE(0x00);
    BYTE(0x00);
    BYTE(0x00);

    /* set arg0(== 1 == STDOUT_FILENO) of write(2) */
    /* mov $0x1,%rdi */
    BYTE(0x48);
    BYTE(0xc7);
    BYTE(0xc7);
    BYTE(0x01);
    BYTE(0x00);
    BYTE(0x00);
    BYTE(0x00);

    /* set arg1 of write(2) */
    /* mov $message,$rsi */
    BYTE(0x48);
    BYTE(0xc7);
    BYTE(0xc6);
    LONG(message);

    /* set arg2 (length of message) of write(2) */
    /* mov $0xe,%rdx */
    BYTE(0x48);
    BYTE(0xc7);
    BYTE(0xc2);
    BYTE(0x0e);
    BYTE(0x00);
    BYTE(0x00);
    BYTE(0x00);

    /* call write(2) */
    /* syscall */
    BYTE(0x0f);
    BYTE(0x05);

    /* set the syscall number of exit(2) */
    /* mov $0x3c,%rax */
    BYTE(0x48);
    BYTE(0xc7);
    BYTE(0xc0);
    BYTE(0x3c);
    BYTE(0x00);
    BYTE(0x00);
    BYTE(0x00);

    /* set arg0 of exit(2) */
    /* mov $9x9,%rdi */
    BYTE(0x48);
    BYTE(0xc7);
    BYTE(0xc7);
    BYTE(0x00);
    BYTE(0x00);
    BYTE(0x00);
    BYTE(0x00);

    /* call exit(2) */
    /* syscall */
    BYTE(0x0f);
    BYTE(0x05);
  }

  .rodata : {
    message = .;
    /* "Hello, world!\n" */
    BYTE(0x48)
    BYTE(0x65)
    BYTE(0x6c)
    BYTE(0x6c)
    BYTE(0x6f)
    BYTE(0x2c)
    BYTE(0x20)
    BYTE(0x77)
    BYTE(0x6f)
    BYTE(0x72)
    BYTE(0x6c)
    BYTE(0x64)
    BYTE(0x21)
    BYTE(0x0a)
  }
}
