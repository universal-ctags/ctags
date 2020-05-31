/* --param-CPreProcessor._expand=1 */
#define COMMAND_LINE_SIZE 127
#define __section(section)              __attribute__((__section__(section)))
#define __initdata	__section(".init.data")
char __initdata b0[COMMAND_LINE_SIZE];
char b1[COMMAND_LINE_SIZE] __initdata;
