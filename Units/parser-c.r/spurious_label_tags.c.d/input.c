/* When run with --c-kinds=+lp option demonstrates spurious tags for statement
 * following a label
 */
static void label_forced_tags(void)
{
label1:
	proto1(arg);
label2:
	if (arg)
		proto2(arg);
label3:
	variable = 3;
	while (condition) {
	label4:
		proto3(arg);
	}
}
