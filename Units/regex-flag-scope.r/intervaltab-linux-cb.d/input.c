/* Taken from Linux (fs/proc/base.c) */

static const struct file_operations proc_pid_cmdline_ops = {
	.read	= proc_pid_cmdline_read,
	.llseek	= generic_file_llseek,
};

static const struct file_operations proc_lstats_operations = {
	.open		= lstats_open,
	.read		= seq_read,
	.write		= lstats_write,
	.llseek		= seq_lseek,
	.release	= single_release,
};

static const struct file_operations proc_single_file_operations = {
	.open		= proc_single_open,
	.read		= seq_read,
	.llseek		= seq_lseek,
	.release	= single_release,
};
