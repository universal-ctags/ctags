SYSCALL_DEFINE3(faccessat, int, dfd, const char __user *, filename, int, mode)
{
        return do_faccessat(dfd, filename, mode, 0);
}

SYSCALL_DEFINE3(fchmodat, int, dfd, const char __user *, filename,
                umode_t, mode)
{
        return do_fchmodat(dfd, filename, mode, 0);
}

SYSCALL_DEFINE3(
	/*
	 * noise
	 */
	mysyscall, int, dfd,
	const char __user *, filename,
                umode_t, mode)
{
        return 0;
}

#define INJECT_MEMBERS(m0, m1) \
	int m0; \
	int padding; \
	int m1

struct P {
	INJECT_MEMBERS(
		x
		,
		y);
};
