// Various bugs reported by Ivc.

PR_EXTERN(void) PL_ClearArenaPool(PLArenaPool *pool, PRInt32 pattern);

GIT_INLINE(size_t) deflateBound(z_streamp stream, size_t s)
{
	return (s + ((s + 7) >> 3) + ((s + 63) >> 6) + 11);
}

AP_DECLARE(apr_status_t) ap_os_create_privileged_process(
	const request_rec *r,
	apr_proc_t *newproc,
	const char *progname,
	const char * const *args,
	const char * const *env,
	apr_procattr_t *attr,
	apr_pool_t *p);
	
LOCAL(struct errRecord *) handleError(z_streamp stream, size_t s) MAY_EXIT
{
	if (size == 0)
		exit (1);
	return ERR_REC;
}

