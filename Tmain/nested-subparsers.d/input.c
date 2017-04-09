DEFINE_EVENT(a);
DEFINE_EVENT(b);
DEFINE_HOOK(h);
DEFINE_HOOK(i);
int
main(void)
{
	LOAD_PLUGIN(mylib.so, isearch);
	int u __attribute__((unused));
}
