typedef struct abc {
	int def;
	int ghi;
} jkl;

jkl mno;

jkl pqr(void)
{
	return (jkl){.def = 1, .ghi = 2};
}
