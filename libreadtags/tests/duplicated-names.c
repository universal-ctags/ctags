/*
 * for s in yes foldcase no; do
 *   u-ctags --quiet --options=NONE -o duplicated-names--sorted-$s.tags --kinds-C='*' --sort=$s duplicated-names.c
 * done
 *
 * u-ctags --extras=+pf --fields=+n -o - duplicated-names.c \
 * | sed -e 's/line:/line:x/' \
 * > broken-line-field.tags
 *

 */
int o;
int O (void) { return 0; }

int N;
struct n {
	int n;
};
typedef int n;
int main(int n)
{
	for (int n = 0; n < 1; n++)
	{
		int n;
		return 0;
	}
}

int m;
int M (void) { return 0; }
