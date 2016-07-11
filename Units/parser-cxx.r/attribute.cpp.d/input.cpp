

int v1 __attribute__((aligned(16)));
unsigned int v2 __attribute__((aligned(8),weak,visibility("hidden")));
char v3[10000] __attribute__((section("STACK"))) = { 0 };
char *__attribute__((aligned(8))) *v4;

struct s1
{
	char s1v1;
	int s1v2[2] __attribute__((packed));
};

struct s1 __attribute__((vector_size(16))) v5;

struct s2
{
	short s2v1[3];
} __attribute__ ((aligned (8)));

struct __attribute__((__packed__)) s3
{
	char s3v1 __attribute__((deprecated));
	char s3v2;
};

typedef int t1 __attribute__ ((aligned (8)));
typedef int t2 __attribute__ ((deprecated));

enum e1 {
	e1e1 __attribute__((deprecated)),
	e1e2
};

__attribute__((noreturn)) void p1(void),
	__attribute__((format(printf, 1, 2))) p2 (const char *, ...),
	p3 (void);

void (__attribute__((noreturn)) ****p4) (void) __attribute__((deprecated));
void p5() __attribute__ ((weak, alias ("p4")));

int __attribute__((visibility("protected")))
	f1(int f1p1,int f1p2 __attribute__((unused)),int f1p3)
		__attribute__ ((warn_unused_result,always_inline,deprecated))
{
l1:
	__attribute__((cold, unused)); /* Semi-colon is required here */
	return -1;
}