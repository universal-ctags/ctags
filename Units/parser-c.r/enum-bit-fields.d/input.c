struct s0 {
	enum {ID0} e0:1;
	int x0;
};

enum E1 {ID1};
struct s1 {
	enum E1 e1:1;
	int x1;
};

struct s2 {
	enum E2 {ID2} e2:1;
	int x2;
};

enum E3 {ID3};
struct s3 {
	enum E3 e3:1;
	int x3;
};

enum E4 {ID4};
#define bits 7
struct s4 {
	enum E4 e4_1:1, e4_2:2, e4_3:bits;
	int x4;
};
