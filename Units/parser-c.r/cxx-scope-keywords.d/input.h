struct data0 {
	void *private;
	void *public;
	void *protected;
};

struct data1 {
	void *private/* comment */;
	void *public/* comment
				 */ ;
	void *protected /* comment
					 */;
};

struct data2 {
	void *private ;
	int   public:1 ;
	void *protected __attribute__ ((aligned (8)))  ;
};

void foo(void) {
  goto private;
private:
  return;
}

void bar(void) {
  goto private ;
private :
  return;
}

void baz(void) {
	goto private/* comment */;
 private/* coment */:
  return;
}

int private;
