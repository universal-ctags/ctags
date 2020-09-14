enum  MUTEX_E { mutex_a, mutex_b, mutex };

#define mutex mutex

int mutex;

struct MUTEX_S {
	int mutex;
};

typedef mutex int;
