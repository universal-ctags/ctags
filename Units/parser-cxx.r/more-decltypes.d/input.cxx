int a;

decltype(a) b;
typeof(a) c;
__typeof__(a) d;
__typeof(a) d0;

decltype(a) const e = 1;
typeof(a) const f = 2;
__typeof__(a) const g = 3;
__typeof(a) const g0 = 3;

static decltype(a) const h = 4;
static typeof(a) const i = 5;
static __typeof__(a) const j = 6;
static __typeof(a) const j0 = 6;

static const decltype(a) k = 7;
static const typeof(a) l = 8;
static const __typeof__(a) m = 9;
static const __typeof(a) m0 = 10;
