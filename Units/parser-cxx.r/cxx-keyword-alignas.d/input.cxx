alignas(64) int buf0[8192];
alignas(void *) int buf1[8192];
struct alignas(32) S {
  alignas(4) int i;
};

