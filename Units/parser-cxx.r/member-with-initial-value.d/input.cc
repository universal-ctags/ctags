typedef struct {
  int i;
} S;

class c {
  S a = {0};
  int i;
};

struct t {
  S b = {0};
  int j;
};

union u {
  S c = {0};
  int k;
};
