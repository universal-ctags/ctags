// https://github.com/universal-ctags/ctags/issues/4056

typedef enum {
  `include "test1.txt"
  OTHER_VAL1,
  `include "test2.txt"
  OTHER_VAL2
} my_enum;

my_enum e;
