#define INT int

template<char i, bool B = i < 10> INT x(void) { return 0; }

