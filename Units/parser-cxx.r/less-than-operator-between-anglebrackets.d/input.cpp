/* "gcc input.cpp -c -std=c++11" accepts this source code. */

template<char i, bool B = (i>0)>     void f0(void) { }
template<char i, bool B = i < 10>    void f1(void) { }
template<char i, long j = (i << 10)> void f2(void) { }
template<char i, long j = i << 10>   void f3(void) { }
template<char i, long j = (i >> 10)> void f4(void) { }
template<char i, long j = (i > 10)>  void f5(void) { }
template<char i, bool B = (i <= 10)> void f6(void) { }
template<char i, bool B = (i >= 10)> void f7(void) { }
template<char i, char j, bool B = i < 10 && (30 > j)> void f8(void) { }
template<char i, char j, bool B = i < (10 && 30 > j)> void f9(void) { }
template<char i, char j, bool B = (i < 10 && 30 > j)> void f10(void) { }
template<bool b = 0 < 2 && 2 < 4> void f11(void) { }
template<typename T1, typename T2> class A;
int main(void) { return 0; }
