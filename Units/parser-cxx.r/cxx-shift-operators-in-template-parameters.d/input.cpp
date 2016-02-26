template <int P> class c {};
c< 8 > bVar;
c< 1<<8 > aVar;
template<int X> f12( c< 1<<X> & aVar) { };
