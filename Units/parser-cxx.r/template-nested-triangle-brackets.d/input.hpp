template <int,
         class _Comp0=less<int>,
         class _Comp1=less<pair<int, int> > >
class Test : public set<int> {
  typedef int xxx;
};
// Taken from #1750 submitted by @tuarba.
