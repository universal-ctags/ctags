int g() {
}

template< typename Accessor, typename bEnable = void >
struct IntroduceBitDef;

template< typename Accessor >
struct IntroduceBitDef< Accessor, typename
boost::enable_if_c< CoreConfig::VERSION <= 3 >::type >
{

// class body here
// anything after this point is not parsed by ctags
 int f() { }

};
