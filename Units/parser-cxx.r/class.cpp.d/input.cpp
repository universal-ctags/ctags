
// These should be ignored.
MACRO01 MACRO02() class N01;
MACRO03 class __attribute__("cool") N02;

namespace X;

// This should be reported.
MACRO03 __attribute__("fancy") __declspec(dllexport) class MY_API C01
{
	// These should be ignored.
	friend class N03;
	MACRO04 friend class N04;
	friend class X::N05;
};
