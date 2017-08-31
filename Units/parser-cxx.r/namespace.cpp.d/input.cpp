
namespace {
	void anon_f() { };
	
	namespace {
		void anon_anon_f() { };
	}
}

namespace a1 {
	void a1_f() { }

	namespace a2 {
		void a1_a2_f() { }
		
		namespace a3 {
			void a1_a2_a3_f() { };
		}
	}
	
	namespace {
		void a1_anon_f() { };
	}
}

namespace b1::b2 {
	void b1_b2_f() { };

	namespace b3::b4 {
		void b1_b2_b3_b4_f() { };
	}
}

inline namespace c1 {
	namespace c2 {
		void c1_c2_f() { };
	}
}

inline namespace d1::d2 {
	void d1_d2_f() { };
	
	inline namespace d3::d4 {
		void d1_d2_d3_d4_f() { }
	}
}

namespace e1 = a1::a2::a3;

namespace e2 = b1::b2 __attribute__((abi_tag("blah")));

namespace f1 _SOME_MACRO(default)
{
}

namespace f2::f3 _SOME_MACRO("blah","foo")
{
}

namespace z1 { };


