// Compile with gcc -std=c++11 input.cpp

#include <string>
#include <memory>

#define EMPTY_MACRO

namespace n01 {

	std::string g01 { "" };
	
	auto f01(std::string p01) -> std::string {
		// initialization of unnamed temporary (makes no sense to report it, just don't think it's a function)
		std::string { "" };
	
		// list initialisation for local
		std::string l01 { "" };
		
		// constructor call for list initialisation
		new std::string { "" };

		// copy list initialisation for local
		std::string l02 = { "" };

		// return list initalizer
		return { "" };
	}
	
	class C01
	{
	public:
		std::string m01 { "" };
		std::string m02;
		std::string m03;
	
		C01(std::string p02)
			: m02 { "" }
		{
		}
	
		C01()
			: m02 { "" },
			m03 { "" }
		{
			std::string l03 { "" };
			f01({ "" });
			std::string l04 { "" };
			C01 l05{ "" };
		}
		
		std::string f02() const EMPTY_MACRO { }
		int m04 { 0 };
		
		std::string f03() const EMPTY_MACRO { }
		std::string f04() const EMPTY_MACRO { }
		int m05 { 0 };
		
		std::string f05() const EMPTY_MACRO { }
		std::string f06() const { }
		std::string f07() { }
	};
}


