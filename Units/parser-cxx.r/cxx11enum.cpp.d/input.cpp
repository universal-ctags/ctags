
#define SOME_MACRO(a,b) 1

typedef int whatever_t;
typedef unsigned char uint8_t;

enum E1 : int { E1_member1, E1_member2 };
enum E2 : long { E2_member1 };
enum E3 : unsigned int { E3_member1 };
enum E4 : unsigned long long int { } E4_var1, E4_var2;
enum E5 : whatever_t { E5_member1 } E5_var1;

enum class EC1 { };
enum class EC2 : short { EC2_member1, EC2_member2 };

enum struct ES1 { };
enum struct ES2 : unsigned { ES2_member1, ES2_member2 };
enum struct ES3 : uint8_t { ES3_member1 = SOME_MACRO(1,whatever appears here), ES3_member2 };
enum struct ES4 : unsigned long long int { ES4_member1 = (1234 * 10) << 1 };
enum struct ES5 : signed whatever_t { } ES4_var1[10];

enum { Anon1_member1, Anon1_member2 };
enum : unsigned int { Anon2_member1 };
enum : whatever_t { Anon3_member1 };

class Class
{
	enum CE1 : int { CE1_member1 = 10, CE1_member2 = (CE1_member1 << 10) };
	enum class CEC1 : unsigned long int { CEC1_member1 };
	enum struct CES1 : int { CES1_member1 };
	virtual enum CEC1 Function1(enum CE1 parameter);
};

// Forward declarations: we ignore them.
enum F1 : whatever_t;
enum class F2;
enum class F3 : whatever_t;
enum struct F4 : unsigned int;

// variable declarations
enum E1 E1_var1;
enum EC1 EC1_var1[10][10];
//enum class EC2 EC2_var1; <-- this is NOT valid C++11



