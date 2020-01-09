// Note that in this file spacing matters: ctags should normalize it.

// Auxiliary declarations
struct AStruct {};
class AClass {};
union AUnion {};
enum AEnum { E1 };
template<typename AType> class ATemplate1;
template<typename AType1,typename AType2> class ATemplate2;

// class/struct/union/enum typedefs
typedef struct AStruct T001;
typedef class AClass T002;
typedef union AUnion T003;
typedef enum AEnum T004;

// typedefs with anonymous types
typedef struct { int a; } T101;
typedef union { int a; int b; } T102;
typedef enum { E2 } T103;

// plain types, pointers etc
typedef int* T201;
typedef const AClass* *  T202;

// function pointers and functions
typedef int (*T301) (int &,int , AUnion *);
typedef AClass &(* T302)(AClass &);
typedef struct AStruct (*T303)(struct AStruct &);
typedef int (T304)(int);
typedef struct AStruct (T305)(int);
typedef union AUnion & (T306)(int);
typedef AClass (T307)(int);
typedef enum AEnum (T308)(int);
typedef int T309(int);
typedef int T310(AUnion *);
#if 0
	// broken input (to make coveralls happy)
	typedef int int(int);
#endif

// arrays
typedef int T401 [ 10];

// stuff containing template instantiations
typedef ATemplate1<int > T501;
typedef ATemplate1< unsigned short int> T502;
typedef ATemplate1<ATemplate2 < AStruct,AClass> > T503;
typedef ATemplate1<int > (*T504)();

// typedefs within a class
template<typename Type> class Container
{
public:
	typedef typename Type::iterator1 T601;
	typedef typename Type :: iterator2 T602;
	typedef ATemplate1<AClass> T603;
	typedef int (*T604)(ATemplate1<AUnion> &);
};

// This should appear as typedef but have not typeref since we can't resolve macros
#define DECLPOINTER(name) name *

typedef DECLPOINTER(struct AStruct) T701;

// Multiple typedefs
typedef struct _ABC {
	int a;
	int b;
} T801, *T802;

typedef int T803, *T804, **T805;
typedef ATemplate2< ATemplate2< ATemplate1<int *>, AClass>, AStruct> T806, **T807;
typedef int T808, *T809, (&T810)(int, int *), T811[10], &T812;

// Typedefs with const/volatile prefix
typedef const struct AStruct1 {
	int a;
} T901, *T902;

typedef const struct AStruct1 T903;
typedef const struct AStruct1 * T904,* T905;
typedef volatile struct AStruct1 * T906;
typedef const enum AEnum T907, &T908;
