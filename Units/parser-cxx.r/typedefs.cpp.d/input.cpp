// Note that in this file spacing matters: ctags should normalize it.

// Auxiliary declarations
struct AStruct {};
class AClass {};
union AUnion {};
enum AEnum { E1 };
template<typename AType> class ATemplate1;
template<typename AType1,typename AType2> class ATemplate2;

// class/struct/union/enum typedefs
typedef struct AStruct T01;
typedef class AClass T02;
typedef union AUnion T03;
typedef enum AEnum T04;

// typedefs with anonymous types
typedef struct { int a; } T11;
typedef union { int a; int b; } T12;
typedef enum { E2 } T13;

// plain types, pointers etc
typedef int* T21;
typedef const AClass* *  T22;

// function pointers
typedef int (*T31) (int &,int , AUnion *);
typedef AClass &(* T32)(AClass &);

// arrays
typedef int T41 [ 10];

// stuff containing template instantiations
typedef ATemplate1<int > T51;
typedef ATemplate1< unsigned short int> T52;
typedef ATemplate1<ATemplate2 < AStruct,AClass> > T53;
typedef ATemplate1<int > (*T54)();

// typedefs within a class
template<typename Type> class Container
{
public:
	typedef Type::iterator1 T61;
	typedef typename Type :: iterator2 T62;
	typedef ATemplate<Type> T63;
	typedef int (*T64)(ATemplate< Type> &);
};

// This should appear as typedef but have not typeref since we can't resolve macros
#define DECLPOINTER(name) name *

typedef DECLPOINTER(struct AStruct) T71;
