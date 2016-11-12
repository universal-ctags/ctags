enum E1 {
    E1_member1 = 1,
    E1_member2,
    E1_member3
};

enum {
	Anon1_member1
};

enum E1 var1;
enum { Anon2_member1 } var2;

// The following is valid only in C
enum { Anon3_member1 } function(){ return Anon3_member1; };

// The following is accepted by gcc but it is also semantically flawed since the
// function definition cannot be present in the same compilation unit.
// 
// enum { Anon4_member1 } function2();
//
// It's true that the definition for function2 *could* be in some other compilation
// unit and since the enum is technically an int, it might even work at linker level.
//
// However, it's ugly and complicates ctags parsing quite a lot.
// So for now I deliberately choose to not support it. If you find a piece of code
// that needs it, drop me a mail at <s dot stefanek at gmail dot com> :P

enum E2 { E2_member1 } var3[10];




