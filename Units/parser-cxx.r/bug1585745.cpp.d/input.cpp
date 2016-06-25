// Despite the weird whitespace, all these destructors should be recognized and tagged.
Class1::~Class1() { }
Class2::~ Class2() { }
Class3:: ~Class3() { }
Class4:: ~ Class4() { }
class Class5 {
    public: ~ Class5() { }
};
