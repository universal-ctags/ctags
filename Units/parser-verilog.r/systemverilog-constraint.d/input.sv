//
// LRM 18. Constrained random value generation
//

// 18.3 Concepts and usage
class Bus;
  rand bit[15:0] addr;
  rand bit[31:0] data;
  constraint word_align {addr[1:0] == 2'b0;}
endclass

typedef enum {low, mid, high} AddrType;
class MyBus extends Bus;
  rand AddrType atype;
  constraint addr_range
  {
    (atype == low ) -> addr inside { [0 : 15] };
    (atype == mid ) -> addr inside { [16 : 127]};
    (atype == high) -> addr inside {[128 : 255]};
  }
endclass

task exercise_bus (MyBus bus);
  int res;
  // EXAMPLE 1: restrict to low addresses
  res = bus.randomize() with {atype == low;};
  // EXAMPLE 2: restrict to address between 10 and 20
  res = bus.randomize() with {10 <= addr && addr <= 20;};
  // EXAMPLE 3: restrict data values to powers-of-two
  res = bus.randomize() with {(data & (data - 1)) == 0;};
endtask

// 18.5 Constraint blocks
// 18.5.1 External constraint blocks
class C;
  rand int x;
  constraint proto1;        // implicit form
  extern constraint proto2; // explicit form
endclass

// 18.5.2 Constraint inheritance
virtual class D;
  pure constraint Test;
endclass

class E;
  // 18.5.4 Distribution
  x != 200;
  x dist {100 := 1, 200 := 2, 300 := 5}

  // 18.5.5 Uniqueness constraints
  rand byte a[5];
  rand byte b;
  rand byte excluded;
  constraint u { unique {b, a[2:3], excluded}; }
  constraint exclusion { excluded == 5; }
endclass

// 18.5.8 Iterative constraints
// 18.5.8.1 foreach iterative constraints
class C;
  rand byte A[] ;
  constraint C1 { foreach ( A [ i ] ) A[i] inside {2,4,8,16}; }
  constraint C2 { foreach ( A [ j ] ) A[j] > 2 * j; }
endclass
