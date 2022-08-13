// https://github.com/universal-ctags/ctags/issues/3457
class A;
  covergroup A0;
    coverpoint intf.x {
      bins foo = { 0 };
      bins bar = { 1 };
    }
    coverpoint intf.y {
      bins foo = { 0 };
      bins bar = { 1 };
    }
    // empty bins definition, end of ';', everything is OK.
    coverpoint intf.z;
  endgroup

  covergroup A1;
    coverpoint intf.a;
    coverpoint intf.b;
    // bins definition with '{}', but not a variable by hierarchical index, OK.
    coverpoint no_sro {
      bins foo = { 0 };
      bins bar = { 1 };
    }
  endgroup

  covergroup A2;
    coverpoint intf.m {
      bins foo = { 0 };
      bins bar = { 1 };
    }
    coverpoint intf.n {
      bins foo = { 0 };
      bins bar = { 1 };
    }
    // exceptional, index by 'this', OK.
    coverpoint this.t {
      bins foo = { 0 };
      bins bar = { 1 };
    }
  endgroup

  covergroup A3;
    coverpoint intf.r {
      bins foo = { 0 };
      bins bar = { 1 };
    }
    coverpoint intf.s {
      bins foo = { 0 };
      bins bar = { 1 };
    }
    // contain multiple hierarchical variable, OK.
    cross intf.u, intf.v {
      bins foo = { 0 };
      bins bar = { 1 };
    }
  endgroup

  covergroup A4;
    coverpoint intf.p;
    // last definition, hierarchical variable, bins definition with '{}'
    // meet at the same time, subsequent parsers will all go WRONG.
    coverpoint intf.q {
      bins foo = { 0 };
      bins bar = { 1 };
    }
  endgroup

  // Error Scope >> covergroup:A.A4
  covergroup A5;
    coverpoint foo;
    coverpoint bar;
  endgroup
endclass

// Error Scope >> covergroup:A.A4
class B;
  int one, two;
  function new();
  endfunction
endclass

// Error Scope >> covergroup:A.A4
typedef enum { FOO, BAR } E;