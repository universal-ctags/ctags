#!/usr/bin/env gjs
//~ "use strict"

let Syn1=function(){}
Syn1.prototype = {
  await:function() { return 0x01; },
  yield:function() { return 0x02; },
  // strict-mode keywords (that are still allowed in some situations)
  let:function() { return 0x11; },
  static:function() { return 0x12; },
  implements:function() { return 0x13; },
  interface:function() { return 0x14; },
  package:function() { return 0x15; },
  private:function() { return 0x16; },
  protected:function() { return 0x17; },
  public:function() { return 0x18; },
  // not keywords
  as:function() { return 0x21; },
  from:function() { return 0x22; },
  meta:function() { return 0x23; },
  of:function() { return 0x24; },
  target:function() { return 0x25; },
  get:function() {
    return this.length;
  },
  length:0,
  set:function(n) {
    this.length=n;
  },
  async:function() {
    return -this.length;
  },
  // stuff specific to the uctags parser
  prototype:function() { return this.prototype; },
  Object:function() { return this; },
  Function:function() { return Syn1; },
  sap:function() { return "sap"; },
}

let Syn2=function(){}
Syn2.prototype = {
  await() { return 0x01; },
  yield() { return 0x02; },
  // strict-mode keywords (that are still allowed in some situations)
  let() { return 0x11; },
  static() { return 0x12; },
  implements() { return 0x13; },
  interface() { return 0x14; },
  package() { return 0x15; },
  private() { return 0x16; },
  protected() { return 0x17; },
  public() { return 0x18; },
  // not keywords
  as() { return 0x21; },
  from() { return 0x22; },
  meta() { return 0x23; },
  of() { return 0x24; },
  target() { return 0x25; },
  get() {
    return this.length;
  },
  length:0,
  set(n) {
    this.length=n;
  },
  async() {
    return -this.length;
  },
  // stuff specific to the uctags parser
  prototype() { return this.prototype; },
  Object() { return this; },
  Function() { return Syn2; },
  sap() { return "sap"; },
}

class Syn3 {
  await() { return 0x01; }
  yield() { return 0x02; }
  // strict-mode keywords (that are still allowed in some situations)
  let() { return 0x11; }
  static() { return 0x12; }
  implements() { return 0x13; }
  interface() { return 0x14; }
  package() { return 0x15; }
  private() { return 0x16; }
  protected() { return 0x17; }
  public() { return 0x18; }
  // not keywords
  async() { return 0x20; }
  as() { return 0x21; }
  from() { return 0x22; }
  get() { return 0x24; }
  meta() { return 0x23; }
  of() { return 0x24; }
  set(n) { return 0x25; }
  target() { return 0x26; }
  // stuff specific to the uctags parser
  prototype() { return 0x30 }
  Object() { return 0x31 }
  Function() { return 0x32 }
  sap() { return 0x33 }
}

let s1 = new Syn1();
let s2 = new Syn2();
console.log(s1.await(), s2.await());
console.log(s1.yield(), s2.yield());
console.log(s1.let(), s2.let());
console.log(s1.static(), s2.static());
console.log(s1.implements(), s2.implements());
console.log(s1.interface(), s2.interface());
console.log(s1.package(), s2.package());
console.log(s1.private(), s2.private());
console.log(s1.protected(), s2.protected());
console.log(s1.public(), s2.public());
console.log(s1.as(), s2.as());
console.log(s1.from(), s2.from());
console.log(s1.meta(), s2.meta());
console.log(s1.of(), s2.of());
console.log(s1.target(), s2.target());
console.log(s1.get(), s2.get());
s1.set(21);
s2.set(21);
console.log(s1.get(), s2.get());
console.log(s1.async(), s2.async());
console.log(s1.prototype(), s2.prototype());
console.log(s1.Object(), s2.Object());
console.log(s1.Function(), s2.Function());
console.log(s1.sap(), s2.sap());

/* functions */

function await() { return 0x01; }
function yield() { return 0x02; } // invalid in strict mode
// strict-mode keywords, all invalid in strict mode in this case
function let() { return 0x11; }
function static() { return 0x12; }
function implements() { return 0x13; }
function interface() { return 0x14; }
function package() { return 0x15; }
function private() { return 0x16; }
function protected() { return 0x17; }
function public() { return 0x18; }
// not keywords
function as() { return 0x21; }
function async() { return 0x22; }
function from() { return 0x23; }
function get() { return 0x24; }
function meta() { return 0x25; }
function of() { return 0x26; }
function set() { return 0x27; }
function target() { return 0x28; }
// stuff specific to the uctags parser
function prototype() { return 0x101; }
function Object() { return 0x102; }
function Function() { return 0x103; }
function sap() { return 0x104; }

console.log(await())
console.log(yield()) // invalid in strict mode
console.log(let()) // invalid in strict mode
console.log(static()) // invalid in strict mode
console.log(implements()) // invalid in strict mode
console.log(interface()) // invalid in strict mode
console.log(package()) // invalid in strict mode
console.log(private()) // invalid in strict mode
console.log(protected()) // invalid in strict mode
console.log(public()) // invalid in strict mode
console.log(as())
console.log(async())
console.log(from())
console.log(get())
console.log(meta())
console.log(of())
console.log(set())
console.log(target())
console.log(prototype())
console.log(Object())
console.log(Function())
console.log(sap())
