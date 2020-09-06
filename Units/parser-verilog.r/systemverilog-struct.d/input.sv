  // 7.x
  struct { bit [7:0] opcode; }IR; // not defined => IR:struct
  struct packed signed { int a; } pack1; // packed:struct => pack1:struct
  struct packed unsigned { int a;} pack2; // packed:struct => pack2:struct
  union packed unsigned { logic [7:0] a; } union1; //packed:struct => union1:struct
  struct packed signed { int a; } [3:0] pack3, pack4;

