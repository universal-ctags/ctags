typedef enum              fwd_type_enum;
typedef struct            fwd_type_struct;
typedef union             fwd_type_union;
typedef class             fwd_type_class;
typedef interface class   fwd_type_interface_class;
typedef                   fwd_type;

typedef enum           {no, yes} type_enum;
typedef enum bit       {W, X}    type_enum_bit;
typedef enum bit [1:0] {Y, Z}    type_enum_bit2;

typedef struct {
  real       struct_real;
  bit  [1:0] struct_bit;
  } type_struct;

typedef union {
  real       union_real;
  bit  [1:0] union_bit;
  } type_union;

typedef union packed {
  bit  [1:0] union_bit1;
  bit  [1:0] union_bit2;
  } type_union_packed;

typedef union tagged {
  void Invalid;
  int Valid;
  } type_union_tagged;

typedef struct {
  real       struct_real;
  union {
    int i;
    bit b;
    } struct_union;
  } type_struct_union;

typedef struct packed unsigned {
  logic [7:0] upper;
  logic [7:0] lower;
} type_strcut_packed_unsigned;

typedef      bit                 type_bit;
typedef      bit [1:0]           type_bit_bus;
typedef      bit [1:0]           type_bit_bus_array [2:0];
typedef      logic signed [7:0]  type_logic_signed_vec;
typedef      bit unsigned [7:0]  type_bit_unsigned_vec;

typedef enum int unsigned{
    cond0 = 0, cond1 = 1, cond2 = 2
    } type_int_unsigned;

typedef enum bit [1:0] {
    A = 2'b00,
    B = 2'b01,
    C = 2'b10,
    D = {1'b1, 1'b1}
} type_enum_bit_bus_defined_values;

// enum with user defined type
typedef  bit [7:0]  foo_t ;
typedef enum foo_t {
    X = 'h0000,
    Y = 'h0001,
    Z = -1
} bar_e;

typedef classname#(paramvalue) type_class;

// from UVM-1.2
typedef class `UVM_CORESERVICE_TYPE;
typedef class `UVM_CORESERVICE_TYPE(arg);
