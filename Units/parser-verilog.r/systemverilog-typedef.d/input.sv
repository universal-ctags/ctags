typedef enum type_enum;
typedef struct type_struct;
typedef union type_union;
typedef class type_class;
typedef interface class type_interface_class;
typedef type_unnamed;

typedef enum bit [1:0] {
    A = 2'b00,
    B = 2'b01,
    C = 2'b10,
    D = 2'b11
} myType;

typedef classname#(paramvalue) myType2;
