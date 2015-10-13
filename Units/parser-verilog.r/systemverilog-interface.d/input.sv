interface intf (
    input in1,
    input [11:0] in2,
    output out,
    inout iout
);

logic a, b, c, d;
logic [11:0] e;

modport master (input a, b, output c, d, e);
modport slave (output a, b, input c, d, e);

endinterface : intf

interface static intf_static;
    logic logic_static;
endinterface

interface automatic intf_automatic;
    logic logic_automatic;
endinterface

extern interface external_interface;
