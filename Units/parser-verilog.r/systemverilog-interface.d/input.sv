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

// from UVM-1.2
class ubus_env extends uvm_env;

  // Virtual Interface variable
  protected virtual interface ubus_if vif;

  // Control properties
  protected bit has_bus_monitor = 1;
endclass : ubus_env
