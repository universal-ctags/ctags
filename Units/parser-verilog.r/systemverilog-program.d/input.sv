program prog (
    input in1,
    input [11:0] in2,
    output out,
    inout iout
);

initial begin : start
    logic ok;
    longint i;
end

final begin : stop
    byte count;
end

endprogram : prog


module topmod;

program mod_prog1;
endprogram : mod_prog1

program mod_prog2;
endprogram : mod_prog2

endmodule


program;  // anounymous program inside compilation unit
    // reg reg_in_anon_prog; /* a variable cannot be defined in anonymous program */
    function func_in_anon_prog;
    endfunction
endprogram

package anon_pkg;

program;  // anounymous program inside package
    // reg reg_in_anon_prog2; /* a variable cannot be defined in anonymous program */
    function func_in_anon_prog2;
    endfunction
endprogram

endpackage

program static prog_static;
    logic logic_static;
endprogram

program automatic prog_automatic;
    logic logic_automatic;
endprogram

extern program external_program;
