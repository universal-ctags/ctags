// Include module declaration in a comment

module foo # (parameter
              PAR_A = 1,
              PAR_B = 2
              )
   (/*AUTOARG*/
   // Inputs
   a, b
   );
   input a, b;

endmodule: foo

module top (/*AUTOARG*/
   // Inputs
   a, b
   );
   //begin: AUTOOUTPUTS
   /*AUTOOUTPUT*/

   //begin: AUTOINPUTS
   /*AUTOINPUT*/
   // Beginning of automatic inputs (from unused autoinst inputs)
   input                a;                      // To uut3 of foo.v
   input                b;                      // To uut3 of foo.v
   // End of automatics

   //begin: AUTOWIREs
   /*AUTOWIRE*/

   //begin: AUTOREGINPUTs
   /*AUTOREGINPUT*/

   //begin: AUTOREGs
   /*AUTOREGINPUT*/

   //begin: AUTOUNUSEDs
   wire unused_pin;
   assign unused_pin = |{
                         /*AUTOUNUSED*/
                         1'b0} ;

   //begin: AUTOTIEOFFs
   /*AUTOTIEOFF*/
   foo uut1 (
             // Inputs
             a,
             b),
     uut2 (
           .a (a),
           .b (b));
   foo uut3 (/*AUTOINST*/
             // Inputs
             .a                         (a),
             .b                         (b))  ;
   foo #(3, 4)
   uut4 (/*AUTOINST*/
             // Inputs
             .a                         (a),
             .b                         (b));
   foo #(.PAR_A (5),
         .PAR_B (6))
   uut5 (/*AUTOINST*/
         // Inputs
         .a                         (a),
         .b                         (b));
   foo uut6 [10:0]();
   foo uut7 [1:0][10:0]();
   foo uut8 () ;

   /*! Function Description
    *
    *  \param <name> <description>
    *
    *  \return <return value description>
    */

   function void func_foo(int a);

   endfunction : func_foo




endmodule: top
