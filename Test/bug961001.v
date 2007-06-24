/*
 * In Verilog, the following two lines are both valid syntax:
 *
 * `define GUEST
 * `define GUEST <value>
 *
 * The first defines "GUEST" as existing, but with no assigned 
 * value.  The second defines "GUEST" as existing with an 
 * assigned value.  Ctags55 correctly handles both cases, but 
 * Ctags551 - Ctags554 only handles the `define with value 
 * correctly.  Here is some test code to demonstrate this:
 */
`define HOSTA
`define HOSTB
`define HOSTC
`define HOSTD

`define GUESTA 1
`define GUESTB 2
`define GUESTC 3
`define GUESTD 4
/*
 * Ctags55 correctly generates a tag for all `defines in the 
 * code, but Ctags554 does not generate tags for "HOSTB" 
 * or "HOSTD".
 */
