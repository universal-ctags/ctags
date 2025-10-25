.. _ctags_lang-verilog(7):

======================================================================
ctags-lang-verilog
======================================================================

The man page about SystemVerilog/Verilog parser for Universal Ctags


:Version: 6.2.1
:Manual group: Universal Ctags
:Manual section: 7

SYNOPSIS
--------
|	**ctags** ... [--kinds-systemverilog=+Q] [--fields-SystemVerilog=+{parameter}] ...
|	**ctags** ... [--fields-Verilog=+{parameter}] ...

    +---------------+---------------+-------------------+
    | Language      | Language ID   | File Mapping      |
    +===============+===============+===================+
    | SystemVerilog | SystemVerilog | .sv, .svh, svi    |
    +---------------+---------------+-------------------+
    | Verilog       | Verilog       | .v                |
    +---------------+---------------+-------------------+

DESCRIPTION
-----------
This man page describes about the SystemVerilog/Verilog parser for Universal Ctags.
SystemVerilog parser supports IEEE Std 1800-2017 keywords.
Verilog parser supports IEEE Std 1364-2005 keywords.

Supported Kinds
~~~~~~~~~~~~~~~

.. code-block:: console

	$ ctags --list-kinds-full=SystemVerilog
	#LETTER NAME       ENABLED REFONLY NROLES MASTER DESCRIPTION
	A       assert     yes     no      0      NONE   assertions (assert, assume, cover, restrict)
	C       class      yes     no      0      NONE   classes
	E       enum       yes     no      0      NONE   enumerators
	H       checker    yes     no      0      NONE   checkers
	I       interface  yes     no      0      NONE   interfaces
	K       package    yes     no      0      NONE   packages
	L       clocking   yes     no      0      NONE   clocking
	M       modport    yes     no      0      NONE   modports
	N       nettype    yes     no      0      NONE   nettype declarations
	O       constraint yes     no      0      NONE   constraints
	P       program    yes     no      0      NONE   programs
	Q       prototype  no      no      0      NONE   prototypes (extern, pure)
	R       property   yes     no      0      NONE   properties
	S       struct     yes     no      0      NONE   structs and unions
	T       typedef    yes     no      0      NONE   type declarations
	V       covergroup yes     no      0      NONE   covergroups
	b       block      yes     no      0      NONE   blocks (begin, fork)
	c       constant   yes     no      0      NONE   constants (parameter, specparam, enum values)
	d       define     yes     no      0      NONE   text macros
	e       event      yes     no      0      NONE   events
	f       function   yes     no      0      NONE   functions
	i       instance   yes     no      0      NONE   instances of module or interface
	l       ifclass    yes     no      0      NONE   interface class
	m       module     yes     no      0      NONE   modules
	n       net        yes     no      0      NONE   net data types
	p       port       yes     no      0      NONE   ports
	q       sequence   yes     no      0      NONE   sequences
	r       register   yes     no      0      NONE   variable data types
	t       task       yes     no      0      NONE   tasks
	w       member     yes     no      0      NONE   struct and union members

Note that ``prototype`` (``Q``) is disabled by default.

.. code-block:: console

	$ ctags --list-kinds-full=Verilog
	#LETTER NAME     ENABLED REFONLY NROLES MASTER DESCRIPTION
	b       block    yes     no      0      NONE   blocks (begin, fork)
	c       constant yes     no      0      NONE   constants (parameter, specparam)
	d       define   yes     no      0      NONE   text macros
	e       event    yes     no      0      NONE   events
	f       function yes     no      0      NONE   functions
	i       instance yes     no      0      NONE   instances of module
	m       module   yes     no      0      NONE   modules
	n       net      yes     no      0      NONE   net data types
	p       port     yes     no      0      NONE   ports
	r       register yes     no      0      NONE   variable data types
	t       task     yes     no      0      NONE   tasks

Supported Language Specific Fields
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: console

	$ ctags --list-fields=Verilog
	#LETTER NAME      ENABLED LANGUAGE JSTYPE FIXED DESCRIPTION
	-       parameter no      Verilog  --b    no    parameter whose value can be overridden.
	$ ctags --list-fields=SystemVerilog
	#LETTER NAME      ENABLED LANGUAGE      JSTYPE FIXED DESCRIPTION
	-       parameter no      SystemVerilog --b    no    parameter whose value can be overridden.

``parameter`` field
....................

If the field ``parameter`` is enabled, a field ``parameter:`` is added on a parameter whose
value can be overridden on an instantiated module, interface, or program.
This is useful for a editor plugin or extension to enable auto-instantiation of modules with
parameters which can be overridden.

.. code-block:: console

    $ ctags ... --fields-Verilog=+{parameter} ...
    $ ctags ... --fields-SystemVerilog=+{parameter} ...

On the following source code fields ``parameter:`` are added on
parameters ``P*``, not on ones ``L*``.  Note that ``L4`` and ``L6`` is declared by
``parameter`` statement, but fields ``parameter:`` are not added,
because they cannot be overridden.

"input.sv"

.. code-block:: systemverilog

	// compilation unit scope
	parameter L1 = "synonym for the localparam";

	module with_parameter_port_list #(
		P1,
		localparam L2 = P1+1,
		parameter P2)
		( /*port list...*/ );
		parameter  L3 = "synonym for the localparam";
		localparam L4 = "localparam";
		// ...
	endmodule

	module with_empty_parameter_port_list #()
		( /*port list...*/ );
		parameter  L5 = "synonym for the localparam";
		localparam L6 = "localparam";
		// ...
	endmodule

	module no_parameter_port_list
		( /*port list...*/ );
		parameter  P3 = "parameter";
		localparam L7 = "localparam";
		// ...
	endmodule

.. code-block:: console

	$ ctags -uo - --fields-SystemVerilog=+{parameter} input.sv
	L1	input.sv	/^parameter L1 = "synonym for the localparam";$/;"	c	parameter:
	with_parameter_port_list	input.sv	/^module with_parameter_port_list #($/;"	m
	P1	input.sv	/^	P1,$/;"	c	module:with_parameter_port_list	parameter:
	L2	input.sv	/^	localparam L2 = P1+1,$/;"	c	module:with_parameter_port_list
	P2	input.sv	/^	parameter P2)$/;"	c	module:with_parameter_port_list	parameter:
	L3	input.sv	/^	parameter  L3 = "synonym for the localparam";$/;"	c	module:with_parameter_port_list
	L4	input.sv	/^	localparam L4 = "localparam";$/;"	c	module:with_parameter_port_list
	with_empty_parameter_port_list	input.sv	/^module with_empty_parameter_port_list #()$/;"	m
	L5	input.sv	/^	parameter  L5 = "synonym for the localparam";$/;"	c	module:with_empty_parameter_port_list
	L6	input.sv	/^	localparam L6 = "localparam";$/;"	c	module:with_empty_parameter_port_list
	no_parameter_port_list	input.sv	/^module no_parameter_port_list$/;"	m
	P3	input.sv	/^	parameter  P3 = "parameter";$/;"	c	module:no_parameter_port_list	parameter:
	L7	input.sv	/^	localparam L7 = "localparam";$/;"	c	module:no_parameter_port_list

Supported Roles
~~~~~~~~~~~~~~~

.. code-block:: console

	$ ./ctags --list-roles=SystemVerilog
	#KIND(L/N) NAME ENABLED DESCRIPTION
	m/module   decl on      declaring instances

	$ ./ctags --list-roles=Verilog
	#KIND(L/N) NAME ENABLED DESCRIPTION
	m/module   decl on      declaring instances

The parser extracts names of modules used in instance declarations as
reference tags. ``decl`` is the role for the tags. See "TAG ENTRIES"
section of :ref:`ctags(1) <ctags(1)>` about reference tags and roles.

.. warning::

   The support for references in Universal Ctags is still
   experimental; the names of the roles may be changed in the future.

TIPS
~~~~

If you want to map files ``*.v`` to SystemVerilog, add
``--langmap=SystemVerilog:.v`` option.

KNOWN ISSUES
---------------------------------------------------------------------

See https://github.com/universal-ctags/ctags/issues/2674 for more information.

VERSIONS
--------

Change since "0.0"
~~~~~~~~~~~~~~~~~~

* New kind ``define``

SEE ALSO
--------

- :ref:`ctags(1) <ctags(1)>`
- :ref:`ctags-client-tools(7) <ctags-client-tools(7)>`
- Language Reference Manuals (LRM)

   - IEEE Standard for SystemVerilog — Unified Hardware Design, Specification, and
     Verification Language, IEEE Std 1800-2017,
     https://ieeexplore.ieee.org/document/8299595
   - IEEE Standard for Verilog Hardware Description Language, IEEE Std 1364-2005,
     https://ieeexplore.ieee.org/document/1620780
