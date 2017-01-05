/* Taken from info document of GNU ld.  */
SECTIONS {
       outputa 0x10000 :
         {
         all.o
         foo.o (.input1)
         }
       outputb :
         {
         foo.o (.input2)
         foo1.o (.input1)
         }
       outputc :
         {
         *(.input1)
         *(.input2)
         }
       .text : { *(.text) }
       .DATA : { [A-Z]*(.data) }
       .DATA2 : { *[A-Z](.data) }       
       .data : { *(.data) }
       .bss : { *(.bss) }
       .foo : { a.?(.foo) }
       .bar : { b.*(*.bar)  *(COMMON) }
       .baz : { b.*(*.alpha .beta.text)  *(COMMON) }
	.note : { *(.note*) }
	all :	{*(*)}
     }
	 
}

