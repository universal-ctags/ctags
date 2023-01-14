/* Taken from info manual of ld */
VERSION {
     VERS_1.1 {
     	 global:
     		 foo1;
     	 local:
     		 old*;
     		 original*;
     		 new*;
     };

     VERS_1.2 {
     		 foo2;
     } VERS_1.1;

     VERS_2.0 {
     		 bar1; bar2;
     	 extern "C++" {
     		 ns::*;
     		 "f(int, double)";
     	 };
     } VERS_1.2;
}
