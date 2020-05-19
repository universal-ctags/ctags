#define defStruct(PREFIX,X) struct PREFIX##X
#define begin {
#define defField(PREFIX,T, F) T PREFIX##F;
#define endf ;
#define ends };

#define mydefs(X) defStruct(my_,X)
#define mydeff(T,Y) defField(my_,T,Y)

defStruct(your_,point) begin
  defField(your_,int, x) endf
  defField(your_,int, y) endf
ends

mydefs(point3d) begin
  mydeff(int, x) endf
  mydeff(int, y) endf
  mydeff(int, z) endf
ends
