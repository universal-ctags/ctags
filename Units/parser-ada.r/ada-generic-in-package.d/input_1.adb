procedure Input_1 is

  generic
    type T is private;
    type Index is range <>;
    type Array_T is array (Index range <>) of T;
    Null_Value : T;
    with function Img (A, B: T) return boolean;
  procedure Generic_Reverse_Array (X : in out Array_T);

  procedure Generic_Reverse_Array (X : in out Array_T) is
  begin
    for I in X'First .. (X'Last + X'First) / 2 loop
      declare
	Tmp     : T;
	X_Left  : T renames X (I);
	X_Right : T renames X (X'Last + X'First - I);
      begin
	Tmp     := X_Left;
	X_Left  := X_Right;
	X_Right := Tmp;
      end;
    end loop;
  end Generic_Reverse_Array;

  type Color is (None, Black, Red, Green, Blue, White);
  type Color_Array is array (Integer range <>) of Color;
  procedure Reverse_Color_Array is new Generic_Reverse_Array
     (T => Color, Index => Integer, Array_T => Color_Array, Null_Value => None);

  type Shape is (None, Circle, Triangle, Square);
  type Shape_Array is array (Integer range <>) of Shape;
  procedure Reverse_Shape_Array is new Generic_Reverse_Array
     (T => Shape, Index => Integer, Array_T => Shape_Array, Null_Value => None);

begin
  null;
end Input_1;
