procedure My_Package is

  generic
    type Unsigned_Type is range <>;
  package Generic_Integer_Images is
    function Digit_To_Character (X : Unsigned_Type) return Character;
  end Generic_Integer_Images;

  package body Generic_Integer_Images is
    function Digit_To_Character (X : Unsigned_Type) return Character is
      (Character'Val (0));
  end Generic_Integer_Images;

  type Signed_Address is range
    -2**(Standard'Address_Size - 1) .. 2**(Standard'Address_Size - 1) - 1; 
begin
  null;
end My_Package;
