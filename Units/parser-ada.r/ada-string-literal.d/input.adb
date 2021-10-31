with Ada.Text_IO;
procedure Reproduce is

  type String_T is new String;
  function "+" (Left : String) return String_T is (String_T(Left));
  function "+" (Left : String_T; Right : String) return String_T is (String_T(String (Left) & Right));

  generic
    Description : String_T;
  package Generic_G is
    procedure Go;
  end Generic_G;

  package body Generic_G is
    procedure Go is
    begin
      Ada.Text_IO.Put_Line (String (Description));
    end Go;
  end Generic_G;

  package Impl is
    procedure Go;
  end Impl;

  package body Impl is
    package Generic1 is new Generic_G (Description => +"; " +":");
    procedure Go renames Generic1.Go;
  end Impl;

begin
  Impl.Go;
end Reproduce;
