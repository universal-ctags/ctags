-- Test labels
-- See: ARM 5.1 :  Simple and Compound Statements - Sequences of Statements
--
-- A label should come before a simple statement or a compound statment.
--
-- Check if file compiles: gnatmake -gnatc Units/parser-ada.r/ada-label.d/input.adb
--
with Ada.Text_IO;

procedure Input is
  X : Integer;
begin
  -- ARM 5.1(4/2) : Simple statements
  --   null statement
  <<Null_Before_Label>> null;  <<Null_After_Label>>

  --   assignment
  <<Assignment_Before_Label>> X := 1; <<Assignment_After_Label>>

  for I in 1 .. X loop
    if I > 1 then
      --   exit statement
      <<Exit_Before_Label>> exit; <<Exit_After_Label>>
    elsif I > 2 then
      --   goto statement
      <<Goto_Before_Label>> goto Assignment_Before_Label;  <<Goto_After_Label>>
    end if;
  end loop;
  
  --   procedure call statement
  <<Procedure_Call_Before_Label>> Ada.Text_IO.Put_Line ("Hello World"); <<Procedure_Call_After_Label>>
  if X > 1 then
    --   simple return statement
    <<Return_Before_Label>> return; <<Return_After_Label>>
  end if;

  declare
    task type Server is
      entry Start(Nr : in Natural);
    end Server;

    task body Server is
      Iden : Natural;
    begin
      <<Accept_Before_Label>> accept Start(Nr : in Natural) do
        Iden := Nr;
	<<Requeue_Before_Label>> requeue Start; <<Requeue_After_Label>>
      end Start; <<Accept_After_Label>>
      Ada.Text_IO.Put_Line ("Working...");
    end;

    My_Task : Server;
  begin
    <<Entry_Before_Label>> My_Task.Start (1); <<Entry_After_Label>>
    <<Delay_Before_Label>> delay 100.0; <<Delay_After_Label>>
    <<Abort_Before_Label>> abort My_Task; <<Abort_After_Label>>
    if X > 1 then
      <<Select_Before_Statement>> select
        My_Task.Start (1);
      or
        delay 10.0;
      end select; <<Select_After_Statement>>
    end if;
  end;
  <<Code_Before_Label>>
  Ada.Text_IO.Put_Line ("Code 1");
  Ada.Text_IO.Put_Line ("Code 2"); 
  <<Code_After_Label>>
  <<Multiple_1_Before_Label>> <<Multiple_2_Before_Label>> <<Multiple_3_Before_Label>> null; <<Multiple_1_After_Label>> <<Multiple_2_After_Label>> <<Multiple_3_After_Label>>
  <<Raise_Before_Label>> raise constraint_error; <<Raise_After_Label>>

  -- ARM 5.1(5/2) Compound statements
  --   if statement
  <<If_Before_Label>> if X>1 then
    Ada.Text_IO.Put_Line ("X>1");
  elsif X > 2 then
    Ada.Text_IO.Put_Line ("X > 2");
  end if; <<If_After_Label>> 

  --   case statement
  <<Case_Before_Label>> case X is
    when 1 => <<Case_Code_Before_Label>> Ada.Text_IO.Put_Line ("1"); <<Case_Code_After_Label>> 
    when 2 => Ada.Text_IO.Put_Line ("2");
    when others => Ada.Text_IO.Put_Line ("2");
  end case; <<Case_After_Label>>

  --   loop statement
  <<Loop_Before_Label>> loop	
    <<Loop_Code_Before_Label>> Ada.Text_IO.Put_Line ("1"); <<Loop_Code_After_Label>>
    exit;
  end loop; <<Loop_After_Label>>

  <<While_Before_Label>> while X > 1 loop
    <<While_Code_Before_Label>> X := X + 1; <<While_Code_After_Label>>
  end loop; <<While_After_Label>>
  
  <<For_Before_Label>> for I in 1 .. X loop
    <<For_Code_Before_Label>> Ada.Text_IO.Put_Line (X'Image); <<For_Code_After_Label>>  
  end loop; <<For_After_Label>>
  
  --   block statement
  <<Declare_Before_Label>> declare
    Y : Integer;
  begin
    Y := X + 1;
    Ada.Text_IO.Put_Line (Y'Image);
  end; <<Declare_After_Label>>

  <<Begin_Before_Label>> begin
    null;
  end; <<Begin_After_Label>>

  --   extended return statement
  declare
    type Coordinate is record
      X : Integer;
      Y : Integer; 
    end record;
    
    function Extended_Return return Coordinate is
    begin
      <<Return_Before_Label>> return Coord : Coordinate do
        Coord.X := 10;
	Coord.Y := 20;
      end return; <<Return_After_Label>>
    end Extended_Return;
  begin
    null;
  end;

  --   accept statement  See above <<Accept_Before_Label>> and <<Accept_After_Label>>
  --   select statement  See above <<Select_Before_Statement>> and <<Select_After_Statement>> 
end Input;