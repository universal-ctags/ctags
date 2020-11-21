--
-- Taken from rtl/commonlib/types_util.vhd of https://github.com/sergeykhbr/riscv_vhdl
--
-----------------------------------------------------------------------------
--! @file
--! @copyright Copyright 2015 GNSS Sensor Ltd. All right reserved.
--! @author    Sergey Khabarov - sergeykhbr@gmail.com
--! @brief	    Package for common testbenches implementation.
------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library std;
use std.textio.all;

package types_util is

function strlen(s: in string) return integer;
function StringToUVector(inStr: string) return std_ulogic_vector;
function StringToSVector(inStr: string) return std_logic_vector;
function UnsignedToSigned(inUnsigned: std_ulogic_vector) return std_logic_vector;
function SignalFromString(inStr: string; ind : integer ) return std_logic;
function SymbolToSVector(inStr: string; idx: integer) return std_logic_vector;

function tost(v:std_logic_vector) return string;
function tost(v:std_logic) return string;
function tost(i : integer) return string;
procedure print(s : string);

end;

package body types_util is

  function strlen(s: in string) return integer is
    variable n: integer:=0; variable sj: integer:=s'left;
  begin
    loop
      if    sj>s'right then exit;
      elsif s(sj)=NUL  then exit; --sequential if protects sj > length
      else                  sj:=sj+1; n:=n+1;
      end if;
    end loop;
    return n;
  end strlen;

  function SignalFromString(inStr: string; ind : integer ) return std_logic is
    variable temp: std_logic := 'X';
  begin
    if(inStr(inStr'high-ind)='1')    then temp := '1';
    elsif(inStr(inStr'high-ind)='0') then temp := '0';
    end if;
    return temp;
  end function SignalFromString;


  function StringToUVector(inStr: string) return std_ulogic_vector is
    variable temp: std_ulogic_vector(inStr'range) := (others => 'X');
  begin
    for i in inStr'range loop --
      if(inStr(inStr'high-i+1)='1')    then temp(i) := '1';
      elsif(inStr(inStr'high-i+1)='0') then temp(i) := '0';
      end if;
    end loop;
    return temp(inStr'high downto 1);
  end function StringToUVector;
  -- conversion function
  
  function StringToSVector(inStr: string) return std_logic_vector is
    variable temp: std_logic_vector(inStr'range) := (others => 'X');
  begin
    for i in inStr'range loop --
      if(inStr(inStr'high-i+1)='1')    then temp(i) := '1';
      elsif(inStr(inStr'high-i+1)='0') then temp(i) := '0';
      end if;
    end loop;
    return temp(inStr'high downto 1);
  end function StringToSVector;

  function SymbolToSVector(inStr: string; idx: integer) return std_logic_vector is
    constant ss: string(1 to inStr'length) := inStr;
    variable c : integer;
    variable temp: std_logic_vector(7 downto 0) := (others => 'X');
  begin
    c := character'pos(ss(idx+1));
    for i in 0 to 7 loop --
      temp(i) := to_unsigned(c,8)(i);
    end loop;
    return temp;
  end function SymbolToSVector;
  

  function UnsignedToSigned(inUnsigned: std_ulogic_vector) 
    return std_logic_vector is
    variable temp: std_logic_vector(inUnsigned'length-1 downto 0) := (others => 'X');
    variable i: integer:=0;
  begin
    while i < inUnsigned'length loop
      if(inUnsigned(i)='1')    then temp(i) := '1';
      elsif(inUnsigned(i)='0') then temp(i) := '0';
      end if;
      i := i+1;
    end loop;
    return temp;
  end function UnsignedToSigned;


  subtype nibble is std_logic_vector(3 downto 0);

  function todec(i:integer) return character is
  begin
    case i is
    when 0 => return('0');
    when 1 => return('1');
    when 2 => return('2');
    when 3 => return('3');
    when 4 => return('4');
    when 5 => return('5');
    when 6 => return('6');
    when 7 => return('7');
    when 8 => return('8');
    when 9 => return('9');
    when others => return('0');
    end case;
  end;


  function tohex(n:nibble) return character is
  begin
    case n is
    when "0000" => return('0');
    when "0001" => return('1');
    when "0010" => return('2');
    when "0011" => return('3');
    when "0100" => return('4');
    when "0101" => return('5');
    when "0110" => return('6');
    when "0111" => return('7');
    when "1000" => return('8');
    when "1001" => return('9');
    when "1010" => return('a');
    when "1011" => return('b');
    when "1100" => return('c');
    when "1101" => return('d');
    when "1110" => return('e');
    when "1111" => return('f');
    when others => return('X');
    end case;
  end;


  function tost(v:std_logic_vector) return string is
    constant vlen : natural := v'length; --'
    constant slen : natural := (vlen+3)/4;
    variable vv : std_logic_vector(0 to slen*4-1) := (others => '0');
    variable s : string(1 to slen);
    variable nz : boolean := false;
    variable index : integer := -1;
  begin
    vv(slen*4-vlen to slen*4-1) := v;
    for i in 0 to slen-1 loop
      if (vv(i*4 to i*4+3) = "0000") and nz and (i /= (slen-1)) then
        index := i;
      else
        nz := false;
        s(i+1) := tohex(vv(i*4 to i*4+3));
      end if;
    end loop;
    if ((index +2) = slen) then return(s(slen to slen));
    else return(string'("0x") & s(index+2 to slen)); end if; --'
  end;


  function tost(v:std_logic) return string is
  begin
    if to_x01(v) = '1' then return("1"); else return("0"); end if;
  end;


  function tost(i : integer) return string is
    variable L : line;
    variable s, x : string(1 to 128);
    variable n, tmp : integer := 0;
  begin
    tmp := i;
    if i < 0 then tmp := -i; end if;
    loop
      s(128-n) := todec(tmp mod 10);
      tmp := tmp / 10;
      n := n+1;
      if tmp = 0 then exit; end if;
    end loop;
    x(1 to n) := s(129-n to 128);
    if i < 0 then return "-" & x(1 to n); end if;
    return(x(1 to n));
  end;

  procedure print(s : string) is
    variable L : line;
  begin
    L := new string'(s); writeline(output, L);
  end;

end;
