package body badger is
end package body;

package body badger2 is
end package body badger2;

-- Incorporates Errata 5.4

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity accumulator is port (
  a: in std_logic_vector(3 downto 0);
  clk, reset: in std_logic;
  accum: out std_logic_vector(3 downto 0)
  );
end accumulator;

architecture simple of accumulator is

signal accumL: unsigned(3 downto 0);

begin

  accumulate: process (clk, reset) begin
    if (reset = '1') then
      accumL <= "0000";
    elsif (clk'event and clk= '1') then
      accumL <= accumL + to_unsigned(a);
    end if;
  end process;

  accum <= std_logic_vector(accumL);

end simple;
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;

entity adder is port (
  a,b : in std_logic_vector (15 downto 0);
  sum: out std_logic_vector (15 downto 0)
  );
end adder;

architecture dataflow of adder is

begin

  sum <= a + b;

end dataflow;
library IEEE;
use IEEE.std_logic_1164.all;

entity pAdderAttr is
  generic(n : integer := 8);
  port (a    : in std_logic_vector(n - 1 downto 0);
        b    : in std_logic_vector(n - 1 downto 0);
        cin  : in std_logic;
        sum  : out std_logic_vector(n - 1 downto 0);
        cout : out std_logic);
end pAdderAttr;


architecture loopDemo of pAdderAttr is

begin

  process(a, b, cin)
    variable carry: std_logic_vector(sum'length downto 0);
    variable localSum: std_logic_vector(sum'high downto 0);

  begin

    carry(0) := cin;

    for i in sum'reverse_range loop
      localSum(i)  := (a(i) xor b(i)) xor carry(i);
      carry(i + 1) := (a(i) and b(i)) or (carry(i) and (a(i) or b(i)));
    end loop;

    sum <= localSum;
    cout <= carry(carry'high - 1);

  end process;

end loopDemo;
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity adder is port (
  a,b: in unsigned(3 downto 0);
  sum: out unsigned(3 downto 0)
  );
end adder;

architecture simple of adder is

begin

  sum <= a + b;

end simple;
library IEEE;
use IEEE.std_logic_1164.all;


library IEEE;
use IEEE.std_logic_1164.all;

entity AND2 is port (
    i1: in std_logic;
    i2: in std_logic;
    y: out std_logic
    );
end AND2;

architecture rtl of AND2 is

begin

  y <= '1' when i1 = '1' and i2 = '1' else '0';

end rtl;
library IEEE;
use IEEE.std_logic_1164.all;

entity asyncLoad is port (
  loadVal, d: in std_logic_vector(3 downto 0);
  clk, load: in std_logic;
  q: out std_logic_vector(3 downto 0)
  );
end asyncLoad;

architecture rtl of asyncLoad is

begin

  process (clk, load, loadVal) begin
    if (load = '1') then
      q <= loadVal;
    elsif (clk'event and clk = '1' ) then
      q <= d;
    end if;
  end process;

end rtl;
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;

entity BidirBuf is port (
  OE: in std_logic;
  input: in std_logic_vector;
  output: out std_logic_vector
  );
end BidirBuf;

architecture behavioral of BidirBuf is

begin

  bidirBuf: process (OE, input) begin
    if (OE = '1') then
      output <= input;
    else
      output <= (others => 'Z');
    end if;
  end process;

end behavioral;
library IEEE;
use IEEE.std_logic_1164.all;

entity BidirCnt is port (
  OE: in std_logic;
  CntEnable: in std_logic;
  LdCnt: in std_logic;
  Clk: in std_logic;
  Rst: in std_logic;
  Cnt: inout std_logic_vector(3 downto 0)
  );
end BidirCnt;

architecture behavioral of BidirCnt is

  component LoadCnt port (
    CntEn: in std_logic;
    LdCnt: in std_logic;
    LdData: in std_logic_vector(3 downto 0);
    Clk: in std_logic;
    Rst: in std_logic;
    CntVal: out std_logic_vector(3 downto 0)
    );
  end component;

  component BidirBuf port (
    OE: in std_logic;
    input: in std_logic_vector;
    output: inout std_logic_vector
    );
  end component;

signal CntVal: std_logic_vector(3 downto 0);
signal LoadVal: std_logic_vector(3 downto 0);

begin

  u1: loadcnt port map (CntEn => CntEnable,
                        LdCnt => LdCnt,
                        LdData => LoadVal,
                        Clk => Clk,
                        Rst => Rst,
                        CntVal => CntVal
                        );

  u2: bidirbuf port map (OE => oe,
                         input => CntVal,
                         output => Cnt
                        );

  LoadVal <= Cnt;

end behavioral;
library IEEE;
use IEEE.std_logic_1164.all;

entity BIDIR is port (
  ip: in std_logic;
  oe: in std_logic;
  op_fb: out std_logic;
  op: inout std_logic
  );
end BIDIR;

architecture rtl of BIDIR is

begin

  op <= ip when oe = '1' else 'Z';
  op_fb <= op;

end rtl;

library IEEE;
use IEEE.std_logic_1164.all;

use work.primitive.all;

entity bidirbuffer is port (
  input: in std_logic;
  enable: in std_logic;
  feedback: out std_logic;
  output: inout std_logic
  );
end bidirbuffer;

architecture structural of bidirbuffer is

begin

  u1: bidir port map (ip => input,
                      oe => enable,
                      op_fb => feedback,
                      op => output
                      );

end structural;
library IEEE;
use IEEE.std_logic_1164.all;

entity clkGen is port (
  clk: in std_logic;
  reset: in std_logic;
  ClkDiv2, ClkDiv4,
  ClkDiv6,ClkDiv8: out std_logic
  );
end clkGen;
 
architecture behav of clkGen is

subtype numClks is std_logic_vector(1 to 4);
subtype numPatterns is integer range 0 to 11;

type clkTableType is array (numpatterns'low to numPatterns'high) of numClks;

constant clkTable: clkTableType := clkTableType'(
-- ClkDiv8______
-- ClkDiv6_____ |
-- ClkDiv4____ ||
-- ClkDiv2 __ |||
--           ||||
            "1111",
            "0111",
            "1011",
            "0001",
            "1100",
            "0100",
            "1010",
            "0010",
            "1111",
            "0001",
            "1001",
            "0101"); 

signal index: numPatterns;
 
begin

  lookupTable: process (clk, reset) begin
    if reset = '1' then
      index <= 0;
    elsif (clk'event and clk = '1') then
      if index = numPatterns'high then
        index <= numPatterns'low;
      else
        index <= index + 1;
      end if;
    end if;
  end process;

  (ClkDiv2,ClkDiv4,ClkDiv6,ClkDiv8) <= clkTable(index);

end behav;
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity counter is port (
  clk: in std_logic;
  enable: in std_logic;
  reset: in std_logic;
  count: buffer unsigned(3 downto 0)
  );
end counter;

architecture simple of counter is

begin

  increment: process (clk, reset) begin
    if reset = '1' then
      count <= "0000";
    elsif(clk'event and clk = '1') then
      if enable = '1' then
        count <= count + 1;
      else
        count <= count;
      end if;
    end if;
  end process;

end simple;
library IEEE;
use IEEE.std_logic_1164.all;

use work.scaleable.all;

entity count8 is port (
  clk: in std_logic;
  rst: in std_logic;
  count: out std_logic_vector(7 downto 0)
  );
end count8;

architecture structural of count8 is

begin

  u1: scaleUpCnt port map (clk => clk,
                           reset => rst,
                           cnt => count
                          );

end structural;
-- Incorporates Errata 5.4

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity counter is port (
  clk: in std_logic;
  reset: in std_logic;
  count: out std_logic_vector(0 to 9)
  );
end counter;

architecture simple of counter is

signal countL: unsigned(0 to 9);

begin

  increment: process (clk, reset) begin
    if reset = '1' then
      countL <= to_unsigned(3,10);
    elsif(clk'event and clk = '1') then
      countL <= countL + 1;
    end if;
  end process;
  
  count <= std_logic_vector(countL);

end simple;
-- Incorporates Errata 5.4

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity counter is port (
  clk: in std_logic;
  reset: in std_logic;
  count: out std_logic_vector(9 downto 0)
  );
end counter;

architecture simple of counter is

signal countL: unsigned(9 downto 0);

begin

  increment: process (clk, reset) begin
    if reset = '1' then
      countL <= to_unsigned(0,10);
    elsif(clk'event and clk = '1') then
      countL <= countL + 1;
    end if;
  end process;
  
  count <= std_logic_vector(countL);

end simple;
-- Incorporates Errata 5.4

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity counter is port (
  clk: in std_logic;
  reset: in std_logic;
  load: in std_logic;
  enable: in std_logic;
  data: in std_logic_vector(3 downto 0);
  count: out std_logic_vector(3 downto 0)
  );
end counter;

architecture simple of counter is

signal countL: unsigned(3 downto 0);

begin

  increment: process (clk, reset) begin
    if (reset = '1') then
      countL <= "0000";
    elsif(clk'event and clk = '1') then
      if (load = '1') then
        countL <= to_unsigned(data);
      elsif (enable = '1') then
        countL <= countL + 1;
      end if;
    end if;
  end process;
  
  count <= std_logic_vector(countL);

end simple;
-- Incorporates Errata 5.4

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity counter is port (
  clk: in std_logic;
  reset: in std_logic;
  load: in std_logic;
  data: in std_logic_vector(3 downto 0);
  count: out std_logic_vector(3 downto 0)
  );
end counter;

architecture simple of counter is

signal countL: unsigned(3 downto 0);

begin

  increment: process (clk, reset) begin
    if (reset = '1') then
      countL <= "0000";
    elsif(clk'event and clk = '1') then
      if (load = '1') then
        countL <= to_unsigned(data);
      else
        countL <= countL + 1;
      end if;
    end if;
  end process;
  
  count <= std_logic_vector(countL);

end simple;
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity Cnt4Term is port (
  clk: in std_logic;
  Cnt: out std_logic_vector(3 downto 0);
  TermCnt: out std_logic
  );
end Cnt4Term;

architecture behavioral of Cnt4Term is

signal CntL: unsigned(3 downto 0);

begin

  increment: process begin
    wait until clk = '1';
      CntL <= CntL + 1;
  end process;

  Cnt <= to_stdlogicvector(CntL);

  TermCnt <= '1' when CntL = "1111" else '0';

end behavioral;

library IEEE;
use IEEE.std_logic_1164.all;

entity Counter is port (
  clock: in std_logic;
  Count: out std_logic_vector(3 downto 0)
  );
end Counter;

architecture structural of Counter is

  component Cnt4Term port (
    clk: in std_logic;
    Cnt: out std_logic_vector(3 downto 0);
    TermCnt: out std_logic);
  end component;

begin

  u1: Cnt4Term port map (clk => clock,
                         Cnt => Count,
                         TermCnt => open
                    );

end structural;
-- Incorporates Errata 5.4

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity counter is port (
  clk: in std_logic;
  reset: in std_logic;
  count: out std_logic_vector(3 downto 0)
  );
end counter;

architecture simple of counter is

signal countL: unsigned(3 downto 0);

begin

  increment: process (clk) begin
    if(clk'event and clk = '1') then
      if (reset = '1') then
        countL <= "0000";
      else
        countL <= countL + 1;
      end if;
    end if;
  end process;
  
  count <= std_logic_vector(countL);

end simple;
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity convertArith is port (
  truncate: out unsigned(3 downto 0);
  extend: out unsigned(15 downto 0);
  direction: out unsigned(0 to 7)
  );
end convertArith;

architecture simple of convertArith is

constant Const: unsigned(7 downto 0) := "00111010";

begin

  truncate  <= resize(Const, truncate'length);
  extend    <= resize(Const, extend'length);
  direction <= resize(Const, direction'length);

end simple;
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity FEWGATES is port (
  a,b,c,d: in std_logic;
  y: out std_logic
  );
end FEWGATES;

architecture concurrent of FEWGATES is

constant THREE: std_logic_vector(1 downto 0) := "11";

begin

  y <= '1' when (a & b = THREE) or (c & d /= THREE) else '0';

end concurrent;
-- incorporates Errata 12.1

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity typeConvert is port (
  a: out unsigned(7 downto 0)
  );
end typeConvert;

architecture simple of typeConvert is

constant Const: natural := 43;

begin

  a <= To_unsigned(Const,8);

end simple;
-- Incorporates Errata 5.4 

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity counter is port (
  clk: in std_logic;
  count: out std_logic_vector(3 downto 0)
  );
end counter;

architecture simple of counter is

signal countL: unsigned(3 downto 0);

begin

  increment: process (clk) begin
    if (clk'event and clk = '1') then
      countL <= countL + 1;
    end if;
  end process;

  count <= std_logic_vector(countL);

end simple;
-- Incorporates Errata 5.4

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity counter is port (
  clk: in std_logic;
  reset: in std_logic;
  count: out std_logic_vector(0 to 3)
  );
end counter;

architecture simple of counter is

signal countL: unsigned(0 to 3);

begin

  increment: process (clk, reset) begin
    if reset = '1' then
      countL <= "1001";
    elsif(clk'event and clk = '1') then
      countL <= countL + 1;
    end if;
  end process;
  
  count <= std_logic_vector(countL);

end simple;
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity counter is port (
  clk: in std_logic;
  reset: in std_logic;
  count: out std_logic_vector(3 downto 0)
  );
end counter;

architecture simple of counter is

signal countL: unsigned(3 downto 0);

begin

  increment: process (clk, reset) begin
    if (reset = '1') then
      countL <= "0000";
    elsif(clk'event and clk = '1') then
      countL <= countL + "001";
    end if;
  end process;
  
  count <= std_logic_vector(countL);

end simple;
-- Incorporates Errata 5.4

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity counter is port (
  clk: in std_logic;
  reset: in std_logic;
  count: out std_logic_vector(3 downto 0)
  );
end counter;

architecture simple of counter is

signal countL: unsigned(3 downto 0);

begin

  increment: process (clk, reset) begin
    if reset = '1' then
      countL <= "1001";
    elsif(clk'event and clk = '1') then
      countL <= countL + 1;
    end if;
  end process;
  
  count <= std_logic_vector(countL);

end simple;
-- Incorporates Errata 5.4

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity counter is port (
  clk: in std_logic;
  reset: in std_logic;
  count: out std_logic_vector(3 downto 0)
  );
end counter;

architecture simple of counter is

signal countL: unsigned(3 downto 0);

begin

  increment: process (clk, reset) begin
    if (reset = '1') then
      countL <= "1001";
    elsif(clk'event and clk = '1') then
      countL <= countL + "0001";
    end if;
  end process;
  
  count <= std_logic_vector(countL);

end simple;
library IEEE;
use IEEE.std_logic_1164.all;

use work.decProcs.all;

entity decoder is port (
  decIn: in std_logic_vector(1 downto 0);
  decOut: out std_logic_vector(3 downto 0)
  );
end decoder;

architecture simple of decoder is

begin

  DEC2x4(decIn,decOut);

end simple;

library ieee;
use ieee.std_logic_1164.all;

entity isa_dec is port
(
  dev_adr:          in std_logic_vector(19 downto 0);

  decOut_n:         out std_logic_vector(5 downto 0)

);
end isa_dec;


architecture synthesis of isa_dec is

  constant  CtrlRegRange: std_logic_vector(2 downto 0)    := "100";
  constant  SuperIoRange: std_logic_vector(2 downto 0)    := "010";

  constant  IntCtrlReg:   std_logic_vector(16 downto 0) := "00000000000000000";
  constant  IoIntStatReg: std_logic_vector(16 downto 0) := "00000000000000001";
  constant  RstCtrlReg:   std_logic_vector(16 downto 0) := "00000000000000010";
  constant  AtcStatusReg: std_logic_vector(16 downto 0) := "00000000000000011";
  constant  MgmtStatusReg:std_logic_vector(16 downto 0) := "00000000000000100";

  alias sio_dec_n: std_logic is        decOut_n(5);
  alias rst_ctrl_rd_n: std_logic is    decOut_n(4);
  alias atc_stat_rd_n: std_logic is    decOut_n(3);
  alias mgmt_stat_rd_n: std_logic is   decOut_n(2);
  alias io_int_stat_rd_n: std_logic is decOut_n(1);
  alias int_ctrl_rd_n: std_logic is    decOut_n(0);

  alias upper: std_logic_vector(2 downto 0) is dev_adr(19 downto 17);
  alias CtrlBits: std_logic_vector(16 downto 0) is dev_adr(16 downto 0);

begin

  decoder: process (upper, CtrlBits)
    begin 
      -- Set defaults for outputs - for synthesis reasons.

        sio_dec_n        <= '1';
        int_ctrl_rd_n    <= '1';
        io_int_stat_rd_n <= '1';
        rst_ctrl_rd_n    <= '1';
        atc_stat_rd_n    <= '1';
        mgmt_stat_rd_n   <= '1';

        case upper is 

        when SuperIoRange =>
          sio_dec_n <= '0';
		
        when CtrlRegRange =>

          case CtrlBits is
			
            when IntCtrlReg =>
              int_ctrl_rd_n <= '0';

            when IoIntStatReg =>
              io_int_stat_rd_n <= '0';

            when RstCtrlReg =>
              rst_ctrl_rd_n <= '0';

            when AtcStatusReg =>
              atc_stat_rd_n <= '0';

            when MgmtStatusReg =>
              mgmt_stat_rd_n <= '0';

            when others =>
              null;
							
            end case;

          when others =>
            null;                     
					
        end case;

  end process decoder;

end synthesis;
library ieee;
use ieee.std_logic_1164.all;

entity isa_dec is port
(
  dev_adr:          in std_logic_vector(19 downto 0);

  sio_dec_n:        out std_logic;
  rst_ctrl_rd_n:    out std_logic;
  atc_stat_rd_n:    out std_logic;
  mgmt_stat_rd_n:   out std_logic;
  io_int_stat_rd_n: out std_logic;
  int_ctrl_rd_n:    out std_logic
);
end isa_dec;


architecture synthesis of isa_dec is

  constant  CtrlRegRange: std_logic_vector(2 downto 0)    := "100";
  constant  SuperIoRange: std_logic_vector(2 downto 0)    := "010";

  constant  IntCtrlReg:   std_logic_vector(16 downto 0) := "00000000000000000";
  constant  IoIntStatReg: std_logic_vector(16 downto 0) := "00000000000000001";
  constant  RstCtrlReg:   std_logic_vector(16 downto 0) := "00000000000000010";
  constant  AtcStatusReg: std_logic_vector(16 downto 0) := "00000000000000011";
  constant  MgmtStatusReg:std_logic_vector(16 downto 0) := "00000000000000100";

begin

  decoder: process (dev_adr)
    begin 
      -- Set defaults for outputs

        sio_dec_n        <= '1';
        int_ctrl_rd_n    <= '1';
        io_int_stat_rd_n <= '1';
        rst_ctrl_rd_n    <= '1';
        atc_stat_rd_n    <= '1';
        mgmt_stat_rd_n   <= '1';

        case dev_adr(19 downto 17) is 

        when SuperIoRange =>
          sio_dec_n <= '0';
		
        when CtrlRegRange =>

          case dev_adr(16 downto 0) is
			
            when IntCtrlReg =>
              int_ctrl_rd_n <= '0';

            when IoIntStatReg =>
              io_int_stat_rd_n <= '0';

            when RstCtrlReg =>
              rst_ctrl_rd_n <= '0';

            when AtcStatusReg =>
              atc_stat_rd_n <= '0';

            when MgmtStatusReg =>
              mgmt_stat_rd_n <= '0';

            when others =>
              null;
							
            end case;

          when others =>
            null;                     
					
        end case;

  end process decoder;

end synthesis;
library ieee;
use ieee.std_logic_1164.all;

entity isa_dec is port
(
  dev_adr:         in std_logic_vector(19 downto 0);

  sio_dec_n:       out std_logic;
  rst_ctrl_rd_n:   out std_logic;
  atc_stat_rd_n:   out std_logic;
  mgmt_stat_rd_n:  out std_logic;
  io_int_stat_rd_n:out std_logic;
  int_ctrl_rd_n:   out std_logic
  );
end isa_dec;


architecture synthesis of isa_dec is

  constant  CtrlRegRange: std_logic_vector(2 downto 0)  := "100";
  constant  SuperIoRange: std_logic_vector(2 downto 0)  := "010";

  constant  IntCtrlReg:   std_logic_vector(16 downto 0) := "00000000000000000";
  constant  IoIntStatReg: std_logic_vector(16 downto 0) := "00000000000000001";
  constant  RstCtrlReg:   std_logic_vector(16 downto 0) := "00000000000000010";
  constant  AtcStatusReg: std_logic_vector(16 downto 0) := "00000000000000011";
  constant  MgmtStatusReg:std_logic_vector(16 downto 0) := "00000000000000100";

begin
  sio_dec_n <= '0' when dev_adr (19 downto 17) = SuperIORange else '1';

  int_ctrl_rd_n <= '0' when (dev_adr (19 downto 17) = CtrlRegRange)
                        and (dev_adr(16 downto 0) = IntCtrlReg) else '1';

  io_int_stat_rd_n <= '0' when (dev_adr (19 downto 17) = CtrlRegRange)
                           and (dev_adr(16 downto 0) = IoIntStatReg) else '1';

  rst_ctrl_rd_n <= '0' when (dev_adr (19 downto 17) = CtrlRegRange)
                        and (dev_adr(16 downto 0) = RstCtrlReg) else '1';

  atc_stat_rd_n <= '0' when (dev_adr (19 downto 17) = CtrlRegRange)
                        and (dev_adr(16 downto 0) = AtcStatusReg) else '1';

  mgmt_stat_rd_n <= '0' when (dev_adr (19 downto 17) = CtrlRegRange)
                         and (dev_adr(16 downto 0) = MgmtStatusReg) else '1';


end synthesis;
library ieee;
use ieee.std_logic_1164.all;

entity isa_dec is port
(
  dev_adr:          in std_logic_vector(19 downto 0);
  cs0_n:            in std_logic;

  sio_dec_n:        out std_logic;
  rst_ctrl_rd_n:    out std_logic;
  atc_stat_rd_n:    out std_logic;
  mgmt_stat_rd_n:   out std_logic;
  io_int_stat_rd_n: out std_logic;
  int_ctrl_rd_n:    out std_logic
);
end isa_dec;


architecture synthesis of isa_dec is

  constant  CtrlRegRange: std_logic_vector(2 downto 0)    := "100";
  constant  SuperIoRange: std_logic_vector(2 downto 0)    := "010";

  constant  IntCtrlReg:   std_logic_vector(16 downto 0) := "00000000000000000";
  constant  IoIntStatReg: std_logic_vector(16 downto 0) := "00000000000000001";
  constant  RstCtrlReg:   std_logic_vector(16 downto 0) := "00000000000000010";
  constant  AtcStatusReg: std_logic_vector(16 downto 0) := "00000000000000011";
  constant  MgmtStatusReg:std_logic_vector(16 downto 0) := "00000000000000100";

begin

  decoder: process (dev_adr, cs0_n)
    begin 
      -- Set defaults for outputs - for synthesis reasons.

        sio_dec_n        <= '1';
        int_ctrl_rd_n    <= '1';
        io_int_stat_rd_n <= '1';
        rst_ctrl_rd_n    <= '1';
        atc_stat_rd_n    <= '1';
        mgmt_stat_rd_n   <= '1';

        if (cs0_n = '0') then
          case dev_adr(19 downto 17) is 

          when SuperIoRange =>
            sio_dec_n <= '0';
		
          when CtrlRegRange =>

            case dev_adr(16 downto 0) is
			
              when IntCtrlReg =>
                int_ctrl_rd_n <= '0';

              when IoIntStatReg =>
                io_int_stat_rd_n <= '0';

              when RstCtrlReg =>
                rst_ctrl_rd_n <= '0';

              when AtcStatusReg =>
                atc_stat_rd_n <= '0';

              when MgmtStatusReg =>
                mgmt_stat_rd_n <= '0';

              when others =>
                null;
							
              end case;

            when others =>
              null;                     
                         
          end case;
        else
          null;
        end if;

  end process decoder;

end synthesis;
library ieee;
use ieee.std_logic_1164.all;

entity isa_dec is port
(
  dev_adr:          in std_logic_vector(19 downto 0);
  cs0_n:            in std_logic;

  sio_dec_n:        out std_logic;
  rst_ctrl_rd_n:    out std_logic;
  atc_stat_rd_n:    out std_logic;
  mgmt_stat_rd_n:   out std_logic;
  io_int_stat_rd_n: out std_logic;
  int_ctrl_rd_n:    out std_logic
);
end isa_dec;


architecture synthesis of isa_dec is

  constant  CtrlRegRange: std_logic_vector(2 downto 0)    := "100";
  constant  SuperIoRange: std_logic_vector(2 downto 0)    := "010";

  constant  IntCtrlReg:   std_logic_vector(16 downto 0) := "00000000000000000";
  constant  IoIntStatReg: std_logic_vector(16 downto 0) := "00000000000000001";
  constant  RstCtrlReg:   std_logic_vector(16 downto 0) := "00000000000000010";
  constant  AtcStatusReg: std_logic_vector(16 downto 0) := "00000000000000011";
  constant  MgmtStatusReg:std_logic_vector(16 downto 0) := "00000000000000100";

  signal Lsio_dec_n:        std_logic;
  signal Lrst_ctrl_rd_n:    std_logic;
  signal Latc_stat_rd_n:    std_logic;
  signal Lmgmt_stat_rd_n:   std_logic;
  signal Lio_int_stat_rd_n: std_logic;
  signal Lint_ctrl_rd_n:    std_logic;

begin

  decoder: process (dev_adr)
    begin 
      -- Set defaults for outputs - for synthesis reasons.

        Lsio_dec_n        <= '1';
        Lint_ctrl_rd_n    <= '1';
        Lio_int_stat_rd_n <= '1';
        Lrst_ctrl_rd_n    <= '1';
        Latc_stat_rd_n    <= '1';
        Lmgmt_stat_rd_n   <= '1';

        case dev_adr(19 downto 17) is 

        when SuperIoRange =>
          Lsio_dec_n <= '0';
		
        when CtrlRegRange =>

          case dev_adr(16 downto 0) is
			
            when IntCtrlReg =>
              Lint_ctrl_rd_n <= '0';

            when IoIntStatReg =>
              Lio_int_stat_rd_n <= '0';

            when RstCtrlReg =>
              Lrst_ctrl_rd_n <= '0';

            when AtcStatusReg =>
              Latc_stat_rd_n <= '0';

            when MgmtStatusReg =>
              Lmgmt_stat_rd_n <= '0';

            when others =>
              null;
							
            end case;

          when others =>
            null;                     
                         
        end case;

  end process decoder;

  qualify: process (cs0_n) begin

    sio_dec_n        <= '1';
    int_ctrl_rd_n    <= '1';
    io_int_stat_rd_n <= '1';
    rst_ctrl_rd_n    <= '1';
    atc_stat_rd_n    <= '1';
    mgmt_stat_rd_n   <= '1';

    if (cs0_n = '0') then
      sio_dec_n        <= Lsio_dec_n;
      int_ctrl_rd_n    <= Lint_ctrl_rd_n;
      io_int_stat_rd_n <= Lio_int_stat_rd_n;
      rst_ctrl_rd_n    <= Lrst_ctrl_rd_n;
      atc_stat_rd_n    <= Latc_stat_rd_n;
      mgmt_stat_rd_n   <= Lmgmt_stat_rd_n;
    else
      null;
    end if;
  end process qualify;

end synthesis;
library ieee;
use ieee.std_logic_1164.all;

entity isa_dec is port
(
  dev_adr:          in std_logic_vector(19 downto 0);

  sio_dec_n:        out std_logic;
  rst_ctrl_rd_n:    out std_logic;
  atc_stat_rd_n:    out std_logic;
  mgmt_stat_rd_n:   out std_logic;
  io_int_stat_rd_n: out std_logic;
  int_ctrl_rd_n:    out std_logic
);
end isa_dec;


architecture synthesis of isa_dec is

  constant  CtrlRegRange: std_logic_vector(2 downto 0)    := "100";
  constant  SuperIoRange: std_logic_vector(2 downto 0)    := "010";

  constant  IntCtrlReg:   std_logic_vector(16 downto 0) := "00000000000000000";
  constant  IoIntStatReg: std_logic_vector(16 downto 0) := "00000000000000001";
  constant  RstCtrlReg:   std_logic_vector(16 downto 0) := "00000000000000010";
  constant  AtcStatusReg: std_logic_vector(16 downto 0) := "00000000000000011";
  constant  MgmtStatusReg:std_logic_vector(16 downto 0) := "00000000000000100";

begin

  decoder: process ( dev_adr)
    begin 
      -- Set defaults for outputs - for synthesis reasons.

        sio_dec_n        <= '1';
        int_ctrl_rd_n    <= '1';
        io_int_stat_rd_n <= '1';
        rst_ctrl_rd_n    <= '1';
        atc_stat_rd_n    <= '1';
        mgmt_stat_rd_n   <= '1';

        if dev_adr(19 downto 17) = SuperIOrange then

          sio_dec_n <= '0';

        elsif dev_adr(19 downto 17) = CtrlRegrange then

          if dev_adr(16 downto 0) = IntCtrlReg then

            int_ctrl_rd_n <= '0';

          elsif dev_adr(16 downto 0)= IoIntStatReg then

            io_int_stat_rd_n <= '0';

          elsif dev_adr(16 downto 0) = RstCtrlReg then

            rst_ctrl_rd_n <= '0';

          elsif dev_adr(16 downto 0) = AtcStatusReg then

            atc_stat_rd_n <= '0';

          elsif dev_adr(16 downto 0) = MgmtStatusReg then

            mgmt_stat_rd_n <= '0';

          else

            null;
							
          end if;

        else

          null;

        end if;

  end process decoder;

end synthesis;
library IEEE;
use IEEE.std_logic_1164.all;

package decProcs is

  procedure DEC2x4 (inputs : in std_logic_vector(1 downto 0);
                    decode: out std_logic_vector(3 downto 0)
                   );
end decProcs;

package body decProcs is

  procedure DEC2x4 (inputs : in std_logic_vector(1 downto 0);
                    decode: out std_logic_vector(3 downto 0)
                   ) is
  begin
    case inputs is
      when "11" =>
        decode := "1000";
      when "10" =>
        decode := "0100";
      when "01" =>
        decode := "0010";
      when "00" =>
        decode := "0001";
      when others =>
        decode := "0001";
    end case;
  end DEC2x4;

end decProcs;
library ieee;
use ieee.std_logic_1164.all;

entity isa_dec is port
(
  dev_adr:         in std_logic_vector(19 downto 0);

  sio_dec_n:       out std_logic;
  rst_ctrl_rd_n:   out std_logic;
  atc_stat_rd_n:   out std_logic;
  mgmt_stat_rd_n:  out std_logic;
  io_int_stat_rd_n:out std_logic;
  int_ctrl_rd_n:   out std_logic
  );
end isa_dec;


architecture synthesis of isa_dec is

  constant  CtrlRegRange: std_logic_vector(2 downto 0)  := "100";
  constant  SuperIoRange: std_logic_vector(2 downto 0)  := "010";

  constant  IntCtrlReg:   std_logic_vector(16 downto 0) := "00000000000000000";
  constant  IoIntStatReg: std_logic_vector(16 downto 0) := "00000000000000001";
  constant  RstCtrlReg:   std_logic_vector(16 downto 0) := "00000000000000010";
  constant  AtcStatusReg: std_logic_vector(16 downto 0) := "00000000000000011";
  constant  MgmtStatusReg:std_logic_vector(16 downto 0) := "00000000000000100";

begin
  with dev_adr(19 downto 17) select
    sio_dec_n <= '0' when SuperIORange,
                 '1' when others;

  with dev_adr(19 downto 0) select
    int_ctrl_rd_n <= '0' when CtrlRegRange & IntCtrlReg,
                     '1' when others;

  with dev_adr(19 downto 0) select
    io_int_stat_rd_n <= '0' when CtrlRegRange & IoIntStatReg,
                        '1' when others;

  with dev_adr(19 downto 0) select
    rst_ctrl_rd_n <= '0' when CtrlRegRange & RstCtrlReg,
                     '1' when others;

  with dev_adr(19 downto 0) select
    atc_stat_rd_n <= '0' when CtrlRegRange & AtcStatusReg,
                     '1' when others;

  with dev_adr(19 downto 0) select
    mgmt_stat_rd_n <= '0' when CtrlRegRange & MgmtStatusReg,
                      '1' when others;


end synthesis;
-- Incorporates Errata 5.1 and 5.4

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity progPulse is port (
  clk, reset: in std_logic;
  loadLength,loadDelay: in std_logic;
  data: in std_logic_vector(7 downto 0);
  pulse: out std_logic
  );
end progPulse;

architecture rtl of progPulse is

signal delayCnt, pulseCnt: unsigned(7 downto 0);
signal delayCntVal, pulseCntVal: unsigned(7 downto 0);
signal startPulse, endPulse: std_logic;

begin

  delayReg: process (clk, reset) begin
    if reset = '1' then
      delayCntVal <= "11111111";
    elsif clk'event and clk = '1' then
      if loadDelay = '1' then
        delayCntVal <= unsigned(data);
      end if;
    end if;
  end process;

  lengthReg: process (clk, reset) begin
    if reset = '1' then
      pulseCntVal <= "11111111";
    elsif clk'event and clk = '1' then
      if loadLength = '1' then -- changed loadLength to loadDelay (Errata 5.1)
        pulseCntVal <= unsigned(data);
      end if;
    end if;
  end process;

  pulseDelay: process (clk, reset) begin
    if (reset = '1') then
      delayCnt <= "11111111";
    elsif(clk'event and clk = '1') then
      if (loadDelay = '1' or loadLength = '1' or endPulse = '1') then -- changed startPulse to endPulse (Errata 5.1)
        delayCnt <= delayCntVal;
      elsif endPulse = '1' then
        delayCnt <= delayCnt - 1;
      end if;
    end if;
  end process;

  startPulse <= '1' when delayCnt = "00000000" else '0';

  pulseLength: process (clk, reset) begin
    if (reset = '1') then
      pulseCnt <= "11111111";
    elsif (clk'event and clk = '1') then
      if (loadLength = '1') then
        pulseCnt <= pulseCntVal;
      elsif (startPulse = '1' and endPulse = '1') then
        pulseCnt <= pulseCntVal;
      elsif (endPulse = '1') then
        pulseCnt <= pulseCnt;
      else
        pulseCnt <= pulseCnt - 1;
      end if;
    end if;
  end process;

  endPulse <= '1' when pulseCnt = "00000000" else '0';

  pulseOutput: process (clk, reset) begin
    if (reset = '1') then
      pulse <= '0';
    elsif (clk'event and clk = '1') then
      if (startPulse = '1') then
        pulse <= '1';
      elsif (endPulse = '1') then
        pulse <= '0';
      end if;
    end if;
  end process;
        

end rtl;
library IEEE;
use IEEE.std_logic_1164.all;

entity DFF is port (
    d: in std_logic;
    clk: in std_logic;
    arst : in std_logic;
    q: out std_logic;
    );
end DFF;

architecture rtl of DFF is

begin

  process (clk) begin
    if arst = '1' then
      q <= '0';
    elsif clk'event and clk = '1' then
        q <= d;
    end if;
  end process;

end rtl;
library IEEE;
use IEEE.std_logic_1164.all;

entity DFF is port (
    d: in std_logic;
    clk: in std_logic;
    a,b,c : in std_logic;
    q: out std_logic
    );
end DFF;

architecture rtl of DFF is

begin

  process (clk, a,b,c) begin
    if ((a = '1' and b = '1') or c = '1') then
      q <= '0';
    elsif clk'event and clk = '1' then
        q <= d;
    end if;
  end process;

end rtl;
library IEEE;
use IEEE.std_logic_1164.all;

entity DFF is port (
    d: in std_logic;
    clk: in std_logic;
    a,b,c : in std_logic;
    q: out std_logic
    );
end DFF;

architecture rtl of DFF is

signal localRst: std_logic;

begin

  localRst <= '1' when (( a = '1' and b = '1') or c = '1') else '0';

  process (clk, localRst) begin
    if localRst = '1' then
      q <= '0';
    elsif clk'event and clk = '1' then
        q <= d;
    end if;
  end process;

end rtl;
library IEEE;
use IEEE.std_logic_1164.all;

entity DFF is port (
    d: in std_logic;
    clk: in std_logic;
    arst: in std_logic;
    q: out std_logic
    );
end DFF;

architecture rtl of DFF is

begin

  process (clk, arst) begin
    if arst = '1' then
      q <= '0';
    elsif clk'event and clk = '1' then
        q <= d;
    end if;
  end process;

end rtl;
library IEEE;
use IEEE.std_logic_1164.all;

entity DFF is port (
    d: in std_logic;
    clk: in std_logic;
    aset : in std_logic;
    q: out std_logic
    );
end DFF;

architecture rtl of DFF is

begin

  process (clk, aset) begin
    if aset = '1' then
      q <= '1';
    elsif clk'event and clk = '1' then
        q <= d;
    end if;
  end process;

end rtl;
library IEEE;
use IEEE.std_logic_1164.all;

entity DFF is port (
    d1, d2: in std_logic;
    clk: in std_logic;
    arst : in std_logic;
    q1, q2: out std_logic
    );
end DFF;

architecture rtl of DFF is

begin

  process (clk, arst) begin
    if arst = '1' then
      q1 <= '0';
      q2 <= '1';
    elsif clk'event and clk = '1' then
      q1 <= d1;
      q2 <= d2;
    end if;
  end process;

end rtl;
library IEEE;
use IEEE.std_logic_1164.all;

entity DFF is port (
    d: in std_logic;
    clk: in std_logic;
    en: in std_logic;
    q: out std_logic
    );
end DFF;

architecture rtl of DFF is

begin

  process begin
    if clk'event and clk = '1' then
      if en = '1' then
        q <= d;
      end if;
    end if;
    wait on clk;
  end process;

end rtl;
library IEEE;
use IEEE.std_logic_1164.all;

entity DFFE is port (
    d: in std_logic;
    en: in std_logic;
    clk: in std_logic;
    q: out std_logic
    );
end DFFE;

architecture rtl of DFFE is

begin

  process begin
    wait until clk = '1';
      if en = '1' then
        q <= d;
      end if;
  end process;

end rtl;
library IEEE;
use IEEE.std_logic_1164.all;

entity DFF is port (
    d: in std_logic;
    clk: in std_logic;
    envector: in std_logic_vector(7 downto 0);
    q: out std_logic
    );
end DFF;

architecture rtl of DFF is

begin

  process (clk) begin
    if clk'event and clk = '1' then
      if envector = "10010111" then
        q <= d;
      end if;
    end if;
  end process;

end rtl;
library IEEE;
use IEEE.std_logic_1164.all;

entity DFF is port (
    d: in std_logic;
    clk: in std_logic;
    en: in std_logic;
    q: out std_logic
    );
end DFF;

architecture rtl of DFF is

begin

  process (clk) begin
    if clk'event and clk = '1' then
      if en = '1' then
        q <= d;
      end if;
    end if;
  end process;

end rtl;

library IEEE;
use IEEE.std_logic_1164.all;

entity DFFE_SR is port (
    d: in std_logic;
    en: in std_logic;
    clk: in std_logic;
    rst: in std_logic;
    prst: in std_logic;
    q: out std_logic
    );
end DFFE_SR;

architecture rtl of DFFE_SR is

begin

  process (clk, rst, prst) begin
    if (prst = '1') then
      q <= '1';
    elsif (rst = '1') then
      q <= '0';
    elsif (clk'event and clk = '1') then
      if (en = '1') then
        q <= d;
      end if;
    end if;
  end process;

end rtl;


library IEEE;
use IEEE.std_logic_1164.all;

entity flipFlop is port (
  clock, input: in std_logic;
  ffOut: out std_logic
  );
end flipFlop;

architecture simple of flipFlop is

  procedure dff (signal clk: in std_logic;
                 signal d: in std_logic;
                 signal q: out std_logic
                ) is
  begin
    if clk'event and clk = '1' then
      q <= d;
    end if;
  end procedure dff;

begin

  dff(clock, input, ffOut);

end simple;

library IEEE;
use IEEE.std_logic_1164.all;

entity DFF is port (
    d: in std_logic;
    clk: in std_logic;
    end: in std_logic;
    q: out std_logic
    );
end DFF;

architecture rtl of DFF is

begin

  process begin
    wait until rising_edge(clk);
      if en = '1' then
        q <= d;
      end if;
  end process;

end rtl;
library IEEE;
use IEEE.std_logic_1164.all;

entity DFF is port (
    d1, d2: in std_logic;
    clk: in std_logic;
    srst : in std_logic;
    q1, q2: out std_logic
    );
end DFF;

architecture rtl of DFF is

begin

  process (clk) begin
    if clk'event and clk = '1' then
      if srst = '1' then
	q1 <= '0';
	q2 <= '1';
      else
        q1 <= d1;
        q2 <= d2;
      end if;
    end if;
  end process;

end rtl;

library IEEE;
use IEEE.std_logic_1164.all;

entity DFFE_SR is port (
    d: in std_logic;
    en: in std_logic;
    clk: in std_logic;
    rst: in std_logic;
    prst: in std_logic;
    q: out std_logic
    );
end DFFE_SR;

architecture rtl of DFFE_SR is

begin

  process (clk, rst, prst) begin
    if (rst = '1') then
      q <= '0';
    elsif (prst = '1') then
      q <= '1';
    elsif (clk'event and clk = '1') then
      if (en = '1') then
        q <= d;
      end if;
    end if;
  end process;

end rtl;


library IEEE;
use IEEE.std_logic_1164.all;

entity DFF is port (
    d: in std_logic;
    clk: in std_logic;
    srst : in std_logic;
    q: out std_logic
    );
end DFF;

architecture rtl of DFF is

begin

  process begin
    wait until clk = '1';
      if srst = '1' then
	q <= '0';
      else
        q <= d;
      end if;
  end process;

end rtl;
library IEEE;
use IEEE.std_logic_1164.all;

entity struct_dffe_sr is port (
  d: in std_logic;
  clk: in std_logic;
  en: in std_logic;
  rst,prst: in std_logic;
  q: out std_logic
  );
end struct_dffe_sr;

use work.primitive.all;

architecture instance of struct_dffe_sr is

begin

  ff: dffe_sr port map (
	d => d,
	clk => clk,
	en => en,
        rst => rst,
        prst => prst,
	q => q
    );

end instance;
library IEEE;
use IEEE.std_logic_1164.all;

entity DFF is port (
    d: in std_logic;
    clk: in std_logic;
    srst : in std_logic;
    q: out std_logic
    );
end DFF;

architecture rtl of DFF is

begin

  process (clk) begin
    if clk'event and clk = '1' then
      if srst = '1' then
	q <= '0';
      else
        q <= d;
      end if;
    end if;
  end process;

end rtl;
library IEEE;
use IEEE.std_logic_1164.all;

entity struct_dffe is port (
  d: in std_logic;
  clk: in std_logic;
  en: in std_logic;
  q: out std_logic
  );
end struct_dffe;

use work.primitive.all;

architecture instance of struct_dffe is

begin

  ff: dffe port map (
	d => d,
	clk => clk,
	en => en,
	q => q
      );

end instance;
library IEEE;
use IEEE.std_logic_1164.all;

use work.primitive.all;

entity dffTri is
  generic (size: integer := 8);
  port (
  data: in std_logic_vector(size - 1 downto 0);
  clock: in std_logic;
  ff_enable: in std_logic;
  op_enable: in std_logic;
  qout: out std_logic_vector(size - 1 downto 0)
  );
end dffTri;

architecture parameterize of dffTri is

type tribufType is record
  ip: std_logic;
  oe: std_logic;
  op: std_logic;
end record;

type tribufArrayType is array (integer range <>) of tribufType;

signal tri: tribufArrayType(size - 1 downto 0);

begin

  g0: for i in 0 to size - 1 generate
    u1: DFFE port map (data(i), tri(i).ip, ff_enable, clock);
  end generate;

  g1: for i in 0 to size - 1 generate
    u2: TRIBUF port map (tri(i).ip, tri(i).oe, tri(i).op);
    tri(i).oe <= op_enable;
    qout(i) <= tri(i).op;
  end generate;

end parameterize;
library IEEE;
use IEEE.std_logic_1164.all;

entity DFF is port (
    d: in std_logic;
    clk: in std_logic;
    en: in std_logic;
    q: out std_logic
    );
end DFF;

architecture rtl of DFF is

begin

  process begin
    wait until clk = '1';
      if en = '1' then
        q <= d;
      end if;
  end process;

end rtl;
library IEEE;
use IEEE.std_logic_1164.all;

entity TRIBUF is port (
  ip: in std_logic;
  oe: in std_logic;
  op: out std_logic bus
  );
end TRIBUF;

architecture sequential of TRIBUF is

begin

  enable: process (ip,oe) begin
    if (oe = '1') then
      op <= ip;
    else
      op <= null;
    end if;
  end process;

end sequential;
library IEEE;
use IEEE.std_logic_1164.all;

entity DLATCHH is port (
    d: in std_logic;
    en: in std_logic;
    q: out std_logic
    );
end DLATCHH;

architecture rtl of DLATCHH is

signal qLocal: std_logic;

begin

  qLocal <= d when en = '1' else qLocal;

  q <= qLocal;

end rtl;
library IEEE;
use IEEE.std_logic_1164.all;

entity DLATCHH is port (
    d: in std_logic;
    en: in std_logic;
    q: out std_logic
    );
end DLATCHH;

architecture rtl of DLATCHH is

begin

  process (en, d) begin
    if en = '1' then
      q <= d;
    end if;
  end process;

end rtl;
library IEEE;
use IEEE.std_logic_1164.all;

entity struct_dlatch is port (
  d: in std_logic;
  en: in std_logic;
  q: out std_logic
  );
end struct_dlatch;

use work.primitive.all;

architecture instance of struct_dlatch is

begin

  latch: dlatchh port map (
	d => d,
	en => en,
	q => q
      );

end instance;
-- Incorporates Errata 5.4

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity downCounter is port (
  clk: in std_logic;
  reset: in std_logic;
  count: out std_logic_vector(3 downto 0)
  );
end downCounter;

architecture simple of downCounter is

signal countL: unsigned(3 downto 0);
signal termCnt: std_logic;

begin

  decrement: process (clk, reset) begin
    if (reset = '1') then
      countL <= "1011";          -- Reset to 11
      termCnt <= '1';
    elsif(clk'event and clk = '1') then
      if (termCnt = '1') then
        countL <= "1011";        -- Count rolls over to 11
      else
        countL <= countL - 1;
      end if;

      if (countL = "0001") then  -- Terminal count decoded 1 cycle earlier
        termCnt <= '1';
      else
        termCnt <= '0';
      end if;
    end if;
  end process;
  
  count <= std_logic_vector(countL);

end simple;
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity compareDC is port (
  addressBus: in std_logic_vector(31 downto 0);
  addressHit: out std_logic
  );
end compareDC;

architecture wontWork of compareDC is

begin

  compare: process(addressBus) begin
    if (addressBus = "011110101011--------------------") then
      addressHit <= '1';
    else
      addressHit <= '0';
    end if;
  end process compare;

end wontWork;
library ieee;
use ieee.std_logic_1164.all;
entity encoder is
        port (invec: in std_logic_vector(7 downto 0);
              enc_out: out std_logic_vector(2 downto 0)
             );
end encoder;

architecture rtl of encoder is

begin

  encode: process (invec) begin
    case invec is
      when "00000001" =>
        enc_out <= "000";

      when "00000010" =>
        enc_out <= "001";

      when "00000100" =>
        enc_out <= "010";

      when "00001000" =>
        enc_out <= "011";

      when "00010000" =>
        enc_out <= "100";

      when "00100000" =>
        enc_out <= "101";

      when "01000000" =>
        enc_out <= "110";

      when "10000000" =>
        enc_out <= "111";

      when others =>
        enc_out <= "000";

      end case;
    end process;

end rtl;
library ieee;
use ieee.std_logic_1164.all;

entity encoder is
  port (invec:in std_logic_vector(7 downto 0);
        enc_out:out  std_logic_vector(2 downto 0)
       );
end encoder;

architecture rtl of encoder is
begin
  process (invec)
    begin
      if invec(7) = '1' then
        enc_out <= "111";

      elsif invec(6) = '1' then
        enc_out <= "110";

      elsif invec(5) = '1' then
        enc_out <= "101";

      elsif invec(4) = '1' then
        enc_out <= "100";

      elsif invec(3) = '1' then
        enc_out <= "011";

      elsif invec(2) = '1' then
        enc_out <= "010";

      elsif invec(1) = '1' then
        enc_out <= "001";

      elsif invec(0) = '1' then
        enc_out <= "000";

      else 
        enc_out <= "000";
      end if; 
    end process;
end rtl;

library ieee;
use ieee.std_logic_1164.all;
entity encoder is
  port (invec: in std_logic_vector(7 downto 0);
        enc_out: out std_logic_vector(2 downto 0)
        );
end encoder;

architecture rtl of encoder is

begin
  enc_out <= "111" when invec(7) = '1' else
             "110" when invec(6) = '1' else
             "101" when invec(5) = '1' else
             "100" when invec(4) = '1' else
             "011" when invec(3) = '1' else
             "010" when invec(2) = '1' else
             "001" when invec(1) = '1' else
             "000" when invec(0) = '1' else
             "000";

end rtl;
-- includes Errata 5.2
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all; -- errata 5.2

entity compare is port (
  ina: in std_logic_vector (3 downto 0);
  inb: in std_logic_vector (2 downto 0);
  equal: out std_logic
  );
end compare;

architecture simple of compare is

begin

  equalProc: process (ina, inb) begin
    if (ina = inb ) then
      equal <= '1';
    else
      equal <= '0';
    end if;
  end process;

end simple;
library IEEE;
use IEEE.std_logic_1164.all;

entity LogicFcn is port (
  A: in std_logic;
  B: in std_logic;
  C: in std_logic;
  Y: out std_logic
  );
end LogicFcn;

architecture behavioral of LogicFcn is

begin

  fcn: process (A,B,C) begin

    if (A = '0' and B = '0') then
      Y <= '1';
    elsif C = '1' then
      Y <= '1';
    else
      Y <= '0';
    end if;

  end process;

end behavioral;
library IEEE;
use IEEE.std_logic_1164.all;

entity LogicFcn is port (
  A: in std_logic;
  B: in std_logic;
  C: in std_logic;
  Y: out std_logic
  );
end LogicFcn;

architecture dataflow of LogicFcn is

begin

  Y <= '1' when (A = '0' AND B = '0') OR
                (C = '1')
           else '0';

end dataflow;
library IEEE;
use IEEE.std_logic_1164.all;
use work.primitive.all;

entity LogicFcn is port (
  A: in std_logic;
  B: in std_logic;
  C: in std_logic;
  Y: out std_logic
  );
end LogicFcn;

architecture structural of LogicFcn is

signal notA, notB, andSignal: std_logic;

begin

  i1: inverter port map (i => A,
                         o => notA);

  i2: inverter port map (i => B,
                         o => notB);

  a1: and2 port map (i1 => notA,
                     i2 => notB,
                     y => andSignal);

  o1: or2 port map (i1 => andSignal,
                    i2 => C,
                    y => Y);

end structural;
library IEEE;
use IEEE.std_logic_1164.all;

entity SimDFF is port (
  D, Clk: in std_logic;
  Q: out std_logic
  );
end SimDff;

architecture SimModel of SimDFF is

constant tCQ: time := 8 ns;
constant tS:  time := 4 ns;
constant tH:  time := 3 ns;

begin

  reg: process (Clk, D) begin

    -- Assign output tCQ after rising clock edge
    if (Clk'event and Clk = '1') then
      Q <= D after tCQ;
    end if;

    -- Check setup time
    if (Clk'event and Clk = '1') then
      assert (D'last_event >= tS)
        report "Setup time violation"
        severity Warning;
    end if;

    -- Check hold time
    if (D'event and Clk'stable and Clk = '1') then
      assert (D'last_event - Clk'last_event > tH)
        report "Hold Time Violation"
        severity Warning;
    end if;

  end process;

end simModel;
library IEEE;
use IEEE.std_logic_1164.all;

entity DFF is port (
    d: in std_logic;
    clk: in std_logic;
    q: out std_logic
    );
end DFF;

architecture rtl of DFF is

begin

  process (clk) begin
    wait until clk = '1';
      q <= d;
  end process;

end rtl;
library IEEE;
use IEEE.std_logic_1164.all;

entity DFF is port (
    d: in std_logic;
    clk: in std_logic;
    q: out std_logic
    );
end DFF;

architecture rtl of DFF is

begin

  process begin
    wait until clk = '1';
      q <= d;

    wait on clk;
  end process;

end rtl;
configuration SimpleGatesCfg of FEWGATES is

  for structural

    for all: AND2
      use entity work.and2(rtl);
    end for;

    for u3: inverter
      use entity work.inverter(rtl);
    end for;

    for u4: or2
      use entity work.or2(rtl);
    end for;

  end for;

end SimpleGatesCfg;
configuration SimpleGatesCfg of FEWGATES is

  for structural

    for u1: and2
      use entity work.and2(rtl);
    end for;

    for u2: and2
      use entity work.and2(rtl);
    end for;

    for u3: inverter
      use entity work.inverter(rtl);
    end for;

    for u4: or2
      use entity work.or2(rtl);
    end for;

  end for;

end SimpleGatesCfg;
library IEEE;
use IEEE.std_logic_1164.all;

entity FEWGATES is port (
  a,b,c,d: in std_logic;
  y: out std_logic
  );
end FEWGATES;

use work.and2;
use work.or2;
use work.inverter;

architecture structural of FEWGATES is

  component AND2 port (
    i1: in std_logic;
    i2: in std_logic;
    y: out std_logic
    );
  end component;

  component OR2 port (
    i1: in std_logic;
    i2: in std_logic;
    y: out std_logic
    );
  end component;

  component INVERTER port (
    i: in std_logic;
    o: out std_logic
    );
  end component;

signal a_and_b, c_and_d, not_c_and_d: std_logic;

begin

  u1: and2 port map (i1 => a ,
                     i2 => b,
                     y => a_and_b
                     );

  u2: and2 port map (i1 => c,
                     i2 => d,
                     y => c_and_d
                     );

  u3: inverter port map (i => c_and_d,
                         o => not_c_and_d);

  u4: or2 port map (i1 => a_and_b,
                    i2 => not_c_and_d,
                    y => y
                    );
end structural;
library IEEE;
use IEEE.std_logic_1164.all;

entity FEWGATES is port (
  a,b,c,d: in std_logic;
  y: out std_logic
  );
end FEWGATES;

use work.and2;
use work.or2;
use work.inverter;

architecture structural of FEWGATES is

  component AND2 port (
    i1: in std_logic;
    i2: in std_logic;
    y: out std_logic
    );
  end component;

  component OR2 port (
    i1: in std_logic;
    i2: in std_logic;
    y: out std_logic
    );
  end component;

  component INVERTER port (
    i: in std_logic;
    o: out std_logic
    );
  end component;

signal a_and_b, c_and_d, not_c_and_d: std_logic;

-- Configution specifications

for all: and2 use entity work.and2(rtl);
for u3: inverter use entity work.inverter(rtl);
for u4: or2 use entity work.or2(rtl);

begin

  u1: and2 port map (i1 => a, i2 => b,
                     y => a_and_b
                     );

  u2: and2 port map (i1 => c, i2 => d,
                     y => c_and_d
                     );

  u3: inverter port map (i => c_and_d,
                         o => not_c_and_d);

  u4: or2 port map (i1 => a_and_b, i2 => not_c_and_d,
                    y => y
                    );
end structural;
library IEEE;
use IEEE.std_logic_1164.all;

entity FEWGATES is port (
  a,b,c,d: in std_logic;
  y: out std_logic
  );
end FEWGATES;

use work.GatesPkg.all;

architecture structural of FEWGATES is

signal a_and_b, c_and_d, not_c_and_d: std_logic;

begin

  u1: and2 port map (i1 => a ,
                     i2 => b,
                     y => a_and_b
                     );

  u2: and2 port map (i1 => c,
                     i2 => d,
                     y => c_and_d
                     );

  u3: inverter port map (i => c_and_d,
                         o => not_c_and_d);

  u4: or2 port map (i1 => a_and_b,
                    i2 => not_c_and_d,
                    y => y
                    );
end structural;
library IEEE;
use IEEE.std_logic_1164.all;


entity FEWGATES is port (
  a,b,c,d: in std_logic;
  y: out std_logic
  );
end FEWGATES;

architecture concurrent of FEWGATES is

signal a_and_b, c_and_d, not_c_and_d: std_logic;

begin

  a_and_b <= '1' when a = '1' and b = '1' else '0';
  c_and_d <= '1' when c = '1' and d = '1' else '0';

  not_c_and_d <= not c_and_d;

  y <= '1' when a_and_b = '1' or not_c_and_d = '1' else '0';

end concurrent;
library IEEE;
use IEEE.std_logic_1164.all;

package GatesPkg is

  component AND2 port (
    i1: in std_logic;
    i2: in std_logic;
    y: out std_logic
    );
  end component;

  component OR2 port (
    i1: in std_logic;
    i2: in std_logic;
    y: out std_logic
    );
  end component;

  component INVERTER port (
    i: in std_logic;
    o: out std_logic
    );
  end component;

end GatesPkg;
library IEEE;
use IEEE.std_logic_1164.all;

use work.primitive.all;

entity FEWGATES is port (
  a,b,c,d: in std_logic;
  y: out std_logic
  );
end FEWGATES;

architecture structural of FEWGATES is

signal a_and_b, c_and_d, not_c_and_d: std_logic;

begin

  u1: and2 port map (i1 => a ,
                     i2 => b,
                     y => a_and_b
                     );

  u2: and2 port map (i1 =>c,
                     i2 => d,
                     y => c_and_d
                     );

  u3: inverter port map (a => c_and_d,
                         y => not_c_and_d);

  u4: or2 port map (i1 => a_and_b,
                    i2 => not_c_and_d,
                    y => y
                    );

end structural;
library IEEE;
use IEEE.std_logic_1164.all;

entity AND2 is port (
    i1: in std_logic;
    i2: in std_logic;
    y: out std_logic
    );
end AND2;

architecture rtl of AND2 is

begin

  y <= '1' when i1 = '1' and i2 = '1' else '0';

end rtl;

library IEEE;
use IEEE.std_logic_1164.all;

entity OR2 is port (
    i1: in std_logic;
    i2: in std_logic;
    y: out std_logic
    );
end OR2;

architecture rtl of OR2 is

begin

  y <= '1' when i1 = '1' or i2 = '1' else '0';

end rtl;

library IEEE;
use IEEE.std_logic_1164.all;

entity INVERTER is port (
    i: in std_logic;
    o: out std_logic
    );
end INVERTER;

architecture rtl of INVERTER is

begin

  o <= not i;

end rtl;

library IEEE;
use IEEE.std_logic_1164.all;

entity FEWGATES is port (
  a,b,c,d: in std_logic;
  y: out std_logic
  );
end FEWGATES;

architecture structural of FEWGATES is

  component AND2 port (
    i1: in std_logic;
    i2: in std_logic;
    y: out std_logic
    );
  end component;

  component OR2 port (
    i1: in std_logic;
    i2: in std_logic;
    y: out std_logic
    );
  end component;

  component INVERTER port (
    i: in std_logic;
    o: out std_logic
    );
  end component;

signal a_and_b, c_and_d, not_c_and_d: std_logic;

begin

  u1: and2 port map (i1 => a ,
                     i2 => b,
                     y => a_and_b
                     );

  u2: and2 port map (i1 => c,
                     i2 => d,
                     y => c_and_d
                     );

  u3: inverter port map (i => c_and_d,
                         o => not_c_and_d);

  u4: or2 port map (i1 => a_and_b,
                    i2 => not_c_and_d,
                    y => y
                    );
end structural;
library IEEE;
use IEEE.std_logic_1164.all;

use work.simPrimitives.all;

entity simHierarchy is port (
  A, B, Clk: in std_logic;
  Y: out std_logic
  );
end simHierarchy;

architecture hierarchical of simHierarchy is

signal ADly, BDly, OrGateDly, ClkDly: std_logic;
signal OrGate, FlopOut: std_logic;

begin

  ADly <= transport A after 2 ns;
  BDly <= transport B after 2 ns;
  OrGateDly <= transport OrGate after 1.5 ns;
  ClkDly <= transport Clk after 1 ns;

  u1: OR2 generic map (tPD => 10 ns)
          port map ( I1 => ADly,
                     I2 => BDly,
                     Y => OrGate
                   );

  u2: simDFF generic map ( tS => 4 ns,
                           tH => 3 ns,
                           tCQ => 8 ns
                         )
             port map ( D => OrGateDly,
                        Clk => ClkDly,
                        Q => FlopOut
                      );

  Y <= transport FlopOut after 2 ns;

end hierarchical;
library IEEE;
use IEEE.std_logic_1164.all;

library IEEE;
use IEEE.std_logic_1164.all;

entity INVERTER is port (
    i: in std_logic;
    o: out std_logic
    );
end INVERTER;

architecture rtl of INVERTER is

begin

  o <= not i;

end rtl;
--------------------------------------------------------------------------------
--| File name   : $RCSfile: io1164.vhd $
--| Library     : SUPPORT
--| Revision    : $Revision: 1.1 $
--| Author(s)   : Vantage Analysis Systems, Inc; Des Young
--| Integration : Des Young
--| Creation    : Nov 1995
--| Status      : $State: Exp $
--|
--| Purpose     : IO routines for std_logic_1164.
--| Assumptions : Numbers use radixed character set with no prefix.
--| Limitations : Does not read VHDL pound-radixed numbers.
--| Known Errors: none
--|
--| Description:
--| This is a modified library. The source is basically that donated by
--| Vantage to libutil. Des Young removed std_ulogic_vector support (to
--| conform to synthesizable libraries), and added read_oct/hex to integer.
--|
--| =======================================================================
--| Copyright (c) 1992-1994 Vantage Analysis Systems, Inc., all rights
--| reserved. This package is provided by Vantage Analysis Systems.
--| The package may not be sold without the express written consent of
--| Vantage Analysis Systems, Inc.
--|
--| The VHDL for this package may be copied and/or distributed as long as
--| this copyright notice is retained in the source and any modifications
--| are clearly marked in the History: list.
--|
--| Title       : IO1164 package VHDL source
--| Package Name: somelib.IO1164
--| File Name   : io1164.vhdl
--| Author(s)   : dbb
--| Purpose     : * Overloads procedures READ and WRITE for STD_LOGIC types
--|                 in manner consistent with TEXTIO package.
--|               * Provides procedures to read and write logic values as
--|                 binary, octal, or hexadecimal values ('X' as appropriate).
--|                 These should be particularly useful for models
--|                 to read in stimulus as 0/1/x or octal or hex.
--| Subprograms :
--| Notes       : 
--| History     : 1. Donated to libutil by Dave Bernstein 15 Jun 94
--|               2. Removed all std_ulogic_vector support, Des Young, 14 Nov 95
--|                  (This is because that type is not supported for synthesis).
--|               3. Added read_oct/hex to integer, Des Young, 20 Nov 95
--|
--| =======================================================================
--| Extra routines by Des Young, des@alantec.com. 1995. GNU copyright.
--| =======================================================================
--|
--------------------------------------------------------------------------------

library ieee;
package io1164 is

    --$ !VANTAGE_METACOMMENTS_ON
    --$ !VANTAGE_DNA_ON

    -- import std_logic package
    use ieee.std_logic_1164.all;

    -- import textio package
    use std.textio.all;

    --
    -- the READ and WRITE procedures act similarly to the procedures in the
    -- STD.TEXTIO package.  for each type, there are two read procedures and
    -- one write procedure for converting between character and internal
    -- representations of values.  each value is represented as the string of
    -- characters that you would use in VHDL code.  (remember that apostrophes
    -- and quotation marks are not used.)  input is case-insensitive.  output
    -- is in upper case.  see the following LRM sections for more information:
    --
    --      2.3   - Subprogram Overloading
    --      3.3   - Access Types             (STD.TEXTIO.LINE is an access type)
    --      7.3.6 - Allocators               (allocators create access values)
    --     14.3   - Package TEXTIO
    --

    -- Note that the procedures for std_ulogic will match calls with the value
    -- parameter of type std_logic.

    --
    -- declare READ procedures to overload like in TEXTIO
    --
    procedure read(l: inout line; value: out std_ulogic       ; good: out boolean);
    procedure read(l: inout line; value: out std_ulogic                          );
    procedure read(l: inout line; value: out std_logic_vector ; good: out boolean);
    procedure read(l: inout line; value: out std_logic_vector                    );

    --
    -- declare WRITE procedures to overload like in TEXTIO
    --
    procedure write(l        : inout line                  ;
                    value    : in    std_ulogic            ;
                    justified: in    side          := right;
                    field    : in    width         := 0   );
    procedure write(l        : inout line                  ;
                    value    : in    std_logic_vector      ;
                    justified: in    side          := right;
                    field    : in    width         := 0   );

    --
    -- declare procedures to convert between logic values and octal
    -- or hexadecimal ('X' where appropriate).
    --

    -- octal / std_logic_vector
    procedure read_oct (l         : inout line                    ;
                        value     : out   std_logic_vector        ;
                        good      : out   boolean                );
    procedure read_oct (l         : inout line                    ;
                        value     : out   std_logic_vector       );
    procedure write_oct(l         : inout line                    ;
                        value     : in    std_logic_vector        ;
                        justified : in    side            := right;
                        field     : in    width           := 0   );

    -- hexadecimal / std_logic_vector
    procedure read_hex (l         : inout line                    ;
                        value     : out   std_logic_vector        ;
                        good      : out   boolean                );
    procedure read_hex (l         : inout line                    ;
                        value     : out   std_logic_vector       );
    procedure write_hex(l         : inout line                    ;
                        value     : in    std_logic_vector        ;
                        justified : in    side            := right;
                        field     : in    width           := 0   );

    -- read a number into an integer
    procedure read_oct(l     : inout line;
                       value : out   integer;
                       good  : out   boolean);
    procedure read_oct(l     : inout line;
                       value : out   integer);
    procedure read_hex(l     : inout line;
                       value : out   integer;
                       good  : out   boolean);
    procedure read_hex(l     : inout line;
                       value : out   integer);

end io1164;

--------------------------------------------------------------------------------
--| Copyright (c) 1992-1994 Vantage Analysis Systems, Inc., all rights reserved
--| This package is provided by Vantage Analysis Systems.
--| The package may not be sold without the express written consent of
--| Vantage Analysis Systems, Inc.
--|
--| The VHDL for this package may be copied and/or distributed as long as
--| this copyright notice is retained in the source and any modifications
--| are clearly marked in the History: list.
--|
--| Title       : IO1164 package body VHDL source
--| Package Name: VANTAGE_LOGIC.IO1164
--| File Name   : io1164.vhdl
--| Author(s)   : dbb
--| Purpose     : source for IO1164 package body
--| Subprograms :
--| Notes       : see package declaration
--| History     : see package declaration
--------------------------------------------------------------------------------

package body io1164 is


    --$ !VANTAGE_METACOMMENTS_ON
    --$ !VANTAGE_DNA_ON

    -- define lowercase conversion of characters for canonical comparison
    type char2char_t is array (character'low to character'high) of character;
    constant lowcase: char2char_t := (
        nul, soh, stx, etx, eot, enq, ack, bel,
        bs,  ht,  lf,  vt,  ff,  cr,  so,  si,
        dle, dc1, dc2, dc3, dc4, nak, syn, etb,
        can, em,  sub, esc, fsp, gsp, rsp, usp, 

        ' ', '!', '"', '#', '$', '%', '&', ''',
        '(', ')', '*', '+', ',', '-', '.', '/',
        '0', '1', '2', '3', '4', '5', '6', '7',
        '8', '9', ':', ';', '<', '=', '>', '?',

        '@', 'a', 'b', 'c', 'd', 'e', 'f', 'g',
        'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
        'p', 'q', 'r', 's', 't', 'u', 'v', 'w',
        'x', 'y', 'z', '[', '\', ']', '^', '_',

        '`', 'a', 'b', 'c', 'd', 'e', 'f', 'g',
        'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
        'p', 'q', 'r', 's', 't', 'u', 'v', 'w',
        'x', 'y', 'z', '{', '|', '}', '~', del);

    -- define conversions between various types

    -- logic    -> character
    type f_logic_to_character_t is 
        array (std_ulogic'low to std_ulogic'high) of character;
    constant f_logic_to_character : f_logic_to_character_t := 
        ( 
          'U' => 'U', 
          'X' => 'X',
          '0' => '0',
          '1' => '1',
          'Z' => 'Z',
          'W' => 'W',
          'L' => 'L',
          'H' => 'H',
          '-' => '-'
        );

    -- character, integer, logic

    constant x_charcode     : integer := -1;
    constant maxoct_charcode: integer := 7;
    constant maxhex_charcode: integer := 15;
    constant bad_charcode   : integer := integer'left;

    type digit2int_t is 
        array ( character'low to character'high ) of integer;
    constant octdigit2int: digit2int_t := (
          '0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4,
          '5' => 5, '6' => 6, '7' => 7,
          'X' | 'x' => x_charcode, others => bad_charcode );
    constant hexdigit2int: digit2int_t := (
          '0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4,
          '5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9, 
          'A' | 'a' => 10, 'B' | 'b' => 11, 'C' | 'c' => 12,
          'D' | 'd' => 13, 'E' | 'e' => 14, 'F' | 'f' => 15,
          'X' | 'x' => x_charcode, others => bad_charcode  );

    constant oct_bits_per_digit: integer := 3;
    constant hex_bits_per_digit: integer := 4;

    type     int2octdigit_t is 
        array ( 0 to maxoct_charcode ) of character;
    constant int2octdigit: int2octdigit_t :=
        (  0 => '0',  1 => '1',  2 => '2',  3 => '3',
           4 => '4',  5 => '5',  6 => '6',  7 => '7' );

    type     int2hexdigit_t is 
        array ( 0 to maxhex_charcode ) of character;
    constant int2hexdigit: int2hexdigit_t :=
        (  0 => '0',  1 => '1',  2 => '2',  3 => '3',
           4 => '4',  5 => '5',  6 => '6',  7 => '7',
           8 => '8',  9 => '9', 10 => 'A', 11 => 'B',
          12 => 'C', 13 => 'D', 14 => 'E', 15 => 'F' );

    type     oct_logic_vector_t is
        array(1 to oct_bits_per_digit) of std_ulogic;
    type     octint2logic_t is 
        array (x_charcode to maxoct_charcode) of oct_logic_vector_t;
    constant octint2logic  : octint2logic_t := (
        ( 'X', 'X', 'X' ),
        ( '0', '0', '0' ),
        ( '0', '0', '1' ),
        ( '0', '1', '0' ),
        ( '0', '1', '1' ),
        ( '1', '0', '0' ),
        ( '1', '0', '1' ),
        ( '1', '1', '0' ),
        ( '1', '1', '1' )
    );

    type     hex_logic_vector_t is
        array(1 to hex_bits_per_digit) of std_ulogic;
    type     hexint2logic_t is 
        array (x_charcode to maxhex_charcode) of hex_logic_vector_t;
    constant hexint2logic  : hexint2logic_t := (
        ( 'X', 'X', 'X', 'X' ),
        ( '0', '0', '0', '0' ),
        ( '0', '0', '0', '1' ),
        ( '0', '0', '1', '0' ),
        ( '0', '0', '1', '1' ),
        ( '0', '1', '0', '0' ),
        ( '0', '1', '0', '1' ),
        ( '0', '1', '1', '0' ),
        ( '0', '1', '1', '1' ),
        ( '1', '0', '0', '0' ),
        ( '1', '0', '0', '1' ),
        ( '1', '0', '1', '0' ),
        ( '1', '0', '1', '1' ),
        ( '1', '1', '0', '0' ),
        ( '1', '1', '0', '1' ),
        ( '1', '1', '1', '0' ),
        ( '1', '1', '1', '1' )
    );

    ----------------------------------------------------------------------------
    -- READ procedure bodies
    --
    --     The strategy for duplicating TEXTIO's overloading of procedures
    --     with and without GOOD parameters is to put all the logic in the
    --     version with the GOOD parameter and to have the version without
    --     GOOD approximate a runtime error by use of an assertion.
    --
    ----------------------------------------------------------------------------

    --
    -- std_ulogic
    --     note: compatible with std_logic
    --

    procedure read( l: inout line; value: out std_ulogic; good : out boolean ) is

        variable c      : character;        -- char read while looping
        variable m      : line;             -- safe copy of L
        variable success: boolean := false; -- readable version of GOOD
        variable done   : boolean := false; -- flag to say done reading chars

    begin

        --
        -- algorithm:
        -- 
        --     if there are characters in the line
        --         save a copy of the line
        --         get the next character
        --         if got one
        --             set value
        --         if all ok
        --             free temp copy
        --         else
        --             free passed in line
        --             assign copy back to line
        --         set GOOD
        --     

        -- only operate on lines that contain characters
        if ( ( l /= null ) and ( l.all'length /= 0 ) ) then

            -- save a copy of string in case read fails
            m := new string'( l.all );
    
            -- grab the next character
            read( l, c, success );

            -- if read ok    
            if success then

--
-- an issue here is whether lower-case values should be accepted or not
--

                -- determine the value    
                case c is
                    when 'U' | 'u' => value := 'U';
                    when 'X' | 'x' => value := 'X';
                    when '0'       => value := '0';
                    when '1'       => value := '1';
                    when 'Z' | 'z' => value := 'Z';
                    when 'W' | 'w' => value := 'W';
                    when 'L' | 'l' => value := 'L';
                    when 'H' | 'h' => value := 'H';
                    when '-'       => value := '-';
                    when others    => success := false;
                end case;

            end if;

            -- free working storage
            if success then
                deallocate( m );
            else
                deallocate( l );
                l := m;
            end if;

        end if; -- non null access, non empty string

        -- set output parameter
        good := success;

    end read;

    procedure read( l: inout line; value: out std_ulogic ) is
        variable success: boolean;  -- internal good flag
    begin
        read( l, value, success );  -- use safe version
        assert success 
            report "IO1164.READ: Unable to read STD_ULOGIC value." 
            severity error;
    end read;

    --
    -- std_logic_vector
    --     note: NOT compatible with std_ulogic_vector
    --

    procedure read(l    : inout line           ; 
                   value: out   std_logic_vector;
                   good : out   boolean        ) is

        variable m           : line           ; -- saved copy of L
        variable success     : boolean := true; -- readable GOOD
        variable logic_value : std_logic      ; -- value for one array element
        variable c           : character      ; -- read a character

    begin

        --
        -- algorithm:
        -- 
        --     this procedure strips off leading whitespace, and then calls the
        --     READ procedure for each single logic value element in the output
        --     array.
        --     

        -- only operate on lines that contain characters
        if ( ( l /= null ) and ( l.all'length /= 0 ) ) then

            -- save a copy of string in case read fails
            m := new string'( l.all );

            -- loop for each element in output array
            for i in value'range loop

                -- prohibit internal blanks
                if i /= value'left then
                    if l.all'length = 0 then
                        success := false;
                        exit;
                    end if;
                    c := l.all(l.all'left);
                    if c = ' ' or c = ht then
                        success := false;
                        exit;
                    end if;
                end if;

                -- read the next logic value
                read( l, logic_value, success );

                -- stuff the value in if ok, else bail out
                if success then
                    value( i ) := logic_value;
                else
                    exit;
                end if;

            end loop; -- each element in output array

            -- free working storage
            if success then
                deallocate( m );
            else
                deallocate( l );
                l := m;
            end if;

        elsif ( value'length /= 0 ) then
            -- string is empty but the  return array has 1+ elements
            success := false;
        end if;

        -- set output parameter
        good := success;

    end read;

    procedure read(l: inout line; value: out std_logic_vector ) is
        variable success: boolean;
    begin
        read( l, value, success );
        assert success 
            report "IO1164.READ: Unable to read T_WLOGIC_VECTOR value." 
            severity error;
    end read;

    ----------------------------------------------------------------------------
    -- WRITE procedure bodies
    ----------------------------------------------------------------------------

    --
    -- std_ulogic
    --     note: compatible with std_logic
    --

    procedure write(l        : inout line          ;
                    value    : in    std_ulogic    ;
                    justified: in    side  := right;
                    field    : in    width := 0    ) is
    begin

        --
        -- algorithm:
        -- 
        --     just write out the string associated with the enumerated
        --     value.
        --     

        case value is
            when 'U' => write( l, character'('U'), justified, field );
            when 'X' => write( l, character'('X'), justified, field );
            when '0' => write( l, character'('0'), justified, field );
            when '1' => write( l, character'('1'), justified, field );
            when 'Z' => write( l, character'('Z'), justified, field );
            when 'W' => write( l, character'('W'), justified, field );
            when 'L' => write( l, character'('L'), justified, field );
            when 'H' => write( l, character'('H'), justified, field );
            when '-' => write( l, character'('-'), justified, field );
        end case;
    end write;

    --
    -- std_logic_vector
    --     note: NOT compatible with std_ulogic_vector
    --

    procedure write(l        : inout line                   ;
                    value    : in    std_logic_vector       ;
                    justified: in    side           := right;
                    field    : in    width          := 0    ) is

        variable m: line; -- build up intermediate string

    begin

        --
        -- algorithm:
        -- 
        --     for each value in array
        --         add string representing value to intermediate string
        --     write intermediate string to line parameter
        --     free intermediate string
        --     

        -- for each value in array
        for i in value'range loop

            -- add string representing value to intermediate string
            write( m, value( i ) );

        end loop;

        -- write intermediate string to line parameter
        write( l, m.all, justified, field );

        -- free intermediate string
        deallocate( m );

    end write;


--------------------------------------------------------------------------------

    ----------------------------------------------------------------------------
    -- procedure bodies for octal and hexadecimal read and write
    ----------------------------------------------------------------------------

    --
    -- std_logic_vector/octal
    --     note: NOT compatible with std_ulogic_vector
    --

    procedure read_oct(l         : inout line            ; 
                       value     : out   std_logic_vector; 
                       good      : out   boolean         ) is

        variable m               : line                      ; -- safe L
        variable success         : boolean            := true; -- readable GOOD
        variable logic_value     : std_logic                 ; -- elem value
        variable c               : character                 ; -- char read
        variable charcode        : integer                   ; -- char->int
        variable oct_logic_vector: oct_logic_vector_t        ; -- for 1 digit
        variable bitpos          : integer                   ; -- in state vec.
    begin

        --
        -- algorithm:
        -- 
        --     skip over leading blanks, then read a digit
        --     and do a conversion into a logic value
        --     for each element in array
        --     

        -- make sure logic array is right size to read this base
        success := ( ( value'length rem oct_bits_per_digit ) = 0 );
        if success then

            -- only operate on non-empty strings
            if ( ( l /= null ) and ( l.all'length /= 0 ) ) then

                -- save old copy of string in case read fails
                m := new string'( l.all );

                -- pick off leading white space and get first significant char
                c := ' ';
                while success and ( l.all'length > 0 ) and ( ( c = ' ' ) or ( c = ht ) ) loop
                    read( l, c, success );
                end loop;

                -- turn character into integer
                charcode := octdigit2int( c );

                -- not doing any bits yet
                bitpos := 0;

                -- check for bad first character
                if charcode = bad_charcode then
                    success := false;
                else
                    -- loop through each value in array
                    oct_logic_vector := octint2logic( charcode );
                    for i in value'range loop
    
                        -- doing the next bit
                        bitpos := bitpos + 1;
    
                        -- stick the value in
                        value( i ) := oct_logic_vector( bitpos );
                        
                        -- read the next character if we're not at array end
                        if ( bitpos = oct_bits_per_digit ) and ( i /= value'right ) then
                            read( l, c, success );
                            if not success then
                                exit;
                            end if;
                            -- turn character into integer
                            charcode := octdigit2int( c );
                            -- check for bad char
                            if charcode = bad_charcode then
                                success := false;
                                exit;
                            end if;
                            -- reset bit position
                            bitpos := 0;
                            -- turn character code into state array 
                            oct_logic_vector := octint2logic( charcode );
                        end if;
                        
                    end loop; -- each index in return array
 
                end if; -- if bad first character

                -- clean up working storage
                if success then
                    deallocate( m );
                else
                    deallocate( l );
                    l := m;
                end if;

            -- no characters to read for return array that isn't null slice
            elsif ( value'length /= 0 ) then
                success := false;
            end if; -- non null access, non empty string

        end if;

        -- set out parameter of success
        good := success;

    end read_oct;


    procedure read_oct(l         : inout line            ; 
                       value     : out   std_logic_vector) is
        variable success: boolean;                 -- internal good flag
    begin                                                              
        read_oct( l, value, success ); -- use safe version  
        assert success 
            report "IO1164.READ_OCT: Unable to read T_LOGIC_VECTOR value." 
            severity error;
    end read_oct;

    procedure write_oct(l        : inout line                   ;
                        value    : in    std_logic_vector       ;
                        justified: in    side           := right;
                        field    : in    width          := 0    ) is

        variable m            : line     ; -- safe copy of L
        variable goodlength   : boolean  ; -- array is ok len for this base
        variable isx          : boolean  ; -- an X in this digit
        variable integer_value: integer  ; -- accumulate integer value
        variable c            : character; -- character read
        variable charpos      : integer  ; -- index string being contructed
        variable bitpos       : integer  ; -- bit index inside digit

    begin

        --
        -- algorithm:
        -- 
        -- make sure this array can be written in this base
        -- create a string to place intermediate results
        -- initialize counters and flags to beginning of string
        -- for each item in array
        --     note unknown, else accumulate logic into integer
        --         if at this digit's last bit
        --             stuff digit just computed into intermediate result
        --             reset flags and counters except for charpos
        -- write intermediate result into line
        -- free work storage
        --     

        -- make sure this array can be written in this base
        goodlength := ( ( value'length rem oct_bits_per_digit ) = 0 );
        assert goodlength
            report "IO1164.WRITE_OCT: VALUE'Length is not a multiple of 3."
            severity error;
        if goodlength then

            -- create a string to place intermediate results
            m := new string(1 to ( value'length / oct_bits_per_digit ) );

            -- initialize counters and flags to beginning of string
            charpos := 0;
            bitpos := 0;
            isx := false;
            integer_value := 0;

            -- for each item in array
            for i in value'range loop

                -- note unknown, else accumulate logic into integer
                case value(i) is
                    when '0' | 'L' =>
                        integer_value := integer_value * 2;
                    when '1' | 'H' =>
                        integer_value := ( integer_value * 2 ) + 1;
                    when others =>
                        isx := true;
                end case;

                -- see if we've done this digit's last bit
                bitpos := bitpos + 1;
                if bitpos = oct_bits_per_digit then

                    -- stuff the digit just computed into the intermediate result
                    charpos := charpos + 1;
                    if isx then
                        m.all(charpos) := 'X';
                    else
                        m.all(charpos) := int2octdigit( integer_value );
                    end if;

                    -- reset flags and counters except for location in string being constructed
                    bitpos := 0;
                    isx := false;
                    integer_value := 0;

                end if;

            end loop;

            -- write intermediate result into line
            write( l, m.all, justified, field );

            -- free work storage
            deallocate( m );

        end if;

    end write_oct;

    --
    -- std_logic_vector/hexadecimal
    --     note: NOT compatible with std_ulogic_vector
    --

    procedure read_hex(l         : inout line            ; 
                       value     : out   std_logic_vector; 
                       good      : out   boolean         ) is

        variable m               : line                      ; -- safe L
        variable success         : boolean            := true; -- readable GOOD
        variable logic_value     : std_logic                 ; -- elem value
        variable c               : character                 ; -- char read
        variable charcode        : integer                   ; -- char->int
        variable hex_logic_vector: hex_logic_vector_t        ; -- for 1 digit
        variable bitpos          : integer                   ; -- in state vec.
    begin

        --
        -- algorithm:
        -- 
        --     skip over leading blanks, then read a digit
        --     and do a conversion into a logic value
        --     for each element in array
        --     

        -- make sure logic array is right size to read this base
        success := ( ( value'length rem hex_bits_per_digit ) = 0 );
        if success then

            -- only operate on non-empty strings
            if ( ( l /= null ) and ( l.all'length /= 0 ) ) then

                -- save old copy of string in case read fails
                m := new string'( l.all );

                -- pick off leading white space and get first significant char
                c := ' ';
                while success and ( l.all'length > 0 ) and ( ( c = ' ' ) or ( c = ht ) ) loop
                    read( l, c, success );
                end loop;

                -- turn character into integer
                charcode := hexdigit2int( c );

                -- not doing any bits yet
                bitpos := 0;

                -- check for bad first character
                if charcode = bad_charcode then
                    success := false;
                else
                    -- loop through each value in array
                    hex_logic_vector := hexint2logic( charcode );
                    for i in value'range loop
    
                        -- doing the next bit
                        bitpos := bitpos + 1;
    
                        -- stick the value in
                        value( i ) := hex_logic_vector( bitpos );
                        
                        -- read the next character if we're not at array end
                        if ( bitpos = hex_bits_per_digit ) and ( i /= value'right ) then
                            read( l, c, success );
                            if not success then
                                exit;
                            end if;
                            -- turn character into integer
                            charcode := hexdigit2int( c );
                            -- check for bad char
                            if charcode = bad_charcode then
                                success := false;
                                exit;
                            end if;
                            -- reset bit position
                            bitpos := 0;
                            -- turn character code into state array 
                            hex_logic_vector := hexint2logic( charcode );
                        end if;
                        
                    end loop; -- each index in return array
 
                end if; -- if bad first character

                -- clean up working storage
                if success then
                    deallocate( m );
                else
                    deallocate( l );
                    l := m;
                end if;

            -- no characters to read for return array that isn't null slice
            elsif ( value'length /= 0 ) then
                success := false;
            end if; -- non null access, non empty string

        end if;

        -- set out parameter of success
        good := success;

    end read_hex;


    procedure read_hex(l         : inout line            ; 
                       value     : out   std_logic_vector) is
        variable success: boolean;                 -- internal good flag
    begin                                                              
        read_hex( l, value, success ); -- use safe version  
        assert success 
            report "IO1164.READ_HEX: Unable to read T_LOGIC_VECTOR value." 
            severity error;
    end read_hex;

    procedure write_hex(l        : inout line                   ;
                        value    : in    std_logic_vector       ;
                        justified: in    side           := right;
                        field    : in    width          := 0    ) is

        variable m            : line     ; -- safe copy of L
        variable goodlength   : boolean  ; -- array is ok len for this base
        variable isx          : boolean  ; -- an X in this digit
        variable integer_value: integer  ; -- accumulate integer value
        variable c            : character; -- character read
        variable charpos      : integer  ; -- index string being contructed
        variable bitpos       : integer  ; -- bit index inside digit

    begin

        --
        -- algorithm:
        -- 
        -- make sure this array can be written in this base
        -- create a string to place intermediate results
        -- initialize counters and flags to beginning of string
        -- for each item in array
        --     note unknown, else accumulate logic into integer
        --         if at this digit's last bit
        --             stuff digit just computed into intermediate result
        --             reset flags and counters except for charpos
        -- write intermediate result into line
        -- free work storage
        --     

        -- make sure this array can be written in this base
        goodlength := ( ( value'length rem hex_bits_per_digit ) = 0 );
        assert goodlength
            report "IO1164.WRITE_HEX: VALUE'Length is not a multiple of 4."
            severity error;
        if goodlength then

            -- create a string to place intermediate results
            m := new string(1 to ( value'length / hex_bits_per_digit ) );

            -- initialize counters and flags to beginning of string
            charpos := 0;
            bitpos := 0;
            isx := false;
            integer_value := 0;

            -- for each item in array
            for i in value'range loop

                -- note unknown, else accumulate logic into integer
                case value(i) is
                    when '0' | 'L' =>
                        integer_value := integer_value * 2;
                    when '1' | 'H' =>
                        integer_value := ( integer_value * 2 ) + 1;
                    when others =>
                        isx := true;
                end case;

                -- see if we've done this digit's last bit
                bitpos := bitpos + 1;
                if bitpos = hex_bits_per_digit then

                    -- stuff the digit just computed into the intermediate result
                    charpos := charpos + 1;
                    if isx then
                        m.all(charpos) := 'X';
                    else
                        m.all(charpos) := int2hexdigit( integer_value );
                    end if;

                    -- reset flags and counters except for location in string being constructed
                    bitpos := 0;
                    isx := false;
                    integer_value := 0;

                end if;

            end loop;

            -- write intermediate result into line
            write( l, m.all, justified, field );

            -- free work storage
            deallocate( m );

        end if;

    end write_hex;

------------------------------------------------------------------------------

    ------------------------------------
    -- Read octal/hex numbers to integer
    ------------------------------------

    --
    -- Read octal to integer
    --
    
    procedure read_oct(l     : inout line;
                       value : out integer;
                       good  : out boolean) is

        variable pos     : integer;
        variable digit   : integer;
        variable result  : integer := 0;
        variable success : boolean := true;
        variable c       : character;
        variable old_l   : line := l;

    begin
        -- algorithm:
        --
        --  skip leading white space, read digit, convert
        --  into integer
        --
        if (l /= NULL) then
            -- set pos to start of actual number by skipping white space
            pos := l'LEFT;
            c := l(pos);
            while ( l.all'length > 0 ) and ( ( c = ' ' ) or ( c = HT ) ) loop
                pos := pos + 1;
                c := l(pos);
            end loop;
            
            -- check for start of valid number
            digit := octdigit2int(l(pos));

            if ((digit = bad_charcode) or (digit = x_charcode)) then
                good := FALSE;
                return;
            else
                -- calculate integer value
                for i in pos to l'RIGHT loop
                    digit := octdigit2int(l(pos));
                    exit when (digit = bad_charcode) or (digit = x_charcode);
                    result := (result * 8) + digit;
                    pos := pos + 1;
                end loop;
                value := result;
                -- shrink line
                if (pos > 1) then
                    l := new string'(old_l(pos to old_l'HIGH));
                    deallocate(old_l);
                end if;
                good := TRUE;
                return;
            end if;
        else
            good := FALSE;
        end if;

    end read_oct;

    -- simple version
    procedure read_oct(l     : inout line;
                       value : out   integer) is

        variable success: boolean;                 -- internal good flag

    begin                                                              
        read_oct( l, value, success ); -- use safe version  
        assert success 
            report "IO1164.READ_OCT: Unable to read octal integer value." 
            severity error;
    end read_oct;


    --
    -- Read hex to integer
    --
    
    procedure read_hex(l     : inout line;
                       value : out integer;
                       good  : out boolean) is

        variable pos     : integer;
        variable digit   : integer;
        variable result  : integer := 0;
        variable success : boolean := true;
        variable c       : character;
        variable old_l   : line := l;

    begin
        -- algorithm:
        --
        --  skip leading white space, read digit, convert
        --  into integer
        --
        if (l /= NULL) then
            -- set pos to start of actual number by skipping white space
            pos := l'LEFT;
            c := l(pos);
            while ( l.all'length > 0 ) and ( ( c = ' ' ) or ( c = HT ) ) loop
                pos := pos + 1;
                c := l(pos);
            end loop;
            
            -- check for start of valid number
            digit := hexdigit2int(l(pos));

            if ((digit = bad_charcode) or (digit = x_charcode)) then
                good := FALSE;
                return;
            else
                -- calculate integer value
                for i in pos to l'RIGHT loop
                    digit := hexdigit2int(l(pos));
                    exit when (digit = bad_charcode) or (digit = x_charcode);
                    result := (result * 16) + digit;
                    pos := pos + 1;
                end loop;
                value := result;
                -- shrink line
                if (pos > 1) then
                    l := new string'(old_l(pos to old_l'HIGH));
                    deallocate(old_l);
                end if;
                good := TRUE;
                return;
            end if;
        else
            good := FALSE;
        end if;

    end read_hex;

    -- simple version
    procedure read_hex(l     : inout line;
                       value : out   integer) is

        variable success: boolean;                 -- internal good flag

    begin                                                              
        read_hex( l, value, success ); -- use safe version  
        assert success 
            report "IO1164.READ_HEX: Unable to read hex integer value." 
            severity error;
    end read_hex;

end io1164;
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity asyncLdCnt is port (
  loadVal: in std_logic_vector(3 downto 0);
  clk, load: in std_logic;
  q: out std_logic_vector(3 downto 0)
  );
end asyncLdCnt;

architecture rtl of asyncLdCnt is

signal qLocal: unsigned(3 downto 0);

begin

  process (clk, load, loadVal) begin
    if (load = '1') then
      qLocal <= to_unsigned(loadVal);
    elsif (clk'event and clk = '1' ) then
      qLocal <= qLocal + 1;
    end if;
  end process;

  q <= to_stdlogicvector(qLocal);

end rtl;
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;

entity LoadCnt is port (
  CntEn: in std_logic;
  LdCnt: in std_logic;
  LdData: in std_logic_vector(3 downto 0);
  Clk: in std_logic;
  Rst: in std_logic;
  CntVal: out std_logic_vector(3 downto 0)
  );
end LoadCnt;

architecture behavioral of LoadCnt is

signal Cnt: std_logic_vector(3 downto 0);

begin

  counter: process (Clk, Rst) begin
    if Rst = '1' then
      Cnt <= (others => '0');
    elsif (Clk'event and Clk = '1') then
      if (LdCnt = '1') then
        Cnt <= LdData;
      elsif (CntEn = '1') then
        Cnt <= Cnt + 1;
      else
        Cnt <= Cnt;
      end if;
    end if;
  end process;

  CntVal <= Cnt;

end behavioral;
library IEEE;
use IEEE.std_logic_1164.all;
library UTILS;
use UTILS.io1164.all;
use std.textio.all;

entity loadCntTB is
end loadCntTB;

architecture testbench of loadCntTB is

  component loadCnt port (
    data: in std_logic_vector (7 downto 0);
    load: in std_logic;
    clk: in std_logic;
    rst: in std_logic;
    q: out std_logic_vector (7 downto 0)
    );
  end component;

file vectorFile: text is in "vectorfile";
type vectorType is record
  data: std_logic_vector(7 downto 0);
  load: std_logic;
  rst: std_logic;
  q: std_logic_vector(7 downto 0);
end record;

signal testVector: vectorType;
signal TestClk: std_logic := '0';
signal Qout: std_logic_vector(7 downto 0);

constant ClkPeriod: time := 100 ns;

for all: loadCnt use entity work.loadcnt(rtl);

begin

-- File reading and stimulus application
  readVec: process
    variable VectorLine: line;
    variable VectorValid: boolean;
    variable vRst: std_logic;
    variable vLoad: std_logic;
    variable vData: std_logic_vector(7 downto 0);
    variable vQ: std_logic_vector(7 downto 0);
    
  begin
    while not endfile (vectorFile) loop
      readline(vectorFile, VectorLine);

      read(VectorLine, vRst, good => VectorValid);
      next when not VectorValid;
      read(VectorLine, vLoad);
      read(VectorLine, vData);
      read(VectorLine, vQ);

      wait for ClkPeriod/4;

      testVector.Rst <= vRst;
      testVector.Load <= vLoad;
      testVector.Data <= vData;
      testVector.Q <= vQ;

      wait for (ClkPeriod/4) * 3;

    end loop;

  assert false
    report "Simulation complete"
    severity note;

  wait;

  end process;

-- Free running test clock
  TestClk <= not TestClk after ClkPeriod/2;

-- Instance of design being tested
  u1: loadCnt port map (Data => testVector.Data,
                        load => testVector.Load,
                        clk => TestClk,
                        rst => testVector.Rst,
                        q => Qout
                       );

-- Process to verify outputs
  verify: process (TestClk)
  variable ErrorMsg: line;
  begin
    if (TestClk'event and TestClk = '0') then
      if Qout /= testVector.Q then
        write(ErrorMsg, string'("Vector failed "));
        write(ErrorMsg, now);
        writeline(output, ErrorMsg);
      end if;
    end if;
  end process;
                        

end testbench;
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;

entity loadCnt is port (
  data: in std_logic_vector (7 downto 0);
  load: in std_logic;
  clk: in std_logic;
  rst: in std_logic;
  q: out std_logic_vector (7 downto 0)
  );
end loadCnt;

architecture rtl of loadCnt is

signal cnt: std_logic_vector (7 downto 0);

begin

  counter: process (clk, rst) begin
    if (rst = '1') then
      cnt <= (others => '0');
    elsif (clk'event and clk = '1') then
      if (load = '1') then
        cnt <= data;
      else
        cnt <= cnt + 1;
      end if;
    end if;
  end process;

  q <= cnt;

end rtl;
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;

entity multiplier is port (
  a,b : in std_logic_vector (15 downto 0);
  product: out std_logic_vector (31 downto 0)
  );
end multiplier;

architecture dataflow of multiplier is

begin

  product <= a * b;

end dataflow;
library IEEE;
use IEEE.std_logic_1164.all;

entity mux is port (
  A, B, Sel: in std_logic;
  Y: out std_logic
  );
end mux;

architecture simModel of mux is

-- Delay Constants
constant tPD_A:   time := 10 ns;
constant tPD_B:   time := 15 ns;
constant tPD_Sel: time := 5 ns;

begin

  DelayMux: process (A, B, Sel)

  variable localY: std_logic; -- Zero delay place holder for Y

  begin

  -- Zero delay model
    case Sel is
      when '0' =>
        localY := A;
      when others =>
        localY := B;
    end case;

  -- Delay calculation
    if (B'event) then
      Y <= localY after tPD_B;
    elsif (A'event) then
      Y <= localY after tPD_A;
    else
      Y <= localY after tPD_Sel;
    end if;

  end process;


end simModel;
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;

entity ForceShare is port (
  a,b,c,d,e,f: in std_logic_vector (7 downto 0);
  result: out std_logic_vector(7 downto 0)
  );
end ForceShare;

architecture behaviour of ForceShare is

begin

  sum: process (a,c,b,d,e,f)
  begin

    if (a + b = "10011010") then
      result <= c;
    elsif (a + b = "01011001") then
          result <= d;
    elsif (a + b = "10111011") then
      result <= e;
    else
      result <= f;
    end if;
  end process;

end behaviour;
library IEEE;
use IEEE.std_logic_1164.all;

entity TRIBUF8 is port (
  ip: in std_logic_vector(7 downto 0);
  oe: in std_logic;
  op: out std_logic_vector(7 downto 0)
  );
end TRIBUF8;

architecture concurrent of TRIBUF8 is

begin

  op <= ip when oe = '1' else (others => 'Z');

end concurrent;
library IEEE;
use IEEE.std_logic_1164.all;

entity TRIBUF is port (
  ip: in std_logic;
  oe: in std_logic;
  op: out std_logic
  );
end TRIBUF;

architecture concurrent of TRIBUF is

begin

  op <= ip when oe = '1' else 'Z';

end concurrent;
library IEEE;
use IEEE.std_logic_1164.all;

entity TRIBUF8 is port (
  ip: in std_logic_vector(7 downto 0);
  oe: in std_logic;
  op: out std_logic_vector(7 downto 0)
  );
end TRIBUF8;

architecture sequential of TRIBUF8 is

begin

  enable: process (ip,oe) begin
    if (oe = '1') then
      op <= ip;
    else
      op <= (others => 'Z');
    end if;
  end process;

end sequential;
library IEEE;
use IEEE.std_logic_1164.all;

entity TRIBUF is port (
  ip: in bit;
  oe: in bit;
  op: out bit
  );
end TRIBUF;

architecture sequential of TRIBUF is

begin

  enable: process (ip,oe) begin
    if (oe = '1') then
      op <= ip;
    else
      op <= null;
    end if;
  end process;

end sequential;
library IEEE;
use IEEE.std_logic_1164.all;

entity TRIBUF is port (
  ip: in std_logic;
  oe: in std_logic;
  op: out std_logic
  );
end TRIBUF;

architecture sequential of TRIBUF is

begin

  enable: process (ip,oe) begin
    if (oe = '1') then
      op <= ip;
    else
      op <= 'Z';
    end if;
  end process;

end sequential;
library IEEE;
use IEEE.std_logic_1164.all;

use work.primitive.all;

entity tribuffer is port (
  input: in std_logic;
  enable: in std_logic;
  output: out std_logic
  );
end tribuffer;

architecture structural of tribuffer is

begin

  u1: tribuf port map (ip => input,
                       oe => enable,
                       op => output
                      );

end structural;
library ieee;
use ieee.std_logic_1164.all;

use work.primitive.all;

entity oddParityGen is
  generic ( width : integer := 8 );
  port (ad: in std_logic_vector (width - 1 downto 0);
        oddParity : out std_logic ) ;
end oddParityGen; 

architecture scaleable of oddParityGen is

signal genXor: std_logic_vector(ad'range);

begin

  genXOR(0) <= '0';

  parTree: for i in 1 to ad'high generate
    x1: xor2 port map (i1 => genXor(i - 1),
                       i2 => ad(i - 1),
                       y  => genXor(i)
                      );
  end generate;

 oddParity <= genXor(ad'high) ;

end scaleable ;
library ieee;
use ieee.std_logic_1164.all;

entity oddParityLoop is
  generic ( width : integer := 8 );
  port (ad: in std_logic_vector (width - 1 downto 0);
        oddParity : out std_logic ) ;
end oddParityLoop ; 

architecture scaleable of oddParityLoop is
begin

  process (ad) 
    variable loopXor: std_logic;
  begin
    loopXor := '0';

    for i in 0 to width -1 loop
        loopXor := loopXor xor ad( i ) ;
    end loop ;

    oddParity <= loopXor ;

  end process;

end scaleable ;
library IEEE;
use IEEE.std_logic_1164.all;

library IEEE;
use IEEE.std_logic_1164.all;

entity OR2 is port (
    i1: in std_logic;
    i2: in std_logic;
    y: out std_logic
    );
end OR2;

architecture rtl of OR2 is

begin

  y <= '1' when i1 = '1' or i2 = '1' else '0';

end rtl;
library IEEE;
USE IEEE.std_logic_1164.all;


entity OR2 is port (
  I1, I2: in std_logic;
  Y: out std_logic
  );
end OR2;

architecture simple of OR2 is

begin

  Y <= I1 OR I2 after 10 ns;

end simple;
library IEEE;
USE IEEE.std_logic_1164.all;

package simPrimitives is

  component OR2
    generic (tPD: time := 1 ns);

    port (I1, I2: in std_logic;
      Y: out std_logic
      );
  end component;

end simPrimitives;


library IEEE;
USE IEEE.std_logic_1164.all;

entity OR2 is
  generic (tPD: time := 1 ns);

  port (I1, I2: in std_logic;
    Y: out std_logic
    );
end OR2;

architecture simple of OR2 is

begin

  Y <= I1 OR I2 after tPD;

end simple;
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity adder is port (
  a,b: in std_logic_vector(3 downto 0);
  sum: out std_logic_vector(3 downto 0);
  overflow: out std_logic
  );
end adder;

architecture concat of adder is

signal localSum: std_logic_vector(4 downto 0);

begin

  localSum <= std_logic_vector(unsigned('0' & a) + unsigned('0' & b));

  sum <= localSum(3 downto 0);
  overflow <= localSum(4);

end concat;
library IEEE;
use IEEE.std_logic_1164.all;

use work.primitive.all;

entity paramDFF is
  generic (size: integer := 8);
  port (
  data: in std_logic_vector(size - 1 downto 0);
  clock: in std_logic;
  reset: in std_logic;
  ff_enable: in std_logic;
  op_enable: in std_logic;
  qout: out std_logic_vector(size - 1 downto 0)
  );
end paramDFF;

architecture parameterize of paramDFF is

signal reg: std_logic_vector(size - 1 downto 0);

begin

  u1: pDFFE generic map (n => size)
            port map    (d => data,
                         clk =>clock,
                         rst => reset,
                         en => ff_enable,
                         q => reg
                        );
  u2: pTRIBUF generic map (n => size)
              port map    (ip => reg,
                           oe => op_enable,
                           op => qout
                          );

end paramterize;
library ieee;
use ieee.std_logic_1164.all;

use work.primitive.all;

entity oddParityGen is
  generic ( width : integer := 32 );
  port (ad: in std_logic_vector (width - 1 downto 0);
        oddParity : out std_logic ) ;
end oddParityGen; 

architecture scaleable of oddParityGen is

signal genXor: std_logic_vector(ad'range);

signal one: std_logic := '1';

begin

  parTree: for i in ad'range generate
    g0: if i = 0 generate
      x0: xor2 port map (i1 => one,
                         i2 => one,
                         y => genXor(0)
                        );
    end generate;

    g1: if i > 0 and i <= ad'high generate
      x1: xor2 port map (i1 => genXor(i - 1),
                         i2 => ad(i - 1),
                         y  => genXor(i)
                        );
    end generate;

  end generate;

 oddParity <= genXor(ad'high) ;

end scaleable ;
library ieee;
use ieee.std_logic_1164.all;

use work.primitive.all;

entity oddParityGen is
  generic ( width : integer := 32 ); -- (2 <= width <= 32) and a power of 2
  port (ad: in std_logic_vector (width - 1 downto 0);
        oddParity : out std_logic ) ;
end oddParityGen; 

architecture scaleable of oddParityGen is

signal stage0: std_logic_vector(31 downto 0);
signal stage1: std_logic_vector(15 downto 0);
signal stage2: std_logic_vector(7 downto 0);
signal stage3: std_logic_vector(3 downto 0);
signal stage4: std_logic_vector(1 downto 0);

begin

  g4: for i in stage4'range generate
    g41: if (ad'length > 2) generate
         x4: xor2 port map (stage3(i), stage3(i + stage4'length), stage4(i));
    end generate;
  end generate;

  g3: for i in stage3'range generate
    g31: if (ad'length > 4) generate
         x3: xor2 port map (stage2(i), stage2(i + stage3'length), stage3(i));
    end generate;
  end generate;

  g2: for i in stage2'range generate
    g21: if (ad'length > 8) generate
         x2: xor2 port map (stage1(i), stage1(i + stage2'length), stage2(i));
    end generate;
  end generate;

  g1: for i in stage1'range generate
    g11: if (ad'length > 16) generate
         x1: xor2 port map (stage0(i), stage0(i + stage1'length), stage1(i));
    end generate;
  end generate;


  s1: for i in ad'range generate
    s14: if (ad'length = 2) generate
         stage4(i) <= ad(i);
    end generate;

    s13: if (ad'length = 4) generate
         stage3(i) <= ad(i);
    end generate;

    s12: if (ad'length = 8) generate
         stage2(i) <= ad(i);
    end generate;

    s11: if (ad'length = 16) generate
         stage1(i) <= ad(i);
    end generate;

    s10: if (ad'length = 32) generate
         stage0(i) <= ad(i);
    end generate;

  end generate;


  genPar: xor2 port map (stage4(0), stage4(1), oddParity);

end scaleable ;
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity powerOfFour is port(
  clk      : in  std_logic; 
  inputVal : in  unsigned(3 downto 0);
  power    : out unsigned(15 downto 0)
  );
end powerOfFour;

architecture behavioral of powerOfFour is

   function Pow( N, Exp : integer )  return integer is
      Variable Result   : integer := 1;

   begin
      for i in 1 to Exp loop
         Result := Result * N;
      end loop;
      return( Result );
   end Pow;

signal inputValInt: integer range 0 to 15;
signal powerL: integer range 0 to 65535;
   
begin

   inputValInt <= to_integer(inputVal);
   power <= to_unsigned(powerL,16);

   process begin
     wait until Clk = '1';

       powerL <= Pow(inputValInt,4);
   
   end process;

end behavioral;
package PowerPkg is  
  component Power port(
      Clk                   : in  bit; 
      inputVal              : in  bit_vector(0 to 3);
      power                 : out bit_vector(0 to 15) );
   end component;
end          PowerPkg;

use work.bv_math.all;
use work.int_math.all;
use work.PowerPkg.all;

entity Power is port(
   Clk                   : in  bit; 
   inputVal              : in  bit_vector(0 to 3);
   power                 : out bit_vector(0 to 15) );
end    Power;




architecture funky of Power is
   
   function Pow( N, Exp : integer )  return integer is
      Variable Result   : integer := 1;
      Variable i        : integer := 0;
   begin
      while( i < Exp ) loop
         Result := Result * N;
         i      := i      + 1;
      end loop;
      return( Result );
   end Pow;
   
   
   function RollVal(  CntlVal : integer )  return integer is
   begin
      return( Pow( 2, CntlVal ) + 2 );
   end RollVal;
   
   
begin
   process 
   begin
      wait until Clk = '1';
         
        power <= i2bv(Rollval(bv2I(inputVal)),16);
   
   end process;
end funky;
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity priority_encoder is port
  (interrupts : in  std_logic_vector(7 downto 0);
   priority   : in  std_logic_vector(2 downto 0);
   result     : out std_logic_vector(2 downto 0)
  );
end priority_encoder;

architecture behave of priority_encoder is
begin

  process (interrupts)
     variable selectIn  : integer;
     variable LoopCount : integer;
  begin

    LoopCount   := 1;
    selectIn := to_integer(to_unsigned(priority));
      
      while (LoopCount <= 7) and (interrupts(selectIn) /= '0') loop
		
        if (selectIn = 0) then
          selectIn := 7;
        else
          selectIn := selectIn - 1;
        end if;
			
        LoopCount := LoopCount + 1;
  
      end loop;
		
    result <= std_logic_vector(to_unsigned(selectIn,3));

  end process;
  
end behave;
library IEEE;
use IEEE.std_logic_1164.all;

package primitive is
  component DFFE port (
    d: in std_logic;
    q: out std_logic;
    en: in std_logic;
    clk: in std_logic
    );
  end component;

  component DFFE_SR port (
    d: in std_logic;
    en: in std_logic;
    clk: in std_logic;
    rst: in std_logic;
    prst: in std_logic;
    q: out std_logic
    );
  end component;

  component DLATCHH port (
    d: in std_logic;
    en: in std_logic;
    q: out std_logic
    );
  end component;

  component AND2 port (
    i1: in std_logic;
    i2: in std_logic;
    y: out std_logic
    );
  end component;

  component OR2 port (
    i1: in std_logic;
    i2: in std_logic;
    y: out std_logic
    );
  end component;

  component INVERTER port (
    i: in std_logic;
    o: out std_logic
    );
  end component;

  component TRIBUF port (
    ip: in std_logic;
    oe: in std_logic;
    op: out std_logic
    );
  end component;

  component BIDIR port (
    ip: in std_logic;
    oe: in std_logic;
    op_fb: out std_logic;
    op: inout std_logic
    );
  end component;

end package;

library IEEE;
use IEEE.std_logic_1164.all;

entity DFFE is port (
    d: in std_logic;
    q: out std_logic;
    en: in std_logic;
    clk: in std_logic
    );
end DFFE;

architecture rtl of DFFE is

begin

  process begin
    wait until clk = '1';
      if (en = '1') then
        q <= d;
      end if;
  end process;

end rtl;

library IEEE;
use IEEE.std_logic_1164.all;

entity DFFE_SR is port (
    d: in std_logic;
    en: in std_logic;
    clk: in std_logic;
    rst: in std_logic;
    prst: in std_logic;
    q: out std_logic
    );
end DFFE_SR;

architecture rtl of DFFE_SR is

begin

  process (clk, rst, prst) begin
    if (rst = '1') then
      q <= '0';
    elsif (prst = '1') then
      q <= '1';
    elsif (clk'event and clk = '1') then
      if (en = '1') then
        q <= d;
      end if;
    end if;
  end process;

end rtl;

library IEEE;
use IEEE.std_logic_1164.all;

entity DLATCHH is port (
    d: in std_logic;
    en: in std_logic;
    q: out std_logic
    );
end DLATCHH;

architecture rtl of DLATCHH is

begin

  process (en) begin
    if (en = '1') then
      q <= d;
    end if;
  end process;

end rtl;


library IEEE;
use IEEE.std_logic_1164.all;

entity AND2 is port (
    i1: in std_logic;
    i2: in std_logic;
    y: out std_logic
    );
end AND2;

architecture rtl of AND2 is

begin

  y <= '1' when i1 = '1' and i2 = '1' else '0';

end rtl;


library IEEE;
use IEEE.std_logic_1164.all;

entity OR2 is port (
    i1: in std_logic;
    i2: in std_logic;
    y: out std_logic
    );
end OR2;

architecture rtl of OR2 is

begin

  y <= '1' when i1 = '1' or i2 = '1' else '0';

end rtl;



library IEEE;
use IEEE.std_logic_1164.all;

entity INVERTER is port (
    i: in std_logic;
    o: out std_logic
    );
end INVERTER;

architecture rtl of INVERTER is

begin

  o <= not i;

end rtl;


library IEEE;
use IEEE.std_logic_1164.all;

entity TRIBUF is port (
  ip: in std_logic;
  oe: in std_logic;
  op: out std_logic
  );
end TRIBUF;

architecture rtl of TRIBUF is

begin

  op <= ip when oe = '1' else 'Z';

end rtl;


library IEEE;
use IEEE.std_logic_1164.all;

entity BIDIR is port (
  ip: in std_logic;
  oe: in std_logic;
  op_fb: out std_logic;
  op: inout std_logic
  );
end BIDIR;

architecture rtl of BIDIR is

begin

  op <= ip when oe = '1' else 'Z';
  op_fb <= op;

end rtl;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity progPulse is port (
  clk, reset: in std_logic;
  loadLength,loadDelay: in std_logic;
  data: in std_logic_vector(7 downto 0);
  pulse: out std_logic
  );
end progPulse;

architecture rtl of progPulse is

signal downCnt, downCntData: unsigned(7 downto 0);
signal downCntLd, downCntEn: std_logic;
signal delayCntVal, pulseCntVal: unsigned(7 downto 0);
signal startPulse, endPulse: std_logic;

subtype fsmType is std_logic_vector(1 downto 0);
constant loadDelayCnt  : fsmType := "00";
constant waitDelayEnd  : fsmType := "10";
constant loadLengthCnt : fsmType := "11";
constant waitLengthEnd : fsmType := "01";

signal currState, nextState: fsmType;

begin

  delayreg: process (clk, reset) begin
    if reset = '1' then
      delayCntVal <= "11111111";
    elsif clk'event and clk = '1' then
      if loadDelay = '1' then
        delayCntVal <= to_unsigned(data);
      end if;
    end if;
  end process;

  lengthReg: process (clk, reset) begin
    if reset = '1' then
      pulseCntVal <= "11111111";
    elsif clk'event and clk = '1' then
      if loadDelay = '1' then
        pulseCntVal <= to_unsigned(data);
      end if;
    end if;
  end process;

  nextStProc: process (currState, downCnt, loadDelay, loadLength) begin
    case currState is
      when loadDelayCnt =>
        nextState <= waitDelayEnd;

      when waitDelayEnd =>
        if (loadDelay = '1' or loadLength = '1') then
          nextState <= loadDelayCnt;
        elsif (downCnt = 0) then
          nextState <= loadLengthCnt;
        else
          nextState <= waitDelayEnd;
        end if;

      when loadLengthCnt =>
        if (loadDelay = '1' or loadLength = '1') then
          nextState <= loadDelayCnt;
        else
          nextState <= waitLengthEnd;
        end if;

      when waitLengthEnd =>
        if (loadDelay = '1' or loadLength = '1') then
          nextState <= loadDelayCnt;
        elsif (downCnt = 0) then
          nextState <= loadDelayCnt;
        else
          nextState <= waitDelayEnd;
        end if;

      when others =>
        null;

   end case;
  end process nextStProc;

  currStProc: process (clk, reset) begin
    if (reset = '1') then
      currState <= loadDelayCnt;
    elsif (clk'event and clk = '1') then
      currState <= nextState;
    end if;
  end process currStProc;

  outConProc: process (currState, delayCntVal, pulseCntVal) begin
    case currState is
      when loadDelayCnt =>
        downCntEn <= '0';
        downCntLd <= '1';
        downCntData <= delayCntVal;

      when waitDelayEnd =>
        downCntEn <= '1';
        downCntLd <= '0';
        downCntData <= delayCntVal;

      when loadLengthCnt =>
        downCntEn <= '0';
        downCntLd <= '1';
        downCntData <= pulseCntVal;

      when waitLengthEnd =>
        downCntEn <= '1';
        downCntLd <= '0';
        downCntData <= pulseCntVal;

      when others =>
        downCntEn <= '0';
        downCntLd <= '1';
        downCntData <= pulseCntVal;

    end case;
  end process outConProc;

  downCntr: process (clk,reset) begin
    if (reset = '1') then
      downCnt <= "00000000";
    elsif (clk'event and clk = '1') then
      if (downCntLd = '1') then
        downCnt <= downCntData;
      elsif (downCntEn = '1') then
        downCnt <= downCnt - 1;
      else
        downCnt <= downCnt;
      end if;
    end if;
  end process;

  -- Assign pulse output
  pulse <= currState(0);
        

end rtl;
library ieee;
use ieee.std_logic_1164.all;

entity pulseErr is port
  (a: in std_logic;
   b: out std_logic
  );
end pulseErr;

architecture behavior of pulseErr is

signal c: std_logic;

begin

  pulse: process (a,c) begin
    b <= c XOR a;

    c <= a;
  end process;

end behavior;
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity progPulse is port (
  clk, reset: in std_logic;
  loadLength,loadDelay: in std_logic;
  data: in std_logic_vector(7 downto 0);
  pulse: out std_logic
  );
end progPulse;

architecture rtl of progPulse is

signal downCnt, downCntData: unsigned(7 downto 0);
signal downCntLd, downCntEn: std_logic;
signal delayCntVal, pulseCntVal: unsigned(7 downto 0);
signal startPulse, endPulse: std_logic;

type progPulseFsmType is (loadDelayCnt, waitDelayEnd, loadLengthCnt, waitLengthEnd);
signal currState, nextState: progPulseFsmType;

begin

  delayreg: process (clk, reset) begin
    if reset = '1' then
      delayCntVal <= "11111111";
    elsif clk'event and clk = '1' then
      if loadDelay = '1' then
        delayCntVal <= to_unsigned(data);
      end if;
    end if;
  end process;

  lengthReg: process (clk, reset) begin
    if reset = '1' then
      pulseCntVal <= "11111111";
    elsif clk'event and clk = '1' then
      if loadDelay = '1' then
        pulseCntVal <= to_unsigned(data);
      end if;
    end if;
  end process;

  nextStProc: process (currState, downCnt, loadDelay, loadLength) begin
    case currState is
      when loadDelayCnt =>
        nextState <= waitDelayEnd;

      when waitDelayEnd =>
        if (loadDelay = '1' or loadLength = '1') then
          nextState <= loadDelayCnt;
        elsif (downCnt = 0) then
          nextState <= loadLengthCnt;
        else
          nextState <= waitDelayEnd;
        end if;

      when loadLengthCnt =>
        if (loadDelay = '1' or loadLength = '1') then
          nextState <= loadDelayCnt;
        else
          nextState <= waitLengthEnd;
        end if;

      when waitLengthEnd =>
        if (loadDelay = '1' or loadLength = '1') then
          nextState <= loadDelayCnt;
        elsif (downCnt = 0) then
          nextState <= loadDelayCnt;
        else
          nextState <= waitDelayEnd;
        end if;

      when others =>
        null;

   end case;
  end process nextStProc;

  currStProc: process (clk, reset) begin
    if (reset = '1') then
      currState <= loadDelayCnt;
    elsif (clk'event and clk = '1') then
      currState <= nextState;
    end if;
  end process currStProc;

  outConProc: process (currState, delayCntVal, pulseCntVal) begin
    case currState is
      when loadDelayCnt =>
        downCntEn <= '0';
        downCntLd <= '1';
        downCntData <= delayCntVal;
        pulse <= '0';

      when waitDelayEnd =>
        downCntEn <= '1';
        downCntLd <= '0';
        downCntData <= delayCntVal;
        pulse <= '0';

      when loadLengthCnt =>
        downCntEn <= '0';
        downCntLd <= '1';
        downCntData <= pulseCntVal;
        pulse <= '1';

      when waitLengthEnd =>
        downCntEn <= '1';
        downCntLd <= '0';
        downCntData <= pulseCntVal;
        pulse <= '1';

      when others =>
        downCntEn <= '0';
        downCntLd <= '1';
        downCntData <= pulseCntVal;
        pulse <= '0';

    end case;
  end process outConProc;

  downCntr: process (clk,reset) begin
    if (reset = '1') then
      downCnt <= "00000000";
    elsif (clk'event and clk = '1') then
      if (downCntLd = '1') then
        downCnt <= downCntData;
      elsif (downCntEn = '1') then
        downCnt <= downCnt - 1;
      else
        downCnt <= downCnt;
      end if;
    end if;
  end process;
        

end rtl;
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity progPulseFsm is port (
  downCnt: in std_logic_vector(7 downto 0);
  delayCntVal: in std_logic_vector(7 downto 0);
  lengthCntVal: in std_logic_vector(7 downto 0);
  loadLength: in std_logic;
  loadDelay: in std_logic;
  clk: in std_logic;
  reset: in std_logic;

  downCntEn: out std_logic;
  downCntLd: out std_logic;
  downCntData: out std_logic_vector(7 downto 0);

  pulse: out std_logic
  );
end progPulseFsm;

architecture fsm of progPulseFsm is

type progPulseFsmType is (loadDelayCnt, waitDelayEnd, loadLengthCnt, waitLengthEnd);
type stateVec is array (3 downto 0) of std_logic;
type stateBits is array (progPulseFsmType) of stateVec;

signal loadVal: std_logic;

constant stateTable: stateBits := (
  loadDelayCnt =>  "0010",
  waitDelayEnd =>  "0100",
  loadLengthCnt => "0011",
  waitLengthEnd => "1101" );
--                  ^^^^
--                  ||||__ loadVal   
--                  |||___ downCntLd
--                  ||____ downCntEn
--                  |_____ pulse

signal currState, nextState: progPulseFsmType;

begin

  nextStProc: process (currState, downCnt, loadDelay, loadLength) begin
    case currState is
      when loadDelayCnt =>
        nextState <= waitDelayEnd;

      when waitDelayEnd =>
        if (loadDelay = '1' or loadLength = '1') then
          nextState <= loadDelayCnt;
        elsif (to_unsigned(downCnt) = 0) then
          nextState <= loadLengthCnt;
        else
          nextState <= waitDelayEnd;
        end if;

      when loadLengthCnt =>
        if (loadDelay = '1' or loadLength = '1') then
          nextState <= loadDelayCnt;
        else
          nextState <= waitLengthEnd;
        end if;

      when waitLengthEnd =>
        if (loadDelay = '1' or loadLength = '1') then
          nextState <= loadDelayCnt;
        elsif (to_unsigned(downCnt) = 0) then
          nextState <= loadDelayCnt;
        else
          nextState <= waitDelayEnd;
        end if;

      when others =>
        null;

   end case;

  end process nextStProc;

  currStProc: process (clk, reset) begin
    if (reset = '1') then
      currState <= loadDelayCnt;
    elsif (clk'event and clk = '1') then
      currState <= nextState;
    end if;
  end process currStProc;

  pulse     <= stateTable(currState)(3);
  downCntEn <= stateTable(currState)(2);
  downCntLd <= stateTable(currState)(1);
  loadVal   <= stateTable(currState)(0);
  
  downCntData <= delayCntVal when loadVal = '0' else lengthCntVal;

end fsm;
-- Incorporates Errata 6.1

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity progPulseFsm is port (
  downCnt: in std_logic_vector(7 downto 0);
  delayCntVal: in std_logic_vector(7 downto 0);
  lengthCntVal: in std_logic_vector(7 downto 0);
  loadLength: in std_logic;
  loadDelay: in std_logic;
  clk: in std_logic;
  reset: in std_logic;

  downCntEn: out std_logic;
  downCntLd: out std_logic;
  downtCntData: out std_logic_vector(7 downto 0);

  pulse: out std_logic
  );
end progPulseFsm;

architecture fsm of progPulseFsm is

type progPulseFsmType is (loadDelayCnt, waitDelayEnd, loadLengthCnt, waitLengthEnd);
signal currState, nextState: progPulseFsmType;
signal downCntL: unsigned (7 downto 0);

begin

  downCntL <= to_unsigned(downCnt); -- convert downCnt to unsigned

  nextStProc: process (currState, downCntL, loadDelay, loadLength) begin
    case currState is
      when loadDelayCnt =>
        nextState <= waitDelayEnd;

      when waitDelayEnd =>
        if (loadDelay = '1' or loadLength = '1') then
          nextState <= loadDelayCnt;
        elsif (downCntL = 0) then
          nextState <= loadLengthCnt;
        else
          nextState <= waitDelayEnd;
        end if;

      when loadLengthCnt =>
        if (loadDelay = '1' or loadLength = '1') then
          nextState <= loadDelayCnt;
        else
          nextState <= waitLengthEnd;
        end if;

      when waitLengthEnd =>
        if (loadDelay = '1' or loadLength = '1') then
          nextState <= loadDelayCnt;
        elsif (downCntL = 0) then
          nextState <= loadDelayCnt;
        else
          nextState <= waitDelayEnd;
        end if;

      when others =>
        null;

   end case;

  end process nextStProc;

  currStProc: process (clk, reset) begin
    if (reset = '1') then
      currState <= loadDelayCnt;
    elsif (clk'event and clk = '1') then
      currState <= nextState;
    end if;
  end process currStProc;

  outConProc: process (currState, delayCntVal, lengthCntVal) begin
    case currState is
      when loadDelayCnt =>
        downCntEn <= '0';
        downCntLd <= '1';
        downtCntData <= delayCntVal;
        pulse <= '0';

      when waitDelayEnd =>
        downCntEn <= '1';
        downCntLd <= '0';
        downtCntData <= delayCntVal;
        pulse <= '0';

      when loadLengthCnt =>
        downCntEn <= '0';
        downCntLd <= '1';
        downtCntData <= lengthCntVal;
        pulse <= '1';

      when waitLengthEnd =>
        downCntEn <= '1';
        downCntLd <= '0';
        downtCntData <= lengthCntVal;
        pulse <= '1';

      when others =>
        downCntEn <= '0';
        downCntLd <= '1';
        downtCntData <= delayCntVal;
        pulse <= '0';

    end case;
  end process outConProc;

end fsm;
-- Incorporates errata 5.4

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

use work.specialFunctions.all;

entity powerOfFour is port(
  clk      : in  std_logic; 
  inputVal : in  std_logic_vector(3 downto 0);
  power    : out std_logic_vector(15 downto 0)
  );
end powerOfFour;

architecture behavioral of powerOfFour is

begin

   process begin
     wait until Clk = '1';

       power <= std_logic_vector(to_unsigned(Pow(to_integer(unsigned(inputVal)),4),16));
   
   end process;

end behavioral;
-- Incorporate errata 5.4

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity powerOfFour is port(
  clk      : in  std_logic; 
  inputVal : in  std_logic_vector(3 downto 0);
  power    : out std_logic_vector(15 downto 0)
  );
end powerOfFour;

architecture behavioral of powerOfFour is

   function Pow( N, Exp : integer )  return integer is
      Variable Result   : integer := 1;

   begin
      for i in 1 to Exp loop
         Result := Result * N;
      end loop;
      return( Result );
   end Pow;

begin

   process begin
     wait until Clk = '1';

       power <= std_logic_vector(to_unsigned(Pow(to_integer(to_unsigned(inputVal)),4),16));
   
   end process;

end behavioral;
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;

entity powerOfFour is port(
  clk      : in  std_logic; 
  inputVal : in  std_logic_vector(3 downto 0);
  power    : out std_logic_vector(15 downto 0)
  );
end powerOfFour;

architecture behavioral of powerOfFour is

   function Pow( N, Exp : integer )  return integer is
      Variable Result   : integer := 1;

   begin
      for i in 1 to Exp loop
         Result := Result * N;
      end loop;
      return( Result );
   end Pow;

begin

   process begin
     wait until Clk = '1';

       power <= conv_std_logic_vector(Pow(conv_integer(inputVal),4),16);
   
   end process;

end behavioral;
library IEEE;
use IEEE.std_logic_1164.all;

entity regFile is port (
  clk, rst: in std_logic;
  data: in std_logic_vector(31 downto 0);
  regSel: in std_logic_vector(1 downto 0);
  wrEnable: in std_logic;
  regOut: out std_logic_vector(31 downto 0)
  );
end regFile;

architecture behavioral of regFile is

subtype reg is std_logic_vector(31 downto 0);
type regArray is array (integer range <>) of reg;

signal registerFile: regArray(0 to 3);

begin

  regProc: process (clk, rst) 
  variable i: integer;

  begin
    i := 0;

    if rst = '1' then
      while i <= registerFile'high loop
        registerFile(i) <= (others => '0');
        i := i + 1;
      end loop;

    elsif clk'event and clk = '1' then
      if (wrEnable = '1') then
        case regSel is
          when "00" =>
            registerFile(0) <= data;
          when "01" =>
            registerFile(1) <= data;
          when "10" =>
            registerFile(2) <= data;
          when "11" =>
            registerFile(3) <= data;
          when others =>
            null;
        end case;
      end if;
    end if;
  end process;

  outputs: process(regSel, registerFile) begin
    case regSel is
      when "00" =>
        regOut <= registerFile(0);
      when "01" =>
        regOut <= registerFile(1);
      when "10" =>
        regOut <= registerFile(2);
      when "11" =>
        regOut <= registerFile(3);
      when others =>
        null;
    end case;
  end process;

end behavioral;
library IEEE;
use IEEE.std_logic_1164.all;

entity DFF is port (
    d1,d2: in std_logic;
    q1,q2: out std_logic;
    clk: in std_logic;
    rst : in std_logic
    );
end DFF;

architecture rtl of DFF is

begin

  resetLatch: process (clk, rst) begin
    if rst = '1' then
      q1 <= '0';
    elsif clk'event and clk = '1' then
      q1 <= d1;
      q2 <= d2;
    end if;
  end process;

end rtl;
library ieee;
use ieee.std_logic_1164.all;

entity resFcnDemo is port (
  a, b: in std_logic;
  oeA,oeB: in std_logic;
  result: out std_logic
  );
end resFcnDemo;

architecture multiDriver of resFcnDemo is

begin

  result <= a when oeA = '1' else 'Z';
  result <= b when oeB = '1' else 'Z';

end multiDriver;
library IEEE;
use IEEE.std_logic_1164.all;

use work.primitive.all;

entity scaleDFF is port (
  data: in std_logic_vector(7 downto 0);
  clock: in std_logic;
  enable: in std_logic;
  qout: out std_logic_vector(7 downto 0)
  );
end scaleDFF;

architecture scalable of scaleDFF is

begin

  u1: sDFFE port map (d => data,
                      clk =>clock,
                      en => enable,
                      q => qout
                     );

end scalable;
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity sevenSegment is port (
  bcdInputs: in std_logic_vector (3 downto 0);
  a_n, b_n, c_n, d_n,
  e_n, f_n, g_n: out std_logic
  );
end sevenSegment;

architecture behavioral of sevenSegment is

signal la_n, lb_n, lc_n, ld_n, le_n, lf_n, lg_n: std_logic;
signal oe: std_logic;

begin

  bcd2sevSeg: process (bcdInputs) begin

  -- Assign default to "off"
    la_n <= '1';    lb_n <= '1';
    lc_n <= '1';    ld_n <= '1';
    le_n <= '1';    lf_n <= '1';
    lg_n <= '1';

    case bcdInputs is
      when "0000" => la_n <= '0';        lb_n <= '0';
                     lc_n <= '0';        ld_n <= '0';
                     le_n <= '0';        lf_n <= '0';

      when "0001" => lb_n <= '0';        lc_n <= '0';

      when "0010" => la_n <= '0';        lb_n <= '0';
                     ld_n <= '0';        le_n <= '0';
                     lg_n <= '0';

      when "0011" => la_n <= '0';        lb_n <= '0';
                     lc_n <= '0';        ld_n <= '0';
                     lg_n <= '0';

      when "0100" => lb_n <= '0';        lc_n <= '0';
                     lf_n <= '0';        lg_n <= '0';

      when "0101" => la_n <= '0';        lc_n <= '0';
                     ld_n <= '0';        lf_n <= '0';
                     lg_n <= '0';

      when "0110" => la_n <= '0';        lc_n <= '0';
                     ld_n <= '0';        le_n <= '0';
                     lf_n <= '0';        lg_n <= '0';

      when "0111" => la_n <= '0';        lb_n <= '0';
                     lc_n <= '0';

      when "1000" => la_n <= '0';        lb_n <= '0';
                     lc_n <= '0';        ld_n <= '0';
                     le_n <= '0';        lf_n <= '0';
                     lg_n <= '0';

      when "1001" => la_n <= '0';        lb_n <= '0';
                     lc_n <= '0';        ld_n <= '0';
                     lf_n <= '0';        lg_n <= '0';

-- All other inputs possibilities are "don't care"

      when others => la_n <= 'X';        lb_n <= 'X';
                     lc_n <= 'X';        ld_n <= 'X';
                     le_n <= 'X';        lf_n <= 'X';
                     lg_n <= 'X';

    end case;

  end process bcd2sevSeg;

  -- Disable outputs for all invalid input values

  oe <= '1' when (bcdInputs < 10) else '0';

  a_n <= la_n when oe = '1' else 'Z';
  b_n <= lb_n when oe = '1' else 'Z';
  c_n <= lc_n when oe = '1' else 'Z';
  d_n <= ld_n when oe = '1' else 'Z';
  e_n <= le_n when oe = '1' else 'Z';
  f_n <= lf_n when oe = '1' else 'Z';
  g_n <= lg_n when oe = '1' else 'Z';


end behavioral;
library ieee;
use ieee.std_logic_1164.all;

use std.textio.all;

entity sevenSegmentTB is
end sevenSegmentTB;

architecture testbench of sevenSegmentTB is

component sevenSegment port (
  bcdInputs: in std_logic_vector (3 downto 0);
  a_n, b_n, c_n, d_n,
  e_n, f_n, g_n: out std_logic
  );
end component;

type vector is record
  bcdStimulus: std_logic_vector(3 downto 0);
  sevSegOut: std_logic_vector(6 downto 0);
end record;

constant NumVectors: integer:= 17;
constant PropDelay: time := 40 ns;
constant SimLoopDelay: time := 10 ns;

type vectorArray is array (0 to NumVectors - 1) of vector;
constant vectorTable: vectorArray := (
  (bcdStimulus => "0000", sevSegOut => "0000001"),
  (bcdStimulus => "0001", sevSegOut => "1001111"),
  (bcdStimulus => "0010", sevSegOut => "0010010"),
  (bcdStimulus => "0011", sevSegOut => "0000110"),
  (bcdStimulus => "0100", sevSegOut => "1001100"),
  (bcdStimulus => "0101", sevSegOut => "0100100"),
  (bcdStimulus => "0110", sevSegOut => "0100000"),
  (bcdStimulus => "0111", sevSegOut => "0001111"),
  (bcdStimulus => "1000", sevSegOut => "0000000"),
  (bcdStimulus => "1001", sevSegOut => "0000100"),
  (bcdStimulus => "1010", sevSegOut => "ZZZZZZZ"),
  (bcdStimulus => "1011", sevSegOut => "ZZZZZZZ"),
  (bcdStimulus => "1100", sevSegOut => "ZZZZZZZ"),
  (bcdStimulus => "1101", sevSegOut => "ZZZZZZZ"),
  (bcdStimulus => "1110", sevSegOut => "ZZZZZZZ"),
  (bcdStimulus => "1111", sevSegOut => "ZZZZZZZ"),
  (bcdStimulus => "0000", sevSegOut => "0110110") -- this vector fails
  );

for all : sevenSegment use entity work.sevenSegment(behavioral);

signal StimInputs: std_logic_vector(3 downto 0);
signal CaptureOutputs: std_logic_vector(6 downto 0);

begin

  u1: sevenSegment port map (bcdInputs => StimInputs,
                             a_n => CaptureOutputs(6),
                             b_n => CaptureOutputs(5),
                             c_n => CaptureOutputs(4),
                             d_n => CaptureOutputs(3),
                             e_n => CaptureOutputs(2),
                             f_n => CaptureOutputs(1),
                             g_n => CaptureOutputs(0));

  LoopStim: process
    variable FoundError: boolean := false;
    variable TempVector: vector;
    variable ErrorMsgLine: line;
  begin

    for i in vectorTable'range loop
      TempVector := vectorTable(i);

      StimInputs <= TempVector.bcdStimulus;

      wait for PropDelay;

      if CaptureOutputs /= TempVector.sevSegOut then
          write (ErrorMsgLine, string'("Vector failed at "));
          write (ErrorMsgLine, now);
          writeline (output, ErrorMsgLine);
          FoundError := true;
      end if;

      wait for SimLoopDelay;

    end loop;

  assert FoundError
    report "No errors. All vectors passed."
    severity note;

  wait;

  end process;

end testbench;
library ieee;
use ieee.std_logic_1164.all;

entity sevenSegment is port (
  bcdInputs: in std_logic_vector (3 downto 0);
  a_n, b_n, c_n, d_n,
  e_n, f_n, g_n: out std_logic
  );
end sevenSegment;

architecture behavioral of sevenSegment is

begin

  bcd2sevSeg: process (bcdInputs) begin

  -- Assign default to "off"
    a_n <= '1';    b_n <= '1';
    c_n <= '1';    d_n <= '1';
    e_n <= '1';    f_n <= '1';
    g_n <= '1';

    case bcdInputs is
      when "0000" =>
        a_n <= '0';        b_n <= '0';
        c_n <= '0';        d_n <= '0';
        e_n <= '0';        f_n <= '0';

      when "0001" =>
        b_n <= '0';        c_n <= '0';

      when "0010" =>
        a_n <= '0';        b_n <= '0';
        d_n <= '0';        e_n <= '0';
        g_n <= '0';

      when "0011" =>
        a_n <= '0';        b_n <= '0';
        c_n <= '0';        d_n <= '0';
        g_n <= '0';

      when "0100" =>
        b_n <= '0';        c_n <= '0';
        f_n <= '0';        g_n <= '0';

      when "0101" =>
        a_n <= '0';        c_n <= '0';
        d_n <= '0';        f_n <= '0';
        g_n <= '0';

      when "0110" =>
        a_n <= '0';        c_n <= '0';
        d_n <= '0';        e_n <= '0';
        f_n <= '0';        g_n <= '0';

      when "0111" =>
        a_n <= '0';        b_n <= '0';
        c_n <= '0';

      when "1000" =>
        a_n <= '0';        b_n <= '0';
        c_n <= '0';        d_n <= '0';
        e_n <= '0';        f_n <= '0';
        g_n <= '0';

      when "1001" =>
        a_n <= '0';        b_n <= '0';
        c_n <= '0';        d_n <= '0';
        f_n <= '0';        g_n <= '0';

      when others =>
        null;

    end case;

  end process bcd2sevSeg;

end behavioral;
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;

entity ForceShare is port (
  a,b,c,d,e,f: in std_logic_vector (7 downto 0);
  result: out std_logic_vector(7 downto 0)
  );
end ForceShare;

architecture behaviour of ForceShare is

begin

  sum: process (a,c,b,d,e,f)
  variable tempSum: std_logic_vector(7 downto 0);
  begin

    tempSum := a + b; -- temporary node for sum

    if (tempSum = "10011010") then
      result <= c;
    elsif (tempSum = "01011001") then
          result <= d;
    elsif (tempSum = "10111011") then
      result <= e;
    else
      result <= f;
    end if;
  end process;

end behaviour;
library IEEE;
use IEEE.std_logic_1164.all;

entity shifter is port (
  clk, rst: in std_logic;
  shiftEn,shiftIn: std_logic;
  q: out std_logic_vector (15 downto 0)
  );
end shifter;


architecture behav of shifter is

signal qLocal: std_logic_vector(15 downto 0);

begin

  shift: process (clk, rst) begin
    if (rst = '1') then
      qLocal <= (others => '0');
    elsif (clk'event and clk = '1') then
      if (shiftEn = '1') then
        qLocal <= qLocal(14 downto 0) & shiftIn;
      else
        qLocal <= qLocal;
      end if;
    end if;

    q <= qLocal;
  end process;

end behav;
library ieee;
use ieee.std_logic_1164.all;

entity lastAssignment is port
  (a, b: in std_logic;
   selA, selb: in std_logic;
   result: out std_logic
  );
end lastAssignment;

architecture behavioral of lastAssignment is

begin

  demo: process (a,b,selA,selB) begin
    if (selA = '1') then
      result <= a;
    else
      result <= '0';
    end if;

    if (selB = '1') then
      result <= b;
    else
      result <= '0';
    end if;
  end process demo;

end behavioral;
library ieee;
use ieee.std_logic_1164.all;

entity signalDemo is port (
  a: in std_logic;
  b: out std_logic
  );
end signalDemo;

architecture basic of signalDemo is

signal c: std_logic;

begin

  demo: process (a) begin

    c <= a;

    if c = '0' then
      b <= a;
    else
      b <= '0';
    end if;

  end process;

end basic;
library ieee;
use ieee.std_logic_1164.all;

entity signalDemo is port (
  a: in std_logic;
  b: out std_logic
  );
end signalDemo;

architecture basic of signalDemo is

signal c: std_logic;

begin

  demo: process (a) begin

    c <= a;

    if c = '1' then
      b <= a;
    else
      b <= '0';
    end if;

  end process;

end basic;
library IEEE;
USE IEEE.std_logic_1164.all;

package simPrimitives is

  component OR2
    generic (tPD: time := 1 ns);

    port (I1, I2: in std_logic;
          Y: out std_logic
         );
  end component;

  component SimDFF
  generic(tCQ: time := 1 ns;
          tS : time := 1 ns;
          tH : time := 1 ns
         );
  port (D, Clk: in std_logic;
        Q: out std_logic
       );
  end component;


end simPrimitives;


library IEEE;
USE IEEE.std_logic_1164.all;

entity OR2 is
  generic (tPD: time := 1 ns);

  port (I1, I2: in std_logic;
    Y: out std_logic
    );
end OR2;

architecture simple of OR2 is

begin

  Y <= I1 OR I2 after tPD;

end simple;



library IEEE;
use IEEE.std_logic_1164.all;

entity SimDFF is
  generic(tCQ: time := 1 ns;
          tS : time := 1 ns;
          tH : time := 1 ns
         );
  port (D, Clk: in std_logic;
  Q: out std_logic
  );
end SimDff;

architecture SimModel of SimDFF is

begin

  reg: process (Clk, D) begin

    -- Assign output tCQ after rising clock edge
    if (Clk'event and Clk = '1') then
      Q <= D after tCQ;
    end if;

    -- Check setup time
    if (Clk'event and Clk = '1') then
      assert (D'last_event >= tS)
        report "Setup time violation"
        severity Warning;
    end if;

    -- Check hold time
    if (D'event and Clk'stable and Clk = '1') then
      assert (D'last_event - Clk'last_event > tH)
        report "Hold Time Violation"
        severity Warning;
    end if;

  end process;

end simModel;

library IEEE;
use IEEE.std_logic_1164.all;

entity SRFF is port (
    s,r: in std_logic;
    clk: in std_logic;
    q: out std_logic
    );
end SRFF;

architecture rtl of SRFF is

begin

  process begin
    wait until rising_edge(clk);
      if s = '0' and r = '1' then
	q <= '0';
      elsif s = '1' and r = '0' then
	q <= '1';
      end if;
  end process;

end rtl;
library IEEE;
use IEEE.std_logic_1164.all;

entity SRFF is port (
    s,r: in std_logic;
    clk: in std_logic;
    q: out std_logic
    );
end SRFF;

architecture rtl of SRFF is

begin

  process begin
    wait until clk = '1';
      if s = '0' and r = '1' then
	q <= '0';
      elsif s = '1' and r = '0' then
	q <= '1';
      end if;
  end process;

end rtl;
library IEEE;
use IEEE.std_logic_1164.all;

package scaleable is
  component scaleUpCnt port (
    clk: in std_logic;
    reset: in std_logic;
    cnt: in std_logic_vector
    );
  end component;
end scaleable;

library IEEE;
use IEEE.std_logic_1164.all;

use work.primitive.all;

entity scaleUpCnt is port (
  clk: in std_logic;
  reset: in std_logic;
  cnt: out std_logic_vector
  );
end scaleUpCnt;

architecture scaleable of scaleUpCnt is

signal one: std_logic := '1';
signal cntL: std_logic_vector(cnt'range);
signal andTerm: std_logic_vector(cnt'range);

begin

-- Special case is the least significant bit

  lsb: tff port map (t => one,
                     reset => reset,
                     clk => clk,
                     q => cntL(cntL'low)
                    );

  andTerm(0) <= cntL(cntL'low);


-- General case for all other bits

  genAnd: for i in 1 to cntL'high generate
    andTerm(i) <= andTerm(i - 1) and cntL(i);
  end generate;

  genTFF: for i in 1 to cntL'high generate
    t1: tff port map (t => andTerm(i),
                      clk => clk,
                      reset => reset,
                      q => cntl(i)
                     );
  end generate;

  cnt <= CntL;

end scaleable;
library IEEE;
use IEEE.std_logic_1164.all;

entity pci_target is port (
  PCI_Frame_n: in std_logic;	-- PCI Frame#
  PCI_Irdy_n: in std_logic;		-- PCI Irdy#
  Hit: in std_logic;			-- Hit on address decode
  D_Done: in std_logic;			-- Device decode complete
  Term: in std_logic;			-- Terminate transaction
  Ready: in std_logic;			-- Ready to transfer data
  Cmd_Write: in std_logic;		-- Command is Write
  Cmd_Read: in std_logic;		-- Command is Read
  T_Abort: in std_logic;		-- Target error  - abort transaction
  PCI_Clk: in std_logic;		-- PCI Clock
  PCI_Reset_n: in std_logic;	-- PCI Reset#

  PCI_Devsel_n: out std_logic;	-- PCI Devsel#
  PCI_Trdy_n: out std_logic;	-- PCI Trdy#
  PCI_Stop_n: out std_logic;	-- PCI Stop#
  OE_AD: out std_logic;			-- PCI AD bus enable
  OE_Trdy_n: out std_logic;		-- PCI Trdy# enable
  OE_Stop_n: out std_logic;		-- PCI Stop# enable
  OE_Devsel_n: out std_logic	-- PCI Devsel# enable

  );
end pci_target;

architecture fsm of pci_target is

signal LPCI_Devsel_n, LPCI_Trdy_n, LPCI_Stop_n: std_logic;

subtype targetFsmType is std_logic_vector(2 downto 0);

constant Idle: 		targetFsmType := "000";
constant B_Busy: 		targetFsmType := "101";
constant Backoff: 	targetFsmType := "010";
constant S_Data: 		targetFsmType := "011";
constant Turn_Ar: 	targetFsmType := "110";

signal currState, nextState: targetFsmType;

begin

 nxtStProc: process (currState, PCI_Frame_n, Hit, D_Done, PCI_Irdy_n, LPCI_Trdy_n,
			         LPCI_Devsel_n, LPCI_Stop_n, Term, Ready) begin
   case currState is
     when IDLE  =>
      if (PCI_Frame_n = '0' and Hit = '0') then
        nextState <= B_BUSY;
      else
        nextState <= IDLE;
      end if;

    when B_BUSY =>
      if (PCI_Frame_n ='1' and D_Done = '1') or
	     (PCI_Frame_n = '1' and D_Done = '0' and LPCI_Devsel_n = '0') then
        nextState <= IDLE;
      elsif (PCI_Frame_n = '0' or PCI_Irdy_n = '0') and Hit = '1' and
		    (Term = '0' or (Term = '1' and Ready = '1') ) then
        nextState <= S_Data;
      elsif (PCI_Frame_n = '0' or PCI_Irdy_n = '0') and Hit = '1' and
		    (Term = '1' and Ready = '0') then
        nextState <= BACKOFF;
      else
        nextState <= B_BUSY;
      end if;

    when S_DATA =>
      if PCI_Frame_n = '0' and LPCI_Stop_n = '0' and (LPCI_Trdy_n = '1' or PCI_Irdy_n = '0') then
        nextState <= BACKOFF;
      elsif PCI_Frame_n = '1' and (LPCI_Trdy_n = '0' or LPCI_Stop_n = '0') then
        nextState <= TURN_AR;
      else
        nextState <= S_DATA;
      end if;


    when BACKOFF =>
      if PCI_Frame_n = '1' then
        nextState <= TURN_AR;
      else
        nextState <= BACKOFF;
      end if;

    when TURN_AR  =>
      if (PCI_Frame_n = '0' and Hit = '0') then
        nextState <= B_BUSY;
      else
        nextState <= IDLE;
      end if;

      when others =>
	    null;
    end case;
  end process nxtStProc;


  curStProc: process (PCI_Clk, PCI_Reset_n) begin
    if (PCI_Reset_n = '0') then
      currState <= Idle;
    elsif (PCI_Clk'event and PCI_Clk = '1') then
      currState <= nextState;
    end if;
  end process curStProc;


  outConProc: process (currState, Ready, T_Abort, Cmd_Write,
                       Cmd_Read, T_Abort, Term) begin
    case currState is
      when S_Data =>
        if (Cmd_Read = '1') then 
		  OE_AD <= '1';
	    else
		  OE_AD <= '0';
		end if;

        if (Ready = '1' and T_Abort = '0' and (Cmd_Write = '1' or Cmd_Read = '1')) then
          LPCI_Trdy_n <= '0';
        else
          LPCI_Trdy_n <= '1';
        end if;

        if (T_Abort = '1' or Term = '1') and (Cmd_Write = '1' or Cmd_Read = '1')  then
          LPCI_Stop_n <= '0';
        else
          LPCI_Stop_n <= '1';
        end if;

        if (T_Abort = '0') then
          LPCI_Devsel_n <= '0';
        else
          LPCI_Devsel_n <= '1';
        end if;

        OE_Trdy_n <= '1';
        OE_Stop_n <= '1';
        OE_Devsel_n <= '1';

      when Backoff =>
        if (Cmd_Read = '1') then 
		  OE_AD <= '1';
	    else
		  OE_AD <= '0';
		end if;

        LPCI_Stop_n <= '0';

        OE_Trdy_n <= '1';
        OE_Stop_n <= '1';
        OE_Devsel_n <= '1';

        if (T_Abort = '0') then
          LPCI_Devsel_n <= '0';
        else
          LPCI_Devsel_n <= '1';
        end if;

      when Turn_Ar =>

        OE_Trdy_n <= '1';
        OE_Stop_n <= '1';
        OE_Devsel_n <= '1';

	  when others =>

        OE_Trdy_n <= '0';
        OE_Stop_n <= '0';
        OE_Devsel_n <= '0';
		OE_AD <= '0';
        LPCI_Trdy_n <= '1';
        LPCI_Stop_n <= '1';
        LPCI_Devsel_n <= '1';

    end case;

  end process outConProc;

  PCI_Devsel_n <= LPCI_Devsel_n;
  PCI_Trdy_n <= LPCI_Trdy_n;
  PCI_Stop_n <= LPCI_Stop_n;

end fsm;
library IEEE;
use IEEE.std_logic_1164.all;

entity pci_target is port (
  PCI_Frame_n: in std_logic;	-- PCI Frame#
  PCI_Irdy_n: in std_logic;		-- PCI Irdy#
  Hit: in std_logic;			-- Hit on address decode
  D_Done: in std_logic;			-- Device decode complete
  Term: in std_logic;			-- Terminate transaction
  Ready: in std_logic;			-- Ready to transfer data
  Cmd_Write: in std_logic;		-- Command is Write
  Cmd_Read: in std_logic;		-- Command is Read
  T_Abort: in std_logic;		-- Target error  - abort transaction
  PCI_Clk: in std_logic;		-- PCI Clock
  PCI_Reset_n: in std_logic;	-- PCI Reset#

  PCI_Devsel_n: out std_logic;	-- PCI Devsel#
  PCI_Trdy_n: out std_logic;	-- PCI Trdy#
  PCI_Stop_n: out std_logic;	-- PCI Stop#
  OE_AD: out std_logic;			-- PCI AD bus enable
  OE_Trdy_n: out std_logic;		-- PCI Trdy# enable
  OE_Stop_n: out std_logic;		-- PCI Stop# enable
  OE_Devsel_n: out std_logic	-- PCI Devsel# enable

  );
end pci_target;

architecture fsm of pci_target is

signal LPCI_Devsel_n, LPCI_Trdy_n, LPCI_Stop_n: std_logic;

subtype targetFsmType is std_logic_vector(2 downto 0);

constant Idle: 		targetFsmType := "000";
constant B_Busy: 		targetFsmType := "001";
constant Backoff: 	targetFsmType := "011";
constant S_Data: 		targetFsmType := "010";
constant Turn_Ar: 	targetFsmType := "110";

signal currState, nextState: targetFsmType;

begin

 nxtStProc: process (currState, PCI_Frame_n, Hit, D_Done, PCI_Irdy_n, LPCI_Trdy_n,
			         LPCI_Devsel_n, LPCI_Stop_n, Term, Ready) begin
   case currState is
     when IDLE  =>
      if (PCI_Frame_n = '0' and Hit = '0') then
        nextState <= B_BUSY;
      else
        nextState <= IDLE;
      end if;

    when B_BUSY =>
      if (PCI_Frame_n ='1' and D_Done = '1') or
	     (PCI_Frame_n = '1' and D_Done = '0' and LPCI_Devsel_n = '0') then
        nextState <= IDLE;
      elsif (PCI_Frame_n = '0' or PCI_Irdy_n = '0') and Hit = '1' and
		    (Term = '0' or (Term = '1' and Ready = '1') ) then
        nextState <= S_Data;
      elsif (PCI_Frame_n = '0' or PCI_Irdy_n = '0') and Hit = '1' and
		    (Term = '1' and Ready = '0') then
        nextState <= BACKOFF;
      else
        nextState <= B_BUSY;
      end if;

    when S_DATA =>
      if PCI_Frame_n = '0' and LPCI_Stop_n = '0' and (LPCI_Trdy_n = '1' or PCI_Irdy_n = '0') then
        nextState <= BACKOFF;
      elsif PCI_Frame_n = '1' and (LPCI_Trdy_n = '0' or LPCI_Stop_n = '0') then
        nextState <= TURN_AR;
      else
        nextState <= S_DATA;
      end if;


    when BACKOFF =>
      if PCI_Frame_n = '1' then
        nextState <= TURN_AR;
      else
        nextState <= BACKOFF;
      end if;

    when TURN_AR  =>
      if (PCI_Frame_n = '0' and Hit = '0') then
        nextState <= B_BUSY;
      else
        nextState <= IDLE;
      end if;

      when others =>
	    null;
    end case;
  end process nxtStProc;


  curStProc: process (PCI_Clk, PCI_Reset_n) begin
    if (PCI_Reset_n = '0') then
      currState <= Idle;
    elsif (PCI_Clk'event and PCI_Clk = '1') then
      currState <= nextState;
    end if;
  end process curStProc;


  outConProc: process (currState, Ready, T_Abort, Cmd_Write,
                       Cmd_Read, T_Abort, Term) begin
    case currState is
      when S_Data =>
        if (Cmd_Read = '1') then 
		  OE_AD <= '1';
	    else
		  OE_AD <= '0';
		end if;

        if (Ready = '1' and T_Abort = '0' and (Cmd_Write = '1' or Cmd_Read = '1')) then
          LPCI_Trdy_n <= '0';
        else
          LPCI_Trdy_n <= '1';
        end if;

        if (T_Abort = '1' or Term = '1') and (Cmd_Write = '1' or Cmd_Read = '1')  then
          LPCI_Stop_n <= '0';
        else
          LPCI_Stop_n <= '1';
        end if;

        if (T_Abort = '0') then
          LPCI_Devsel_n <= '0';
        else
          LPCI_Devsel_n <= '1';
        end if;

        OE_Trdy_n <= '1';
        OE_Stop_n <= '1';
        OE_Devsel_n <= '1';

      when Backoff =>
        if (Cmd_Read = '1') then 
		  OE_AD <= '1';
	    else
		  OE_AD <= '0';
		end if;

        LPCI_Stop_n <= '0';

        OE_Trdy_n <= '1';
        OE_Stop_n <= '1';
        OE_Devsel_n <= '1';

        if (T_Abort = '0') then
          LPCI_Devsel_n <= '0';
        else
          LPCI_Devsel_n <= '1';
        end if;

      when Turn_Ar =>

        OE_Trdy_n <= '1';
        OE_Stop_n <= '1';
        OE_Devsel_n <= '1';

	  when others =>

        OE_Trdy_n <= '0';
        OE_Stop_n <= '0';
        OE_Devsel_n <= '0';
		OE_AD <= '0';
        LPCI_Trdy_n <= '1';
        LPCI_Stop_n <= '1';
        LPCI_Devsel_n <= '1';

    end case;

  end process outConProc;

  PCI_Devsel_n <= LPCI_Devsel_n;
  PCI_Trdy_n <= LPCI_Trdy_n;
  PCI_Stop_n <= LPCI_Stop_n;

end fsm;
library IEEE;
use IEEE.std_logic_1164.all;

entity pci_target is port (
  PCI_Frame_n: in std_logic;	-- PCI Frame#
  PCI_Irdy_n: in std_logic;		-- PCI Irdy#
  Hit: in std_logic;			-- Hit on address decode
  D_Done: in std_logic;			-- Device decode complete
  Term: in std_logic;			-- Terminate transaction
  Ready: in std_logic;			-- Ready to transfer data
  Cmd_Write: in std_logic;		-- Command is Write
  Cmd_Read: in std_logic;		-- Command is Read
  T_Abort: in std_logic;		-- Target error  - abort transaction
  PCI_Clk: in std_logic;		-- PCI Clock
  PCI_Reset_n: in std_logic;	-- PCI Reset#

  PCI_Devsel_n: out std_logic;	-- PCI Devsel#
  PCI_Trdy_n: out std_logic;	-- PCI Trdy#
  PCI_Stop_n: out std_logic;	-- PCI Stop#
  OE_AD: out std_logic;			-- PCI AD bus enable
  OE_Trdy_n: out std_logic;		-- PCI Trdy# enable
  OE_Stop_n: out std_logic;		-- PCI Stop# enable
  OE_Devsel_n: out std_logic	-- PCI Devsel# enable

  );
end pci_target;

architecture fsm of pci_target is

signal LPCI_Devsel_n, LPCI_Trdy_n, LPCI_Stop_n: std_logic;

subtype targetFsmType is std_logic_vector(2 downto 0);

constant Idle: 		targetFsmType := "000";
constant B_Busy: 	targetFsmType := "001";
constant Backoff: 	targetFsmType := "010";
constant S_Data: 	targetFsmType := "011";
constant Turn_Ar: 	targetFsmType := "100";

signal currState, nextState: targetFsmType;

begin

 nxtStProc: process (currState, PCI_Frame_n, Hit, D_Done, PCI_Irdy_n, LPCI_Trdy_n,
			         LPCI_Devsel_n, LPCI_Stop_n, Term, Ready) begin
   case currState is
     when IDLE  =>
      if (PCI_Frame_n = '0' and Hit = '0') then
        nextState <= B_BUSY;
      else
        nextState <= IDLE;
      end if;

    when B_BUSY =>
      if (PCI_Frame_n ='1' and D_Done = '1') or
	     (PCI_Frame_n = '1' and D_Done = '0' and LPCI_Devsel_n = '0') then
        nextState <= IDLE;
      elsif (PCI_Frame_n = '0' or PCI_Irdy_n = '0') and Hit = '1' and
		    (Term = '0' or (Term = '1' and Ready = '1') ) then
        nextState <= S_Data;
      elsif (PCI_Frame_n = '0' or PCI_Irdy_n = '0') and Hit = '1' and
		    (Term = '1' and Ready = '0') then
        nextState <= BACKOFF;
      else
        nextState <= B_BUSY;
      end if;

    when S_DATA =>
      if PCI_Frame_n = '0' and LPCI_Stop_n = '0' and (LPCI_Trdy_n = '1' or PCI_Irdy_n = '0') then
        nextState <= BACKOFF;
      elsif PCI_Frame_n = '1' and (LPCI_Trdy_n = '0' or LPCI_Stop_n = '0') then
        nextState <= TURN_AR;
      else
        nextState <= S_DATA;
      end if;


    when BACKOFF =>
      if PCI_Frame_n = '1' then
        nextState <= TURN_AR;
      else
        nextState <= BACKOFF;
      end if;

    when TURN_AR  =>
      if (PCI_Frame_n = '0' and Hit = '0') then
        nextState <= B_BUSY;
      else
        nextState <= IDLE;
      end if;

      when others =>
	    null;
    end case;
  end process nxtStProc;


  curStProc: process (PCI_Clk, PCI_Reset_n) begin
    if (PCI_Reset_n = '0') then
      currState <= Idle;
    elsif (PCI_Clk'event and PCI_Clk = '1') then
      currState <= nextState;
    end if;
  end process curStProc;


  outConProc: process (currState, Ready, T_Abort, Cmd_Write,
                       Cmd_Read, T_Abort, Term) begin
    case currState is
      when S_Data =>
        if (Cmd_Read = '1') then 
		  OE_AD <= '1';
	    else
		  OE_AD <= '0';
		end if;

        if (Ready = '1' and T_Abort = '0' and (Cmd_Write = '1' or Cmd_Read = '1')) then
          LPCI_Trdy_n <= '0';
        else
          LPCI_Trdy_n <= '1';
        end if;

        if (T_Abort = '1' or Term = '1') and (Cmd_Write = '1' or Cmd_Read = '1')  then
          LPCI_Stop_n <= '0';
        else
          LPCI_Stop_n <= '1';
        end if;

        if (T_Abort = '0') then
          LPCI_Devsel_n <= '0';
        else
          LPCI_Devsel_n <= '1';
        end if;

        OE_Trdy_n <= '1';
        OE_Stop_n <= '1';
        OE_Devsel_n <= '1';

      when Backoff =>
        if (Cmd_Read = '1') then 
		  OE_AD <= '1';
	    else
		  OE_AD <= '0';
		end if;

        LPCI_Stop_n <= '0';

        OE_Trdy_n <= '1';
        OE_Stop_n <= '1';
        OE_Devsel_n <= '1';

        if (T_Abort = '0') then
          LPCI_Devsel_n <= '0';
        else
          LPCI_Devsel_n <= '1';
        end if;

      when Turn_Ar =>

        OE_Trdy_n <= '1';
        OE_Stop_n <= '1';
        OE_Devsel_n <= '1';

	  when others =>

        OE_Trdy_n <= '0';
        OE_Stop_n <= '0';
        OE_Devsel_n <= '0';
		OE_AD <= '0';
        LPCI_Trdy_n <= '1';
        LPCI_Stop_n <= '1';
        LPCI_Devsel_n <= '1';

    end case;

  end process outConProc;

  PCI_Devsel_n <= LPCI_Devsel_n;
  PCI_Trdy_n <= LPCI_Trdy_n;
  PCI_Stop_n <= LPCI_Stop_n;

end fsm;
library IEEE;
use IEEE.std_logic_1164.all;

entity pci_target is port (
  PCI_Frame_n: in std_logic;	-- PCI Frame#
  PCI_Irdy_n: in std_logic;		-- PCI Irdy#
  Hit: in std_logic;			-- Hit on address decode
  D_Done: in std_logic;			-- Device decode complete
  Term: in std_logic;			-- Terminate transaction
  Ready: in std_logic;			-- Ready to transfer data
  Cmd_Write: in std_logic;		-- Command is Write
  Cmd_Read: in std_logic;		-- Command is Read
  T_Abort: in std_logic;		-- Target error  - abort transaction
  PCI_Clk: in std_logic;		-- PCI Clock
  PCI_Reset_n: in std_logic;	-- PCI Reset#

  PCI_Devsel_n: out std_logic;	-- PCI Devsel#
  PCI_Trdy_n: out std_logic;	-- PCI Trdy#
  PCI_Stop_n: out std_logic;	-- PCI Stop#
  OE_AD: out std_logic;			-- PCI AD bus enable
  OE_Trdy_n: out std_logic;		-- PCI Trdy# enable
  OE_Stop_n: out std_logic;		-- PCI Stop# enable
  OE_Devsel_n: out std_logic	-- PCI Devsel# enable

  );
end pci_target;

architecture fsm of pci_target is

signal LPCI_Devsel_n, LPCI_Trdy_n, LPCI_Stop_n: std_logic;

subtype targetFsmType is std_logic_vector(3 downto 0);

constant Idle: 		targetFsmType := "0000";
constant B_Busy: 	targetFsmType := "0001";
constant Backoff: 	targetFsmType := "0011";
constant S_Data: 	targetFsmType := "1100";
constant Turn_Ar: 	targetFsmType := "1101";

signal currState, nextState: targetFsmType;

begin

 nxtStProc: process (currState, PCI_Frame_n, Hit, D_Done, PCI_Irdy_n, LPCI_Trdy_n,
			         LPCI_Devsel_n, LPCI_Stop_n, Term, Ready) begin
   case currState is
     when IDLE  =>
      if (PCI_Frame_n = '0' and Hit = '0') then
        nextState <= B_BUSY;
      else
        nextState <= IDLE;
      end if;

    when B_BUSY =>
      if (PCI_Frame_n ='1' and D_Done = '1') or
	     (PCI_Frame_n = '1' and D_Done = '0' and LPCI_Devsel_n = '0') then
        nextState <= IDLE;
      elsif (PCI_Frame_n = '0' or PCI_Irdy_n = '0') and Hit = '1' and
		    (Term = '0' or (Term = '1' and Ready = '1') ) then
        nextState <= S_Data;
      elsif (PCI_Frame_n = '0' or PCI_Irdy_n = '0') and Hit = '1' and
		    (Term = '1' and Ready = '0') then
        nextState <= BACKOFF;
      else
        nextState <= B_BUSY;
      end if;

    when S_DATA =>
      if PCI_Frame_n = '0' and LPCI_Stop_n = '0' and (LPCI_Trdy_n = '1' or PCI_Irdy_n = '0') then
        nextState <= BACKOFF;
      elsif PCI_Frame_n = '1' and (LPCI_Trdy_n = '0' or LPCI_Stop_n = '0') then
        nextState <= TURN_AR;
      else
        nextState <= S_DATA;
      end if;


    when BACKOFF =>
      if PCI_Frame_n = '1' then
        nextState <= TURN_AR;
      else
        nextState <= BACKOFF;
      end if;

    when TURN_AR  =>
      if (PCI_Frame_n = '0' and Hit = '0') then
        nextState <= B_BUSY;
      else
        nextState <= IDLE;
      end if;

      when others =>
	    null;
    end case;
  end process nxtStProc;


  curStProc: process (PCI_Clk, PCI_Reset_n) begin
    if (PCI_Reset_n = '0') then
      currState <= Idle;
    elsif (PCI_Clk'event and PCI_Clk = '1') then
      currState <= nextState;
    end if;
  end process curStProc;


  outConProc: process (currState, Ready, T_Abort, Cmd_Write,
                       Cmd_Read, T_Abort, Term) begin
    case currState is
      when S_Data =>
        if (Cmd_Read = '1') then 
		  OE_AD <= '1';
	    else
		  OE_AD <= '0';
		end if;

        if (Ready = '1' and T_Abort = '0' and (Cmd_Write = '1' or Cmd_Read = '1')) then
          LPCI_Trdy_n <= '0';
        else
          LPCI_Trdy_n <= '1';
        end if;

        if (T_Abort = '1' or Term = '1') and (Cmd_Write = '1' or Cmd_Read = '1')  then
          LPCI_Stop_n <= '0';
        else
          LPCI_Stop_n <= '1';
        end if;

        if (T_Abort = '0') then
          LPCI_Devsel_n <= '0';
        else
          LPCI_Devsel_n <= '1';
        end if;

        OE_Trdy_n <= '1';
        OE_Stop_n <= '1';
        OE_Devsel_n <= '1';

      when Backoff =>
        if (Cmd_Read = '1') then 
		  OE_AD <= '1';
	    else
		  OE_AD <= '0';
		end if;

        LPCI_Stop_n <= '0';

        OE_Trdy_n <= '1';
        OE_Stop_n <= '1';
        OE_Devsel_n <= '1';

        if (T_Abort = '0') then
          LPCI_Devsel_n <= '0';
        else
          LPCI_Devsel_n <= '1';
        end if;

      when Turn_Ar =>

        OE_Trdy_n <= '1';
        OE_Stop_n <= '1';
        OE_Devsel_n <= '1';

	  when others =>

        OE_Trdy_n <= '0';
        OE_Stop_n <= '0';
        OE_Devsel_n <= '0';
		OE_AD <= '0';
        LPCI_Trdy_n <= '1';
        LPCI_Stop_n <= '1';
        LPCI_Devsel_n <= '1';

    end case;

  end process outConProc;

  PCI_Devsel_n <= LPCI_Devsel_n;
  PCI_Trdy_n <= LPCI_Trdy_n;
  PCI_Stop_n <= LPCI_Stop_n;

end fsm;
library IEEE;
use IEEE.std_logic_1164.all;

entity pci_target is port (
  PCI_Frame_n: in std_logic;	-- PCI Frame#
  PCI_Irdy_n: in std_logic;		-- PCI Irdy#
  Hit: in std_logic;			-- Hit on address decode
  D_Done: in std_logic;			-- Device decode complete
  Term: in std_logic;			-- Terminate transaction
  Ready: in std_logic;			-- Ready to transfer data
  Cmd_Write: in std_logic;		-- Command is Write
  Cmd_Read: in std_logic;		-- Command is Read
  T_Abort: in std_logic;		-- Target error  - abort transaction
  PCI_Clk: in std_logic;		-- PCI Clock
  PCI_Reset_n: in std_logic;	-- PCI Reset#

  PCI_Devsel_n: out std_logic;	-- PCI Devsel#
  PCI_Trdy_n: out std_logic;	-- PCI Trdy#
  PCI_Stop_n: out std_logic;	-- PCI Stop#
  OE_AD: out std_logic;			-- PCI AD bus enable
  OE_Trdy_n: out std_logic;		-- PCI Trdy# enable
  OE_Stop_n: out std_logic;		-- PCI Stop# enable
  OE_Devsel_n: out std_logic	-- PCI Devsel# enable

  );
end pci_target;

architecture fsm of pci_target is

signal LPCI_Devsel_n, LPCI_Trdy_n, LPCI_Stop_n: std_logic;

subtype targetFsmType is std_logic_vector(2 downto 0);

constant Idle: 		targetFsmType := "000";
constant B_Busy: 		targetFsmType := "101";
constant Backoff: 	targetFsmType := "010";
constant S_Data: 		targetFsmType := "011";
constant Turn_Ar: 	targetFsmType := "110";
constant Dont_Care: targetFsmType := "XXX";

signal currState, nextState: targetFsmType;

begin

 nxtStProc: process (currState, PCI_Frame_n, Hit, D_Done, PCI_Irdy_n, LPCI_Trdy_n,
			         LPCI_Devsel_n, LPCI_Stop_n, Term, Ready) begin
   case currState is
     when IDLE  =>
      if (PCI_Frame_n = '0' and Hit = '0') then
        nextState <= B_BUSY;
      else
        nextState <= IDLE;
      end if;

    when B_BUSY =>
      if (PCI_Frame_n ='1' and D_Done = '1') or
	     (PCI_Frame_n = '1' and D_Done = '0' and LPCI_Devsel_n = '0') then
        nextState <= IDLE;
      elsif (PCI_Frame_n = '0' or PCI_Irdy_n = '0') and Hit = '1' and
		    (Term = '0' or (Term = '1' and Ready = '1') ) then
        nextState <= S_Data;
      elsif (PCI_Frame_n = '0' or PCI_Irdy_n = '0') and Hit = '1' and
		    (Term = '1' and Ready = '0') then
        nextState <= BACKOFF;
      else
        nextState <= B_BUSY;
      end if;

    when S_DATA =>
      if PCI_Frame_n = '0' and LPCI_Stop_n = '0' and (LPCI_Trdy_n = '1' or PCI_Irdy_n = '0') then
        nextState <= BACKOFF;
      elsif PCI_Frame_n = '1' and (LPCI_Trdy_n = '0' or LPCI_Stop_n = '0') then
        nextState <= TURN_AR;
      else
        nextState <= S_DATA;
      end if;


    when BACKOFF =>
      if PCI_Frame_n = '1' then
        nextState <= TURN_AR;
      else
        nextState <= BACKOFF;
      end if;

    when TURN_AR  =>
      if (PCI_Frame_n = '0' and Hit = '0') then
        nextState <= B_BUSY;
      else
        nextState <= IDLE;
      end if;

      when others =>
	    nextState <= Dont_Care;
    end case;
  end process nxtStProc;


  curStProc: process (PCI_Clk, PCI_Reset_n) begin
    if (PCI_Reset_n = '0') then
      currState <= Idle;
    elsif (PCI_Clk'event and PCI_Clk = '1') then
      currState <= nextState;
    end if;
  end process curStProc;


  outConProc: process (currState, Ready, T_Abort, Cmd_Write,
                       Cmd_Read, T_Abort, Term) begin

  -- Set default output assignments
  OE_Trdy_n <= '0';
  OE_Stop_n <= '0';
  OE_Devsel_n <= '0';
  OE_AD <= '0';
  LPCI_Trdy_n <= '1';
  LPCI_Stop_n <= '1';
  LPCI_Devsel_n <= '1';

    case currState is
      when S_Data =>
        if (Cmd_Read = '1') then 
		  OE_AD <= '1';
	    else
		  OE_AD <= '0';
		end if;

        if (Ready = '1' and T_Abort = '0' and (Cmd_Write = '1' or Cmd_Read = '1')) then
          LPCI_Trdy_n <= '0';
        else
          LPCI_Trdy_n <= '1';
        end if;

        if (T_Abort = '1' or Term = '1') and (Cmd_Write = '1' or Cmd_Read = '1')  then
          LPCI_Stop_n <= '0';
        else
          LPCI_Stop_n <= '1';
        end if;

        if (T_Abort = '0') then
          LPCI_Devsel_n <= '0';
        else
          LPCI_Devsel_n <= '1';
        end if;

        OE_Trdy_n <= '1';
        OE_Stop_n <= '1';
        OE_Devsel_n <= '1';

      when Backoff =>
        if (Cmd_Read = '1') then 
		  OE_AD <= '1';
	    else
		  OE_AD <= '0';
		end if;

        LPCI_Stop_n <= '0';

        OE_Trdy_n <= '1';
        OE_Stop_n <= '1';
        OE_Devsel_n <= '1';

        if (T_Abort = '0') then
          LPCI_Devsel_n <= '0';
        else
          LPCI_Devsel_n <= '1';
        end if;

      when Turn_Ar =>

        OE_Trdy_n <= '1';
        OE_Stop_n <= '1';
        OE_Devsel_n <= '1';

	  when others =>

        OE_Trdy_n <= '0';
        OE_Stop_n <= '0';
        OE_Devsel_n <= '0';
		OE_AD <= '0';
        LPCI_Trdy_n <= '1';
        LPCI_Stop_n <= '1';
        LPCI_Devsel_n <= '1';

    end case;

  end process outConProc;

  PCI_Devsel_n <= LPCI_Devsel_n;
  PCI_Trdy_n <= LPCI_Trdy_n;
  PCI_Stop_n <= LPCI_Stop_n;

end fsm;
library IEEE;
use IEEE.std_logic_1164.all;

entity pci_target is port (
  PCI_Frame_n: in std_logic;    -- PCI Frame#
  PCI_Irdy_n: in std_logic;     -- PCI Irdy#
  Hit: in std_logic;            -- Hit on address decode
  D_Done: in std_logic;         -- Device decode complete
  Term: in std_logic;           -- Terminate transaction
  Ready: in std_logic;          -- Ready to transfer data
  Cmd_Write: in std_logic;      -- Command is Write
  Cmd_Read: in std_logic;       -- Command is Read
  T_Abort: in std_logic;        -- Target error  - abort transaction
  PCI_Clk: in std_logic;        -- PCI Clock
  PCI_Reset_n: in std_logic;    -- PCI Reset#

  PCI_Devsel_n: out std_logic;	-- PCI Devsel#
  PCI_Stop_n: out std_logic;    -- PCI Stop#
  PCI_Trdy_n: out std_logic;    -- PCI Trdy#
  OE_AD: out std_logic;         -- PCI AD bus enable
  OE_Trdy_n: out std_logic;     -- PCI Trdy# enable
  OE_Stop_n: out std_logic;     -- PCI Stop# enable
  OE_Devsel_n: out std_logic    -- PCI Devsel# enable
  );
end pci_target;

architecture fsm of pci_target is

signal LPCI_Devsel_n, LPCI_Trdy_n, LPCI_Stop_n: std_logic;

type targetFsmType is (Idle, B_Busy, Backoff, S_Data, Turn_Ar);

signal currState, nextState: targetFsmType;

begin

-- Process to generate next state logic

 nxtStProc: process (currState, PCI_Frame_n, Hit, D_Done, PCI_Irdy_n, LPCI_Trdy_n,
			         LPCI_Devsel_n, LPCI_Stop_n, Term, Ready) begin
   case currState is
     when Idle  =>
      if (PCI_Frame_n = '0' and Hit = '0') then
        nextState <= B_Busy;
      else
        nextState <= Idle;
      end if;

    when B_Busy =>
      if (PCI_Frame_n ='1' and D_Done = '1') or
	     (PCI_Frame_n = '1' and D_Done = '0' and LPCI_Devsel_n = '0') then
        nextState <= Idle;
      elsif (PCI_Frame_n = '0' or PCI_Irdy_n = '0') and Hit = '1' and
		    (Term = '0' or (Term = '1' and Ready = '1') ) then
        nextState <= S_Data;
      elsif (PCI_Frame_n = '0' or PCI_Irdy_n = '0') and Hit = '1' and
		    (Term = '1' and Ready = '0') then
        nextState <= Backoff;
      else
        nextState <= B_Busy;
      end if;

    when S_Data =>
      if PCI_Frame_n = '0' and LPCI_Stop_n = '0' and (LPCI_Trdy_n = '1' or PCI_Irdy_n = '0') then
        nextState <= Backoff;
      elsif PCI_Frame_n = '1' and (LPCI_Trdy_n = '0' or LPCI_Stop_n = '0') then
        nextState <= Turn_Ar;
      else
        nextState <= S_Data;
      end if;


    when Backoff =>
      if PCI_Frame_n = '1' then
        nextState <= Turn_Ar;
      else
        nextState <= Backoff;
      end if;

    when Turn_Ar  =>
      if (PCI_Frame_n = '0' and Hit = '0') then
        nextState <= B_Busy;
      else
        nextState <= Idle;
      end if;

    when others =>
      null;

    end case;

  end process nxtStProc;


-- Process to register the current state

  curStProc: process (PCI_Clk, PCI_Reset_n) begin
    if (PCI_Reset_n = '0') then
      currState <= Idle;
    elsif (PCI_Clk'event and PCI_Clk = '1') then
      currState <= nextState;
    end if;
  end process curStProc;


-- Process to generate outputs

  outConProc: process (currState, Ready, T_Abort, Cmd_Write,
                       Cmd_Read, T_Abort, Term) begin
    case currState is
      when S_Data =>
        if (Cmd_Read = '1') then 
          OE_AD <= '1';
        else
          OE_AD <= '0';
        end if;

        if (Ready = '1' and T_Abort = '0' and (Cmd_Write = '1' or Cmd_Read = '1')) then
          LPCI_Trdy_n <= '0';
        else
          LPCI_Trdy_n <= '1';
        end if;

        if (T_Abort = '1' or Term = '1') and (Cmd_Write = '1' or Cmd_Read = '1')  then
          LPCI_Stop_n <= '0';
        else
          LPCI_Stop_n <= '1';
        end if;

        if (T_Abort = '0') then
          LPCI_Devsel_n <= '0';
        else
          LPCI_Devsel_n <= '1';
        end if;

        OE_Trdy_n <= '1';
        OE_Stop_n <= '1';
        OE_Devsel_n <= '1';

      when Backoff =>
        if (Cmd_Read = '1') then 
          OE_AD <= '1';
        else
          OE_AD <= '0';
        end if;

        LPCI_Stop_n <= '0';

        OE_Trdy_n <= '1';
        OE_Stop_n <= '1';
        OE_Devsel_n <= '1';

        if (T_Abort = '0') then
          LPCI_Devsel_n <= '0';
        else
          LPCI_Devsel_n <= '1';
        end if;

      when Turn_Ar =>

        OE_Trdy_n <= '1';
        OE_Stop_n <= '1';
        OE_Devsel_n <= '1';

      when others =>

        OE_Trdy_n <= '0';
        OE_Stop_n <= '0';
        OE_Devsel_n <= '0';
        OE_AD <= '0';
        LPCI_Trdy_n <= '1';
        LPCI_Stop_n <= '1';
        LPCI_Devsel_n <= '1';

    end case;

  end process outConProc;

-- Assign output ports

  PCI_Devsel_n <= LPCI_Devsel_n;
  PCI_Trdy_n <= LPCI_Trdy_n;
  PCI_Stop_n <= LPCI_Stop_n;

end fsm;
-- Incorporates Errata 10.1 and 10.2

library IEEE;
use IEEE.std_logic_1164.all;

entity pci_target is port (
  PCI_Frame_n: in std_logic;    -- PCI Frame#
  PCI_Irdy_n: in std_logic;     -- PCI Irdy#
  Hit: in std_logic;            -- Hit on address decode
  D_Done: in std_logic;         -- Device decode complete
  Term: in std_logic;           -- Terminate transaction
  Ready: in std_logic;          -- Ready to transfer data
  Cmd_Write: in std_logic;      -- Command is Write
  Cmd_Read: in std_logic;       -- Command is Read
  T_Abort: in std_logic;        -- Target error  - abort transaction
  PCI_Clk: in std_logic;        -- PCI Clock
  PCI_Reset_n: in std_logic;    -- PCI Reset#

  PCI_Devsel_n: out std_logic;  -- PCI Devsel#
  PCI_Trdy_n: out std_logic;    -- PCI Trdy#
  PCI_Stop_n: out std_logic;    -- PCI Stop#
  OE_AD: out std_logic;         -- PCI AD bus enable
  OE_Trdy_n: out std_logic;     -- PCI Trdy# enable
  OE_Stop_n: out std_logic;     -- PCI Stop# enable
  OE_Devsel_n: out std_logic    -- PCI Devsel# enable
  );
end pci_target;

architecture fsm of pci_target is

signal LPCI_Devsel_n, LPCI_Trdy_n, LPCI_Stop_n: std_logic;

subtype targetFsmType is std_logic_vector(4 downto 0);

constant Idle:    integer := 0;
constant B_Busy:  integer := 1;
constant Backoff: integer := 2;
constant S_Data:  integer := 3;
constant Turn_Ar: integer := 4;

signal currState, nextState: targetFsmType;

begin

 nxtStProc: process (currState, PCI_Frame_n, Hit, D_Done, PCI_Irdy_n, LPCI_Trdy_n,
                     LPCI_Devsel_n, LPCI_Stop_n, Term, Ready) begin

   nextState <= (others => '0');

   if currState(Idle) = '1' then
      if (PCI_Frame_n = '0' and Hit = '0') then
        nextState(B_Busy) <= '1';
      else
        nextState(Idle) <= '1';
      end if;
   end if;

   if currState(B_Busy) = '1' then
      if (PCI_Frame_n ='1' and D_Done = '1') or
	     (PCI_Frame_n = '1' and D_Done = '0' and LPCI_Devsel_n = '0') then
        nextState(Idle) <= '1';
      elsif (PCI_Frame_n = '0' or PCI_Irdy_n = '0') and Hit = '1' and
		    (Term = '0' or (Term = '1' and Ready = '1') ) then
        nextState(S_Data) <= '1';
      elsif (PCI_Frame_n = '0' or PCI_Irdy_n = '0') and Hit = '1' and
		    (Term = '1' and Ready = '0') then
        nextState(Backoff) <= '1';
      else
        nextState(B_Busy) <= '1';
      end if;
   end if;

   if currState(S_Data) = '1' then
      if PCI_Frame_n = '0' and LPCI_Stop_n = '0' and
         (LPCI_Trdy_n = '1' or PCI_Irdy_n = '0') then
        nextState(Backoff) <= '1';
      elsif PCI_Frame_n = '1' and (LPCI_Trdy_n = '0' or LPCI_Stop_n = '0') then
        nextState(Turn_Ar) <= '1';
      else
        nextState(S_Data) <= '1';
      end if;
   end if;


   if currState(Backoff) = '1' then
      if PCI_Frame_n = '1' then
        nextState(Turn_Ar) <= '1';
      else
        nextState(Backoff) <= '1';
      end if;
   end if;

   if currState(Turn_Ar) = '1' then
      if (PCI_Frame_n = '0' and Hit = '0') then
        nextState(B_Busy) <= '1';
      else
        nextState(Idle) <= '1';
      end if;
   end if;

  end process nxtStProc;


  curStProc: process (PCI_Clk, PCI_Reset_n) begin
    if (PCI_Reset_n = '0') then
	currState <= (others => '0'); -- per Errata 10.2
      currState(Idle) <= '1';
    elsif (PCI_Clk'event and PCI_Clk = '1') then
      currState <= nextState;
    end if;
  end process curStProc;


  outConProc: process (currState, Ready, T_Abort, Cmd_Write,
                       Cmd_Read, T_Abort, Term) begin
      OE_Trdy_n <= '0';	OE_Stop_n <= '0';	OE_Devsel_n <= '0';	-- defaults per errata 10.1
      OE_AD <= '0';		LPCI_Trdy_n <= '1';	LPCI_Stop_n <= '1';
	  LPCI_Devsel_n <= '1';

    if (currState(S_Data) = '1') then
      if (Cmd_Read = '1') then 
        OE_AD <= '1';
      else
        OE_AD <= '0';
      end if;

      if (Ready = '1' and T_Abort = '0' and (Cmd_Write = '1' or Cmd_Read = '1')) then
          LPCI_Trdy_n <= '0';
      else
        LPCI_Trdy_n <= '1';
      end if;

      if (T_Abort = '1' or Term = '1') and (Cmd_Write = '1' or Cmd_Read = '1')  then
        LPCI_Stop_n <= '0';
      else
        LPCI_Stop_n <= '1';
      end if;

      if (T_Abort = '0') then
        LPCI_Devsel_n <= '0';
      else
        LPCI_Devsel_n <= '1';
      end if;

      OE_Trdy_n <= '1';
      OE_Stop_n <= '1';
      OE_Devsel_n <= '1';
    end if;


    if (currState(Backoff) = '1') then
      if (Cmd_Read = '1') then 
        OE_AD <= '1';
      else
        OE_AD <= '0';
      end if;

      LPCI_Stop_n <= '0';

      OE_Trdy_n <= '1';
      OE_Stop_n <= '1';
      OE_Devsel_n <= '1';

      if (T_Abort = '0') then
        LPCI_Devsel_n <= '0';
      else
        LPCI_Devsel_n <= '1';
      end if;
    end if;


    if (currState(Turn_Ar) = '1') then
      OE_Trdy_n <= '1';
      OE_Stop_n <= '1';
      OE_Devsel_n <= '1';
    end if;

    if (currState(Idle) = '1' or currState(B_Busy) = '1') then
      OE_Trdy_n <= '0';
      OE_Stop_n <= '0';
      OE_Devsel_n <= '0';
      OE_AD <= '0';
      LPCI_Trdy_n <= '1';
      LPCI_Stop_n <= '1';
      LPCI_Devsel_n <= '1';
    end if;

   end process outConProc;

  PCI_Devsel_n <= LPCI_Devsel_n;
  PCI_Trdy_n <= LPCI_Trdy_n;
  PCI_Stop_n <= LPCI_Stop_n;

end fsm;
library IEEE;
use IEEE.std_logic_1164.all;

entity pci_target is port (
  PCI_Frame_n: in std_logic;	-- PCI Frame#
  PCI_Irdy_n: in std_logic;		-- PCI Irdy#
  Hit: in std_logic;			-- Hit on address decode
  D_Done: in std_logic;			-- Device decode complete
  Term: in std_logic;			-- Terminate transaction
  Ready: in std_logic;			-- Ready to transfer data
  Cmd_Write: in std_logic;		-- Command is Write
  Cmd_Read: in std_logic;		-- Command is Read
  T_Abort: in std_logic;		-- Target error  - abort transaction
  PCI_Clk: in std_logic;		-- PCI Clock
  PCI_Reset_n: in std_logic;	-- PCI Reset#

  PCI_Devsel_n: out std_logic;	-- PCI Devsel#
  PCI_Trdy_n: out std_logic;	-- PCI Trdy#
  PCI_Stop_n: out std_logic;	-- PCI Stop#
  OE_AD: out std_logic;			-- PCI AD bus enable
  OE_Trdy_n: out std_logic;		-- PCI Trdy# enable
  OE_Stop_n: out std_logic;		-- PCI Stop# enable
  OE_Devsel_n: out std_logic	-- PCI Devsel# enable

  );
end pci_target;

architecture fsm of pci_target is

signal LPCI_Devsel_n, LPCI_Trdy_n, LPCI_Stop_n: std_logic;

subtype targetFsmType is std_logic_vector(2 downto 0);

constant Idle: 		targetFsmType := "000";
constant B_Busy: 	targetFsmType := "001";
constant Backoff: 	targetFsmType := "011";
constant S_Data: 	targetFsmType := "110";
constant Turn_Ar: 	targetFsmType := "100";

signal currState, nextState: targetFsmType;

begin

 nxtStProc: process (currState, PCI_Frame_n, Hit, D_Done, PCI_Irdy_n, LPCI_Trdy_n,
			         LPCI_Devsel_n, LPCI_Stop_n, Term, Ready) begin
   case currState is
     when IDLE  =>
      if (PCI_Frame_n = '0' and Hit = '0') then
        nextState <= B_BUSY;
      else
        nextState <= IDLE;
      end if;

    when B_BUSY =>
      if (PCI_Frame_n ='1' and D_Done = '1') or
	     (PCI_Frame_n = '1' and D_Done = '0' and LPCI_Devsel_n = '0') then
        nextState <= IDLE;
      elsif (PCI_Frame_n = '0' or PCI_Irdy_n = '0') and Hit = '1' and
		    (Term = '0' or (Term = '1' and Ready = '1') ) then
        nextState <= S_Data;
      elsif (PCI_Frame_n = '0' or PCI_Irdy_n = '0') and Hit = '1' and
		    (Term = '1' and Ready = '0') then
        nextState <= BACKOFF;
      else
        nextState <= B_BUSY;
      end if;

    when S_DATA =>
      if PCI_Frame_n = '0' and LPCI_Stop_n = '0' and (LPCI_Trdy_n = '1' or PCI_Irdy_n = '0') then
        nextState <= BACKOFF;
      elsif PCI_Frame_n = '1' and (LPCI_Trdy_n = '0' or LPCI_Stop_n = '0') then
        nextState <= TURN_AR;
      else
        nextState <= S_DATA;
      end if;


    when BACKOFF =>
      if PCI_Frame_n = '1' then
        nextState <= TURN_AR;
      else
        nextState <= BACKOFF;
      end if;

    when TURN_AR  =>
      if (PCI_Frame_n = '0' and Hit = '0') then
        nextState <= B_BUSY;
      else
        nextState <= IDLE;
      end if;

      when others =>
	    nextState <= IDLE;
    end case;
  end process nxtStProc;


  curStProc: process (PCI_Clk, PCI_Reset_n) begin
    if (PCI_Reset_n = '0') then
      currState <= Idle;
    elsif (PCI_Clk'event and PCI_Clk = '1') then
      currState <= nextState;
    end if;
  end process curStProc;


  outConProc: process (currState, Ready, T_Abort, Cmd_Write,
                       Cmd_Read, T_Abort, Term) begin

  -- Set default output assignments
  OE_Trdy_n <= '0';
  OE_Stop_n <= '0';
  OE_Devsel_n <= '0';
  OE_AD <= '0';
  LPCI_Trdy_n <= '1';
  LPCI_Stop_n <= '1';
  LPCI_Devsel_n <= '1';

    case currState is
      when S_Data =>
        if (Cmd_Read = '1') then 
		  OE_AD <= '1';
	    else
		  OE_AD <= '0';
		end if;

        if (Ready = '1' and T_Abort = '0' and (Cmd_Write = '1' or Cmd_Read = '1')) then
          LPCI_Trdy_n <= '0';
        else
          LPCI_Trdy_n <= '1';
        end if;

        if (T_Abort = '1' or Term = '1') and (Cmd_Write = '1' or Cmd_Read = '1')  then
          LPCI_Stop_n <= '0';
        else
          LPCI_Stop_n <= '1';
        end if;

        if (T_Abort = '0') then
          LPCI_Devsel_n <= '0';
        else
          LPCI_Devsel_n <= '1';
        end if;

        OE_Trdy_n <= '1';
        OE_Stop_n <= '1';
        OE_Devsel_n <= '1';

      when Backoff =>
        if (Cmd_Read = '1') then 
		  OE_AD <= '1';
	    else
		  OE_AD <= '0';
		end if;

        LPCI_Stop_n <= '0';

        OE_Trdy_n <= '1';
        OE_Stop_n <= '1';
        OE_Devsel_n <= '1';

        if (T_Abort = '0') then
          LPCI_Devsel_n <= '0';
        else
          LPCI_Devsel_n <= '1';
        end if;

      when Turn_Ar =>

        OE_Trdy_n <= '1';
        OE_Stop_n <= '1';
        OE_Devsel_n <= '1';

	  when others =>

        OE_Trdy_n <= '0';
        OE_Stop_n <= '0';
        OE_Devsel_n <= '0';
		OE_AD <= '0';
        LPCI_Trdy_n <= '1';
        LPCI_Stop_n <= '1';
        LPCI_Devsel_n <= '1';

    end case;

  end process outConProc;

  PCI_Devsel_n <= LPCI_Devsel_n;
  PCI_Trdy_n <= LPCI_Trdy_n;
  PCI_Stop_n <= LPCI_Stop_n;

end fsm;
library IEEE;
use IEEE.std_logic_1164.all;

entity pci_target is port (
  PCI_Frame_n: in std_logic;    -- PCI Frame#
  PCI_Irdy_n: in std_logic;     -- PCI Irdy#
  Hit: in std_logic;            -- Hit on address decode
  D_Done: in std_logic;         -- Device decode complete
  Term: in std_logic;           -- Terminate transaction
  Ready: in std_logic;          -- Ready to transfer data
  Cmd_Write: in std_logic;      -- Command is Write
  Cmd_Read: in std_logic;       -- Command is Read
  T_Abort: in std_logic;        -- Target error  - abort transaction
  PCI_Clk: in std_logic;        -- PCI Clock
  PCI_Reset_n: in std_logic;    -- PCI Reset#

  PCI_Devsel_n: out std_logic;  -- PCI Devsel#
  PCI_Trdy_n: out std_logic;    -- PCI Trdy#
  PCI_Stop_n: out std_logic;    -- PCI Stop#
  OE_AD: out std_logic;         -- PCI AD bus enable
  OE_Trdy_n: out std_logic;     -- PCI Trdy# enable
  OE_Stop_n: out std_logic;     -- PCI Stop# enable
  OE_Devsel_n: out std_logic    -- PCI Devsel# enable
  );
end pci_target;

architecture fsm of pci_target is

  signal LPCI_Devsel_n, LPCI_Trdy_n, LPCI_Stop_n: std_logic;

  subtype targetFsmType is std_logic_vector(2 downto 0);

  constant Idle:    targetFsmType := "000";
  constant B_Busy:  targetFsmType := "001";
  constant Backoff: targetFsmType := "011";
  constant S_Data:  targetFsmType := "110";
  constant Turn_Ar: targetFsmType := "100";

  signal currState, nextState: targetFsmType;

begin

 nxtStProc: process (currState, PCI_Frame_n, Hit, D_Done, PCI_Irdy_n, LPCI_Trdy_n,
                     LPCI_Devsel_n, LPCI_Stop_n, Term, Ready) begin
   case currState is
     when Idle  =>
      if (PCI_Frame_n = '0' and Hit = '0') then
        nextState <= B_Busy;
      else
        nextState <= Idle;
      end if;

    when B_Busy =>
      if (PCI_Frame_n ='1' and D_Done = '1') or
	     (PCI_Frame_n = '1' and D_Done = '0' and LPCI_Devsel_n = '0') then
        nextState <= Idle;
      elsif (PCI_Frame_n = '0' or PCI_Irdy_n = '0') and Hit = '1' and
		    (Term = '0' or (Term = '1' and Ready = '1') ) then
        nextState <= S_Data;
      elsif (PCI_Frame_n = '0' or PCI_Irdy_n = '0') and Hit = '1' and
		    (Term = '1' and Ready = '0') then
        nextState <= Backoff;
      else
        nextState <= B_Busy;
      end if;

    when S_Data =>
      if PCI_Frame_n = '0' and LPCI_Stop_n = '0' and
         (LPCI_Trdy_n = '1' or PCI_Irdy_n = '0') then
        nextState <= Backoff;
      elsif PCI_Frame_n = '1' and (LPCI_Trdy_n = '0' or LPCI_Stop_n = '0') then
        nextState <= Turn_Ar;
      else
        nextState <= S_Data;
      end if;


    when Backoff =>
      if PCI_Frame_n = '1' then
        nextState <= Turn_Ar;
      else
        nextState <= Backoff;
      end if;

    when Turn_Ar  =>
      if (PCI_Frame_n = '0' and Hit = '0') then
        nextState <= B_Busy;
      else
        nextState <= Idle;
      end if;

      when others =>
	    null;
    end case;
  end process nxtStProc;


  curStProc: process (PCI_Clk, PCI_Reset_n) begin
    if (PCI_Reset_n = '0') then
      currState <= Idle;
    elsif (PCI_Clk'event and PCI_Clk = '1') then
      currState <= nextState;
    end if;
  end process curStProc;


  outConProc: process (currState, Ready, T_Abort, Cmd_Write,
                       Cmd_Read, T_Abort, Term) begin
    case currState is
      when S_Data =>
        if (Cmd_Read = '1') then 
          OE_AD <= '1';
        else
          OE_AD <= '0';
        end if;

        if (Ready = '1' and T_Abort = '0' and (Cmd_Write = '1' or Cmd_Read = '1')) then
          LPCI_Trdy_n <= '0';
        else
          LPCI_Trdy_n <= '1';
        end if;

        if (T_Abort = '1' or Term = '1') and (Cmd_Write = '1' or Cmd_Read = '1')  then
          LPCI_Stop_n <= '0';
        else
          LPCI_Stop_n <= '1';
        end if;

        if (T_Abort = '0') then
          LPCI_Devsel_n <= '0';
        else
          LPCI_Devsel_n <= '1';
        end if;

        OE_Trdy_n <= '1';
        OE_Stop_n <= '1';
        OE_Devsel_n <= '1';

      when Backoff =>
        if (Cmd_Read = '1') then 
          OE_AD <= '1';
        else
          OE_AD <= '0';
        end if;

        LPCI_Stop_n <= '0';

        OE_Trdy_n <= '1';
        OE_Stop_n <= '1';
        OE_Devsel_n <= '1';

        if (T_Abort = '0') then
          LPCI_Devsel_n <= '0';
        else
          LPCI_Devsel_n <= '1';
        end if;

      when Turn_Ar =>

        OE_Trdy_n <= '1';
        OE_Stop_n <= '1';
        OE_Devsel_n <= '1';

      when others =>

        OE_Trdy_n <= '0';
        OE_Stop_n <= '0';
        OE_Devsel_n <= '0';
        OE_AD <= '0';
        LPCI_Trdy_n <= '1';
        LPCI_Stop_n <= '1';
        LPCI_Devsel_n <= '1';

    end case;

  end process outConProc;

  PCI_Devsel_n <= LPCI_Devsel_n;
  PCI_Trdy_n <= LPCI_Trdy_n;
  PCI_Stop_n <= LPCI_Stop_n;

end fsm;
library ieee;
use ieee.std_logic_1164.all;

entity test is port (
  a: in std_logic;
  z: out std_logic;
  en: in std_logic
  );
end test;

architecture simple of test is

begin

  z <= a when en = '1' else 'z';

end simple;
