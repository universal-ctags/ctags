--
-- Taken from rtl/riverlib/core/stacktrbuf.vhd of https://github.com/sergeykhbr/riscv_vhdl
--
-----------------------------------------------------------------------------
--! @file
--! @copyright Copyright 2017 GNSS Sensor Ltd. All right reserved.
--! @author    Sergey Khabarov - sergeykhbr@gmail.com
--! @brief     Stack trace buffer on hardware level.
------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
library commonlib;
use commonlib.types_common.all;

entity StackTraceBuffer is 
  generic (
    abits : integer := 5;
    dbits : integer := 64
  );
  port (
    i_clk   : in std_logic;
    i_raddr : in std_logic_vector(abits-1 downto 0);
    o_rdata : out std_logic_vector(dbits-1 downto 0);
    i_we    : in std_logic;
    i_waddr : in std_logic_vector(abits-1 downto 0);
    i_wdata : in std_logic_vector(dbits-1 downto 0)
  );
end; 
 
architecture arch_StackTraceBuffer of StackTraceBuffer is

  type ram_type is array ((2**abits)-1 downto 0) of std_logic_vector (dbits-1 downto 0);
  signal stackbuf    : ram_type;
  signal raddr       : std_logic_vector(abits-1 downto 0);

begin

  -- registers:
  regs : process(i_clk) begin 
    if rising_edge(i_clk) then 
      if i_we = '1' then
        stackbuf(conv_integer(i_waddr)) <= i_wdata; 
      end if;
      raddr <= i_raddr;
    end if;
  end process;
  
  o_rdata <= stackbuf(conv_integer(raddr));

end;
