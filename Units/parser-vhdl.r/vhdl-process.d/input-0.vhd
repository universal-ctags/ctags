--
-- Taken from rtl/riverlib/core/stacktrbuf.vhd of https://github.com/sergeykhbr/riscv_vhdl
-- with modifications.
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

entity StackTraceBuffer0 is 
  generic (
    abits0 : integer := 5;
    dbits0 : integer := 64
  );
  port (
    i_clk0   : in std_logic;
    i_raddr0 : in std_logic_vector(abits0-1 downto 0);
    o_rdata0 : out std_logic_vector(dbits0-1 downto 0);
    i_we0    : in std_logic;
    i_waddr0 : in std_logic_vector(abits0-1 downto 0);
    i_wdata0 : in std_logic_vector(dbits0-1 downto 0)
  );
end; 
 
architecture arch_StackTraceBuffer0 of StackTraceBuffer0 is

  type ram_type0 is array ((2**abits0)-1 downto 0) of std_logic_vector (dbits0-1 downto 0);
  signal stackbuf0    : ram_type0;
  signal raddr0       : std_logic_vector(abits0-1 downto 0);

begin

  -- registers:
  process(i_clk0) begin 
    if rising_edge(i_clk0) then 
      if i_we0 = '1' then
        stackbuf0(conv_integer(i_waddr0)) <= i_wdata0; 
      end if;
      raddr0 <= i_raddr0;
    end if;
  end process;
  
  o_rdata0 <= stackbuf0(conv_integer(raddr0));

end;
