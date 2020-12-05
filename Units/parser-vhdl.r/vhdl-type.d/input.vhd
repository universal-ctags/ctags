--
-- Taken from rtl/misclib/types_misc.vhd of https://github.com/sergeykhbr/riscv_vhdl
--
--!
--! Copyright 2018 Sergey Khabarov, sergeykhbr@gmail.com
--!
--! Licensed under the Apache License, Version 2.0 (the "License");
--! you may not use this file except in compliance with the License.
--! You may obtain a copy of the License at
--!
--!     http://www.apache.org/licenses/LICENSE-2.0
--! Unless required by applicable law or agreed to in writing, software
--! distributed under the License is distributed on an "AS IS" BASIS,
--! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--! See the License for the specific language governing permissions and
--! limitations under the License.
--!

--! Standard library.
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library commonlib;
use commonlib.types_common.all;
--! Technology definition library.
library techmap;
use techmap.gencomp.all;
--! CPU, System Bus and common peripheries library.
library ambalib;
use ambalib.types_amba4.all;
use ambalib.types_bus0.all;

--! @brief   Declaration of components visible on SoC top level.
package types_misc is

--! @defgroup irq_id_group AXI4 interrupt generic IDs.
--! @ingroup axi4_config_generic_group
--! @details Unique indentificator of the interrupt pin also used
--!          as an index in the interrupts bus.
--! @{

--! Zero interrupt index must be unused.
constant CFG_IRQ_UNUSED         : integer := 0;
--! UART_A interrupt pin.
constant CFG_IRQ_UART1          : integer := 1;
--! Ethernet MAC interrupt pin.
constant CFG_IRQ_ETHMAC         : integer := 2;
--! GP Timers interrupt pin
constant CFG_IRQ_GPTIMERS       : integer := 3;
--! GNSS Engine IRQ pin that generates 1 msec pulses.
constant CFG_IRQ_GNSSENGINE     : integer := 4;
--! Total number of used interrupts in a system
constant CFG_IRQ_TOTAL          : integer := 5;
--! @}

--! @brief SOC global reset former.
--! @details This module produces output reset signal in a case if
--!          button 'Reset' was pushed or PLL isn't a 'lock' state.
--! param[in]  inSysReset Button generated signal
--! param[in]  inSysClk Clock from the PLL. Bus clock.
--! param[out] outReset Output reset signal with active 'High' (1 = reset).
component reset_global
port (
  inSysReset  : in std_ulogic;
  inSysClk    : in std_ulogic;
  outReset    : out std_ulogic );
end component;


--! Boot ROM with AXI4 interface declaration.
component axi4_rom is
generic (
    memtech  : integer := inferred;
    async_reset : boolean := false;
    xaddr    : integer := 0;
    xmask    : integer := 16#fffff#;
    sim_hexfile : string
  );
port (
    clk  : in std_logic;
    nrst : in std_logic;
    cfg  : out axi4_slave_config_type;
    i    : in  axi4_slave_in_type;
    o    : out axi4_slave_out_type
  );
end component; 

--! Internal RAM with AXI4 interface declaration.
component axi4_sram is
  generic (
    memtech  : integer := inferred;
    async_reset : boolean := false;
    xaddr    : integer := 0;
    xmask    : integer := 16#fffff#;
    abits    : integer := 17;
    init_file : string := "" -- only for 'inferred'
  );
  port (
    clk  : in std_logic;
    nrst : in std_logic;
    cfg  : out axi4_slave_config_type;
    i    : in  axi4_slave_in_type;
    o    : out axi4_slave_out_type
  );
end component; 

--! AXI4 to SPI brdige for external Flash IC Micron M25AA1024
type spi_in_type is record
    SDI : std_logic;
end record;

type spi_out_type is record
    SDO : std_logic;
    SCK : std_logic;
    nCS : std_logic;
    nWP : std_logic;
    nHOLD : std_logic;
    RESET : std_logic;
end record;

constant spi_out_none : spi_out_type := (
  '0', '0', '1', '1', '1', '0'
);

component axi4_flashspi is
  generic (
    async_reset : boolean := false;
    xaddr   : integer := 0;
    xmask   : integer := 16#fffff#;
    wait_while_write : boolean := true  -- hold AXI bus response until end of write cycle
  );
  port (
    clk    : in  std_logic;
    nrst   : in  std_logic;
    cfg    : out axi4_slave_config_type;
    i_spi  : in  spi_in_type;
    o_spi  : out spi_out_type;
    i_axi  : in  axi4_slave_in_type;
    o_axi  : out axi4_slave_out_type  );
end component; 

--! @brief AXI4 GPIO controller
component axi4_gpio is
  generic (
    async_reset : boolean := false;
    xaddr    : integer := 0;
    xmask    : integer := 16#fffff#;
    xirq     : integer := 0;
    width    : integer := 12
  );
  port (
    clk  : in std_logic;
    nrst : in std_logic;
    cfg  : out axi4_slave_config_type;
    i    : in  axi4_slave_in_type;
    o    : out axi4_slave_out_type;
    i_gpio : in std_logic_vector(width-1 downto 0);
    o_gpio : out std_logic_vector(width-1 downto 0);
    o_gpio_dir : out std_logic_vector(width-1 downto 0)
  );
end component; 

type uart_in_type is record
  rd   	: std_ulogic;
  cts   : std_ulogic;
end record;

type uart_out_type is record
  td   	: std_ulogic;
  rts   : std_ulogic;
end record;

--! UART with the AXI4 interface declaration.
component axi4_uart is
  generic (
    async_reset : boolean := false;
    xaddr   : integer := 0;
    xmask   : integer := 16#fffff#;
    xirq    : integer := 0;
    fifosz  : integer := 16
  );
  port (
    clk    : in  std_logic;
    nrst   : in  std_logic;
    cfg    : out axi4_slave_config_type;
    i_uart : in  uart_in_type;
    o_uart : out uart_out_type;
    i_axi  : in  axi4_slave_in_type;
    o_axi  : out axi4_slave_out_type;
    o_irq  : out std_logic);
end component;

--! Test Access Point via UART (debug access)
component uart_tap is
  port (
    nrst     : in std_logic;
    clk      : in std_logic;
    i_uart   : in  uart_in_type;
    o_uart   : out uart_out_type;
    i_msti   : in axi4_master_in_type;
    o_msto   : out axi4_master_out_type;
    o_mstcfg : out axi4_master_config_type
  );
end component; 

-- JTAG TAP
component tap_jtag is
  generic (
    ainst  : integer range 0 to 255 := 2;
    dinst  : integer range 0 to 255 := 3);
  port (
    nrst  : in std_logic;
    clk  : in std_logic;
    i_tck   : in std_logic;   -- in: Test Clock
    i_ntrst   : in std_logic;   -- in: 
    i_tms   : in std_logic;   -- in: Test Mode State
    i_tdi   : in std_logic;   -- in: Test Data Input
    o_tdo   : out std_logic;   -- out: Test Data Output
    o_jtag_vref : out std_logic;
    i_msti   : in axi4_master_in_type;
    o_msto   : out axi4_master_out_type;
    o_mstcfg : out axi4_master_config_type
    );
end component;


--! @brief   Interrupt controller with the AXI4 interface declaration.
--! @details To rise interrupt on certain CPU HostIO interface is used.
component axi4_irqctrl is
  generic (
    async_reset : boolean := false;
    xaddr    : integer := 0;
    xmask    : integer := 16#fffff#
  );
  port 
 (
    clk    : in std_logic;
    nrst   : in std_logic;
    i_irqs : in std_logic_vector(CFG_IRQ_TOTAL-1 downto 1);
    o_cfg  : out axi4_slave_config_type;
    i_axi  : in axi4_slave_in_type;
    o_axi  : out axi4_slave_out_type;
    o_irq_meip : out std_logic
  );
  end component;

  --! @brief   General Purpose Timers with the AXI interface.
  --! @details This module provides high precision counter and
  --!          generic number of GP timers.
  component axi4_gptimers is
  generic (
    async_reset : boolean := false;
    xaddr   : integer := 0;
    xmask   : integer := 16#fffff#;
    xirq    : integer := 0;
    tmr_total  : integer := 2
  );
  port (
    clk    : in  std_logic;
    nrst   : in  std_logic;
    cfg    : out axi4_slave_config_type;
    i_axi  : in  axi4_slave_in_type;
    o_axi  : out axi4_slave_out_type;
    o_pwm : out std_logic_vector(tmr_total-1 downto 0);
    o_irq  : out std_logic
  );
  end component; 

--! @brief   Plug-n-Play support module with AXI4 interface declaration.
--! @details Each device in a system hase to implements sideband signal
--!          structure 'nasti_slave_config_type' that allows FW to
--!          detect Hardware configuration in a run-time.
--! @todo Implements PnP signals for all Masters devices.
component axi4_pnp is
  generic (
    async_reset : boolean := false;
    xaddr   : integer := 0;
    xmask   : integer := 16#fffff#;
    tech    : integer := 0;
    hw_id   : std_logic_vector(31 downto 0) := X"20170101"
  );
  port (
    sys_clk : in  std_logic;
    adc_clk : in  std_logic;
    nrst   : in  std_logic;
    mstcfg : in  bus0_xmst_cfg_vector;
    slvcfg : in  bus0_xslv_cfg_vector;
    cfg    : out  axi4_slave_config_type;
    i      : in  axi4_slave_in_type;
    o      : out axi4_slave_out_type;
    -- OTP Timing control
    i_otp_busy : in std_logic;
    o_otp_cfg_rsetup : out std_logic_vector(3 downto 0);
    o_otp_cfg_wadrsetup : out std_logic_vector(3 downto 0);
    o_otp_cfg_wactive : out std_logic_vector(31 downto 0);
    o_otp_cfg_whold : out std_logic_vector(3 downto 0)
  );
end component; 

component axi4_otp is
  generic (
    async_reset : boolean := false;
    xaddr   : integer := 0;
    xmask   : integer := 16#ffffe#
  );
  port (
    clk    : in  std_logic;
    nrst   : in  std_logic;
    cfg    : out axi4_slave_config_type;
    i_axi  : in  axi4_slave_in_type;
    o_axi  : out axi4_slave_out_type;
    o_otp_we     : out  std_ulogic;
    o_otp_re     : out  std_ulogic;
    o_otp_addr   : out std_logic_vector(11 downto 0);
    o_otp_wdata  : out std_logic_vector(15 downto 0);
    i_otp_rdata  : in std_logic_vector(15 downto 0);
    i_cfg_rsetup : in std_logic_vector(3 downto 0);
    i_cfg_wadrsetup : in std_logic_vector(3 downto 0);
    i_cfg_wactive : in std_logic_vector(31 downto 0);
    i_cfg_whold : in std_logic_vector(3 downto 0);
    o_busy : out std_logic
  );
end component; 

end; -- package declaration
