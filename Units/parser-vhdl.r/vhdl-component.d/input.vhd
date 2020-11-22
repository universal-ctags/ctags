-- Taken from a comment https://github.com/universal-ctags/ctags/issues/2678
-- submitted by @pidgeon777
library ieee;
use ieee.std_logic_1164.all;

entity ENTITY_TOP is
  generic (
    GEN : integer := 0
  );
  port (
    INP : in std_logic
  );
end entity;

architecture arch of ENTITY_TOP is
  signal sig : std_logic := '0';

  component ENTITY_1
    generic (
      GEN : integer := 0
    );
    port (
      INP : in std_logic
    );
  end component;

  component ENTITY_2
    generic (
      GEN : integer := 0
    );
    port (
      INP : in std_logic
    );
  end component;

begin

  ENTITY_1_i : ENTITY_1
  generic map(
    GEN => 0
  )
  port map(
    INP => '0'
  );

  ENTITY_2_i : ENTITY_2
  generic map(
    GEN => 0
  )
  port map(
    INP => '0'
  );

end architecture;
