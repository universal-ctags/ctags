-- Taken from a comment https://github.com/universal-ctags/ctags/issues/2678
-- submitted by @pidgeon777
library ieee;
use ieee.std_logic_1164.all;

entity ENTITY_1 is
  generic (
    GEN : integer := 0
  );
  port (
    INP : in std_logic
  );
end entity;

architecture arch of ENTITY_1 is

  signal sig : std_logic := '0';

begin
end architecture;
