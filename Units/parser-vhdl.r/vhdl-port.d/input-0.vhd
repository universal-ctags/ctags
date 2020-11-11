-- Taken from https://github.com/universal-ctags/ctags/issues/2678
-- commented by @pidgeon777.
entity ENTITY_A is
  generic (
    GENERIC_C : integer := value;
  );
  port (
    PORT_C : in std_logic
  );
end entity;

entity ENTITY_B is
  generic (
    GENERIC_C : integer := value;
  );
  port (
    PORT_C : in std_logic
  );
end entity;
