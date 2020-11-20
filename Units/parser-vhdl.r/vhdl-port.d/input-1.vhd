-- Taken from https://github.com/universal-ctags/ctags/issues/2678
-- commented by @pidgeon777.
component COMPONENT_A is
  generic (
    GENERIC_C : integer := value;
  );
  port (
    PORT_C : in std_logic
  );
end component;

component COMPONENT_B is
  generic (
    GENERIC_C : integer := value;
  );
  port (
    PORT_C : in std_logic
  );
end component COMPONENT_B;

component COMPONENT_C is
  generic (
    GENERIC_C : integer := value;
  );
  port (
    PORT_C : in std_logic
  );
end component;
