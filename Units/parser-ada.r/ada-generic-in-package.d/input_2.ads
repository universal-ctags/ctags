package input_2 is

  type Generator is null record;
  generic
    type Unsigned is mod <>;
    type Real is digits <>;
    with function Random (G: Generator) return Unsigned is <>;
  function Random (G: Generator) return Real;

end input_2;