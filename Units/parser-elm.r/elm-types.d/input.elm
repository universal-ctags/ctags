type Apple = Cox | Braeburn

-- Parameterised types

type Box a = Cardboard a | Wooden

type Clog a b = Dutch | English

-- Constructors with types

type DType
    = D1Cons { x : String, y:Maybe Int}
    | D2Cons Float Float Clog
