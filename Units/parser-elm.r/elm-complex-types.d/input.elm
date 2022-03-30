-- A type with a record spec

type A = ACons { x : Float, y : Float }

-- A type with more complex spec

type B
    = B1Cons
      { x : Float
      , y : Float
      }
    | B2Cons String Integer
    | B3Cons

-- With a lack of spacing

type C=CCons{x:Float,y:Float}

-- Tuples

type D = DCons (String, Float, {x:String, y:Float})

-- Empty records and tuples

type E
    = E1Cons {}
    | E2Cons ()

-- With functions as type specifications

type F
    = F1Cons (String -> Int)
    | F2Cons (Float -> String -> (String -> {x:Float, y:Float}))
    | F3Cons

-- With vertical bars within the type

type G a
    = G1Cons a Int
    | G2Cons { a | name : String}
