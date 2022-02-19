-- Simple function with a signature

funcA : Int -> Float -> Float
funcA a = 1

-- Signature over multiple lines

funcB :
    String ->
    Float
funcB b =
    b + 1

-- Function with a record in the signature

funcC : Int -> {c: String}
funcC c = 3

-- Function with a tuple in the signature

funcD : (Int, Int) -> String

funcD = 4

-- Functions in the signature

funcE : Float -> (Int -> Int -> Int -> String)

funcE = 5

-- Comments in the signature

funcF : -- Comment
    String -> {- Old ->-} Int {-- End--}
funcF = 6

-- Dotted types in the signature

funcG : G.Int -> G.Other
funcG = 7

-- Functions with type annotations should provide scope

funcH : Int -> Int
funcH h =
    let
        h2 = 34
    in
    h + h2
