-- Should be able to parse multiple exposed items in a module declaration

module A3Module exposing
    ( map, foldl
    , Maybe, Possibly
    , Result(..)
    , MyList(Empty), Tree(Node, Value, Special) )

