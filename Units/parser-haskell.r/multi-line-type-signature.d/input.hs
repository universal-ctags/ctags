module Foo () where


thing
    :: App m
    => Int
    -> m Int
thing x = pure x

