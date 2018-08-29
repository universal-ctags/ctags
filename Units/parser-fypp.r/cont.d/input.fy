#! Taken from https://fypp.readthedocs.io/en/stable/fypp.html#preprocessor-language
@:assertEqual(size(coords, dim=2), &
    & size(atomtypes))
program helloworld
print *, "Hello, world."
end program helloworld
