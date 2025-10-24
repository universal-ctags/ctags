# Test case for R6 parser null pointer regression
# This triggers the error path in r6ReadRightSideSymbol that was causing
# tokenDelete to be called with NULL pointers

# Case 1: R6:: followed by something other than R6Class
TestCase1 <- R6::SomethingElse("TestCase1")

# Case 2: Incomplete R6 namespace reference
TestCase2 <- R6::

# Case 3: R6 followed by :: but with syntax error
TestCase3 <- R6::R6Clas("TestCase3")

