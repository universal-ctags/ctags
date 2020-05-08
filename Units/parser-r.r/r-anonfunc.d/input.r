# This can be a test case for S4 object system in the future.

setClass("C", representation(x="numeric"))

setGeneric("run", function(object) 1)

setMethod("run", c("C"), definition = function (object) {
		 object@x <- 4
		 3 + object@x
})

c <- new ("C", x = 2)
print(run(c))
print(run('a'))
