setClass("C", representation(x="numeric"))

setGeneric("run", function(object) 1)

setMethod("run", c("C"), definition = function (object) {
		 object@x <- 4
		 3 + object@x
})

setGeneric("run2", function(object) 1)
setMethod("run2", c("C"), function (object) {
		 object@x <- 4
		 3 + object@x
})

setClass ("D", contains = "C")
setGeneric("run3", function(i, j) 1)
setMethod("run3", signature("numeric", "numeric"),
		  function (i, j) { i + j })

c <- new ("C", x = 2)
print(run(c))
print(run('a'))
