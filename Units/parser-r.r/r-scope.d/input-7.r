({
	a <- function ()
	{
		1
	}

	b <- function (x)
	  a()

	c <- function (x)
	  a() * a()
})

a() + b() + c()
