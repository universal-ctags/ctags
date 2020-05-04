f <- function () {
  x <<- 1
}
f()
print (x)

y <- 1
g <- function () {
  y <<- 2
}
g()
print (y)



h <- function () {
  h0 <- function () {
    y <<- 9
  }
  h0()
}
h()
print(y)

i <- function () {
  z <- 1
  i0 <- function () {
    z <<- 12
    v <<- 13
    v ->> u
  }
  i0()
  # 12
  print(z)
}
i()

# The next `z' is undefined.
# print(z)
