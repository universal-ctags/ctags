x <- 1
x <- 2

f <- function () {
  x <- 3
}

# Next one `f' is captured because `f` is function
# though `f' is already defined in the global scope.
f <- function () {
  x <- 4
}

f <- function (x) {
  x <- 5
}

f <- function (x) {
  g <- function () {
    x <- 6
  }
}

f <- function (x) {
  g <- function () {
    x <- 6
  }
  # Following `g' are not captured because `g' is functionVar
  # and already defined.
  g <- function (x) {
    x <- 7
  }
  g <- function () {
    x <- 8
  }
  g <- function () {
    y <- 0
  }
}
