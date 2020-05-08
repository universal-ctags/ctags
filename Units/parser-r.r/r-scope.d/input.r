c00 = 1
c01 <- 2
c02 <<- 3

f0 <- function() {
   r0 <- c00 + c01 + c02
}

f1 <- function(a1) {
   r1 = c00 + c01 + c02 + a1
}

f2 <- function(a20, a21) {
   r2 <<- c00 + c01 + c02 + a20 + a21
}

f4 <- function() c02 +
   c01 +
   c02

f5 <- function(a5) c(1,
1,
r5 <- 3,
a5)

f5(a5 = 4)
f5((c50 = 4))
f5(c51 <- 4)
f5(c52 <<- 4)
