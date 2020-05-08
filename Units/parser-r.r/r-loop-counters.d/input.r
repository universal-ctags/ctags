# defining an operator
`%*%` <- function (v, p) {
    r <- 1
    if (p == 1) {
        r <- v
    } else {
        for (i in 1:p) {
            r <- r * v
        }
    }
    r
}

print (c(2 %*% 10, 2%*%2, 2%*%3)[1:2])

# j introduced as a global var.
for (j in 1:3) {
    print (j)
}
print (j)
