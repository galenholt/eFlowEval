# Testing against matlab

sizen <- 100
# create some test matrices
a <- rnbinom(sizen, size = 1, prob = 0.15)
b <- matrix(rnbinom(sizen*sizen, size = 1, prob = 0.01), nrow = sizen)
c <- a %*% b

# Allocating d certainly takes longer here than in matlab...
d <- array(data = 0, dim = c(sizen, sizen, sizen))

#  time
  # R requires a function to time with system.time
timefunc <- function() {
  for (i in 1:sizen) {
    a = a*b[ ,i] + b[i, ]
    b = b*17 + 10
    c = a %*% b
    d[ , ,i] = c
  }
  return(d)
}

system.time(d <- timefunc())
# 53 seconds in R, 50 in matlab. Call R ok... And this is without any Rcpp work, which could likely speed up some of the heavy computation
save(d, file = 'testout.rdata')
