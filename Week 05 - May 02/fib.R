df <- read.csv()

fib <- function(n) {
  # calculates fibonacci series for n values

  # check inputs
  if(class(n) != "numeric") stop("'n' must be numeric")
  if(n < 1) stop("'n' can't be less than 1")
  if(n > 100) warning("this is a big number")

  # create holding vector
  x <- vector("numeric", n)
  for(i in 1:n) {
    if(i == 1) next
    if(i == 2) x[i] <- 1
    if(i > 2) x[i] <- x[i - 1] + x[i - 2]
  }
  x
}