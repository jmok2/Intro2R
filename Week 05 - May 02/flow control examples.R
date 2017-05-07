rm(list = ls())

spp.codes <- read.csv("tblCodeSpecies.csv", stringsAsFactors = FALSE)

abbrev <- function(genus, species, num.g = 1, num.spp = 3, ...) {
  if(num.g < 1) {
    cat("num.g must be 1 or greater\n")
    num.g <- 1
  }
  if(num.spp < 1) num.spp <- 1
  g <- substr(genus, 1, num.g)
  spp <- substr(species, 1, num.spp)
  paste(g, spp, ...)
}

abbrev(spp.codes$GENUS, spp.codes$SPECIES, num.g = 0)


abbrev <- function(genus, species, type = "short") {
  if(type == "short") {
    g <- substr(genus, 1, 1)
    spp <- substr(species, 1, 3)
    paste(g, spp, sep = ". ")
  } else {
    g <- substr(genus, 1, 1)
    paste(g, species, sep = ". ")
  }
}
abbrev(spp.codes$GENUS, spp.codes$SPECIES, type = "SHORT")


abbrev <- function(genus, species) {
  g <- substr(genus, 1, 1)
  spp <- substr(species, 1, 3)
  short <- paste(g, spp, sep = ". ")
  long <- paste(g, species, sep = ". ")
  ifelse(genus == "Stenella", short, long)
}
abbrev(spp.codes$GENUS, spp.codes$SPECIES)

if(cond) {

} else if(cond) {

} else if(cond) {

}



abbrev <- function(genus, species, type) {
  g <- substr(genus, 1, 1)
  spp <- if(type == "short") substr(species, 1, 3) else species
  switch(type,
         short = paste(g, spp, sep = ". "),
         medium = paste(g, spp, sep = ". "),
         long = paste(genus, spp)
  )
}
abbrev(spp.codes$GENUS, spp.codes$SPECIES, type = "long")




fib <- function(n) {
  x <- vector("numeric", 10)
  for(i in 1:n) {
    if(i == 1) x[i] <- 0
    if(i == 2) x[i] <- 1
    if(i > 2) x[i] <- x[i - 1] + x[i - 2]
  }
  x
}
fib(10)


for(x in split(spp.codes, spp.codes$FAMILY)) {
  fam <- unique(x$FAMILY)
  write.csv(x, file = paste(fam, "spp.codes.csv"))
}

for(x in spp.codes) print(class(x))


fib <- function(n) {
  x <- vector("numeric", n)
  for(i in 1:n) {
    if(i == 1) next
    if(i == 2) x[i] <- 1
    if(i > 2) x[i] <- x[i - 1] + x[i - 2]
    if(x[i] > 120) break
  }
  x
}
fib(30)


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


res <- fib("14")
