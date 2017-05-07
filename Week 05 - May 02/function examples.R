rm(list = ls())
# [] - for indexing objects
# () - arguments for calling functions
# {} - collect lines belonging to a function

isBetween <- function(x, a, b) {
  gt.a <- x > a
  lt.b <- x < b
  result <- gt.a & lt.b
  return(result)
}
isBetween(x = 6, a = 3, b = 10)


isBetween <- function(x, a, b) {
  gt.a <- x > a
  lt.b <- x < b
  result <- gt.a & lt.b
  result
}
isBetween(x = 6, a = 3, b = 10)

# return value is value of last line - 'return' function is redundant here
isBetween <- function(x, a, b) {
  gt.a <- x > a
  lt.b <- x < b
  gt.a & lt.b
}
isBetween(x = 6, a = 3, b = 10)

# one liners don't need curly braces
isBetween <- function(x, a, b) x > a & x < b
isBetween(x = 6, a = 3, b = 10)


isBetween <- function(x, a, b) {
  gt.a <- x > a
  lt.b <- x < b
  print("hello")
  invisible(gt.a & lt.b)
}
isBetween(x = 6, a = 3, b = 10)
res <- isBetween(x = 6, a = 3, b = 10)



spp.codes <- read.csv("tblCodeSpecies.csv", stringsAsFactors = FALSE)


genus <- spp.codes$GENUS
species <- spp.codes$SPECIES


abbrev <- function(genus, species) {
  g <- substr(genus, 1, 1)
  spp <- substr(species, 1, 3)
  paste(g, spp)
}
abbrev(spp.codes$GENUS, spp.codes$SPECIES)

abbrev(spp.codes$SPECIES, genus = spp.codes$GENUS)

# default is function of other arguments
abbrev <- function(genus, species, num.spp = max(3, min(nchar(species)))) {
  g <- substr(genus, 1, 1)
  spp <- substr(species, 1, num.spp)
  paste(g, spp)
}
abbrev(spp.codes$GENUS[1:4], spp.codes$SPECIES[1:4])


abbrev <- function(genus, species, num.g = 1, num.spp = 3) {
  g <- substr(genus, 1, num.g)
  spp <- substr(species, 1, num.spp)
  paste(g, spp)
}
abbrev(s = spp.codes$SPECIES, spp.codes$GENUS, num.s = 4)


abbrev <- function(genus, species, num.g = 1, num.spp = 3, ...) {
  g <- substr(genus, 1, num.g)
  spp <- substr(species, 1, num.spp)
  paste(g, spp)
}
abbrev(spp.codes$GENUS, spp.codes$SPECIES, collapse = ", ")


myMean <- function(x, ...) {
  res <- mean(x, ...)
  paste("Your answer is:", res, ...)
}
myMean(c(NA, 1:10), na.rm = TRUE, sep = "-")



ctd <- read.csv("ctd.csv", stringsAsFactors = FALSE)

tapply(ctd$temp, ctd$station, mean, na.rm = TRUE)


meanMed <- function(x) {
  n <- sum(!is.na(x))
  mean.x <- mean(x, na.rm = TRUE)
  med.x <- median(x, na.rm = TRUE)
  c(n = n, mean = mean.x, median = med.x)
}
meanMed(ctd$temp)

tapply(ctd$temp, ctd$station, meanMed)


tapply(ctd$temp, ctd$station, function(x) {
  n <- sum(!is.na(x))
  mean.x <- mean(x, na.rm = TRUE)
  med.x <- median(x, na.rm = TRUE)
  c(n = n, mean = mean.x, median = med.x)
})


sapply(split(spp.codes, spp.codes$FAMILY), function(x) length(unique(x$GENUS)))

fam.df <- split(spp.codes, spp.codes$FAMILY)


ctd.boot <- lapply(101:110, function(i) {
  ran.rows <- sample(1:nrow(ctd), rep = TRUE)
  ctd[ran.rows, ]
})

sapply(ctd.boot, function(x) mean(x$temp, na.rm = TRUE))

