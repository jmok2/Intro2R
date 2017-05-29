x1 <- 1:5
x2 <- 11:15
x3 <- 21:25

x <- list(x1, x2, x3)

# the "normal" function call
rbind(x[[1]], x[[2]], x[[3]])
# do.call takes the elements of x and makes them arguments to the function rbind
do.call(rbind, x)

do.call(cbind, x)

do.call(c, x)


sample.args <- list(replace = TRUE, x = letters[1:5], size = 20)
do.call(sample, sample.args)


ctd <- read.csv("ctd.csv", stringsAsFactors=F)

# function takes a whole data.frame that is assumed to be from one station
smrzStation <- function(df) {
  mean.temp <- tapply(df$temp, df$depth, mean, na.rm = TRUE)
  mean.sal <- tapply(df$salinity, df$depth, mean, na.rm = TRUE)
  mean.ph <- tapply(df$ph, df$depth, mean, na.rm = TRUE)
  result <- data.frame(temp = mean.temp, sal = mean.sal, ph = mean.ph)
  result$depth <- sort(unique(df$depth))
  result$station <- unique(df$station)
  result
}

st.smry <- lapply(split(ctd, ctd$station), smrzStation)

st.smry.df <- do.call(rbind, st.smry)

# different version where we iterate over vector of station names and
#  pass entire ctd data.frame in to 'df'
smrzStation <- function(st, df) {
  df <- subset(df, station == st)
  mean.temp <- tapply(df$temp, df$depth, mean, na.rm = TRUE)
  mean.sal <- tapply(df$salinity, df$depth, mean, na.rm = TRUE)
  mean.ph <- tapply(df$ph, df$depth, mean, na.rm = TRUE)
  result <- data.frame(temp = mean.temp, sal = mean.sal, ph = mean.ph)
  result$depth <- sort(unique(df$depth))
  result$station <- st
  result
}

st.smry <- lapply(unique(ctd$station), smrzStation, df = ctd)

st.smry.df <- do.call(rbind, st.smry)


# example reading in separate files to one data.frame
fnames <- dir(pattern = "fish_")
fish.df <- do.call(rbind, lapply(fnames, function(f) {
  df <- read.csv(f)
  df$fname <- f
  df$meas.num <- 1:nrow(df)
  df
}))

# sorting numbers treated as characters
x <- sample(1:200, 50)
x <- paste("label_", x, sep = "")

ctd <- ctd[order(ctd$temp), ]
ord <- order(ctd$ph)

x[order(nchar(x), x)]

ctd <- ctd[order(nchar(ctd$station), ctd$station), ]
