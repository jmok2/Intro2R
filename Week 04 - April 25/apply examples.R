rm(list = ls())

x <- lapply(1:5, sample)
x <- lapply(c(4, 5, 5, 3), sample)

x.mean <- lapply(x, mean)

# sapply - tries to simplify

x.mean <- sapply(x, mean)

x.s <- sapply(c(4, 5, 5, 3), sample)
x.s2 <- sapply(rep(5, 6), sample)


ctd <- read.csv("ctd.csv")

# what are the column classes?
sapply(ctd, class)

# the means of value columns?
sapply(ctd[, 3:8], mean, na.rm = TRUE)

rng <- sapply(ctd[, 3:8], range, na.rm = TRUE)


mat <- matrix(1:24, nrow = 4)
rowSums(mat)
apply(mat, 1, sum)

apply(mat, 2, mean)
colMeans(mat)

apply(mat, 2, var)
apply(mat, 2, range)
apply(mat[, 2:3], 1, range)


# tapply - apply function to grouped data

x <- sample(1:100, 100, replace = TRUE)
grp <- sample(letters[1:5], 100, replace = TRUE)

tapply(x, grp, mean)

tapply(ctd$temp, ctd$station, mean)

st.temp <- tapply(ctd$station, ctd$station, length)

cast.mean.temp <- tapply(ctd$temp, list(ctd$station, ctd$sample_date), mean)


# aggregate - grouped data for every column in data.frame or matrix
st.medians <- aggregate(ctd[, 3:8], list(station = ctd$station), median, na.rm = TRUE)

st.medians <- aggregate(ctd[, 3:8], list(station = ctd$station, date = ctd$sample_date), median, na.rm = TRUE)

st.range <- aggregate(ctd[, 3:8], list(station = ctd$station), range, na.rm = TRUE)


# grouped summaries of data.frames
st.rows <- by(ctd, ctd$station, nrow)

cast.rows <- by(ctd, list(station = ctd$station, date = ctd$sample_date), nrow)


# mapply - iteratively apply multiple vectors to arguments in a function

x <- mapply(sample, x = 5:10, size = c(20, 4), replace = TRUE)

x <- mapply(sample, x = 5:10, size = 5, replace = TRUE)



# split - create of vectors, matrices, or data.frames based on groups

ctd.st <- split(ctd, ctd$station)

lapply(ctd.st, nrow)

ctd.cast <- split(ctd, list(ctd$station, ctd$sample_date))

df.nrow <- sapply(ctd.cast, nrow)
to.keep <- df.nrow > 0
ctd.cast.keepers <- ctd.cast[to.keep]

