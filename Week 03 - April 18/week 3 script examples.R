rm(list = ls())

# object summary

x <- matrix(1:12, nrow = 3)

# show contents of an object
print(x)

# write text to screen
cat("Loading data")
cat("Next line", "Two lines", sep = "\n")

# summarize columns or distributions
summary(x)

ctd <- read.csv("ctd.csv", stringsAsFactors = FALSE)
summary(ctd)


# Missing Data (NAs)

x <- NA

# test for NA
is.na(x)
is.na(c(5, NA, 10, 1, NA))

# remove NAs
x <- sample(c(1:5, NA), 20, replace = TRUE)
has.nas <- is.na(x)
x.no.nas <- x[!has.nas]

# na.omit
x.omit <- na.omit(x)

# no missing values for salinity
sal.na <- is.na(ctd$salinity)
ctd.sal.no.miss <- ctd[!sal.na, ]

# no missing values for salinity or dox
good.row <- !is.na(ctd$salinity) & !is.na(ctd$dox)
ctd.good <- ctd[good.row, ]

# complete.cases <- returns logical vector of complete cases (rows)
i <- complete.cases(ctd)
ctd.complete <- ctd[complete.cases(ctd), ]

# complete cases for salinity and dox
ctd.complete <- ctd[complete.cases(ctd[, c("salinity", "dox")]), ]

# Math summaries
x <- sample(1:100, 20, replace = TRUE)
sum(x)
prod(x)
min(x)
max(x)
range(x)
mean(x)
median(x)
var(x)
sd(x)
diff(x)
diff(x, lag = 3)

quantile(x, probs = c(0.05, 0.5, 0.9, 0.95))


# Discrete values

x <- sample(letters, 100, replace = TRUE)

# unique values
unique(x)

# duplicates
duplicated(x)

x[!duplicated(x)]

# count unique values
freq <- table(x)

x1 <- sample(letters[1:5], 100, replace = TRUE)
x2 <- sample(letters[11:15], 100, replace = TRUE)
x12.freq <- table(x1, x2)

# what stations are present in CTD data
unique(ctd$station)

# number of unique stations
length(unique(ctd$station))

# number of unique dates
length(unique(ctd$sample_date))

# number of unique casts (station * date)
st.dt.dup <- duplicated(ctd[, c("station", "sample_date")])
sum(!st.dt.dup)

# Data Selection

# do values in one vector exist in another vector?
test.vec <- letters[1:10]
c("a", "r", "b") %in% test.vec

# which values are TRUE
which(c(TRUE, FALSE, FALSE, TRUE))
which(letters %in% c("a", "f", "z"))

i <- which(ctd$temp < 14.40)
ctd.lt.med <- ctd[which(ctd$temp < 14.4), ]

# which values are min and max
which.min(ctd$temp)
ctd[which.min(ctd$temp), ]
which.max(ctd$temp)

# test if TRUE is present
any(is.na(ctd$temp))
any(is.na(ctd$salinity))

# test if all are TRUE
all(is.na(ctd$temp))
all(!is.na(ctd$temp))

all(c(T, F, T))
all(c(T, T, T))


# sorting
x <- sample(letters, 100, replace = T)

# reverse a vector
rev(x)

# sort a vector
sort(x)
sort(x, decreasing = TRUE)

st <- unique(ctd$station)


# "sort" a data.frame with order
# returns indices as if they were sorted
order(x)

temp.ord <- order(ctd$temp)
ctd.temp.ord <- ctd[temp.ord, ]

ctd.ord <- order(ctd$temp, ctd$pct_light)
new.ctd <- ctd[ctd.ord, ]

ctd <- ctd[order(ctd$station, ctd$sample_date), ]
