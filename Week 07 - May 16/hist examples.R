rm(list = ls())

ctd <- read.csv("ctd.csv", stringsAsFactors = F)

par(mar = c(3, 3, 3, 3))
plot(0, 0)

par(mar = c(8, 8, 8, 8))
plot(0, 0)

plot(0, 0, col = "black")

op <- par(mar = c(6, 6, 6, 6))
plot(0, 0)
par(op)


# histograms
hist(ctd$temp)
hist(ctd$temp, main = "", xlab = "Temperature")
hist(ctd$temp, breaks = c(8, 12, 20, 21, 22, 23, 24, 25))

x <- hist(ctd$temp)
par(mar = c(5, 4, 2, 2) + 0.1)

x <- hist(ctd$temp)
text(x$mids, x$counts, x$counts)


# density plot

temp.dens <- density(ctd$temp)
plot(temp.dens)
plot(temp.dens, main = "", xlab = "Temperature")

plot(density(ctd$temp))
