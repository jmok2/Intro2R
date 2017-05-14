rm(list = ls())
ctd <- read.csv("ctd.csv", stringsAsFactors = FALSE)

st1 <- subset(ctd, station == "Station.1" & sample_date == "2010-01-06")


# plot temperature / depth
plot(st1$temp, st1$depth)
temp <- st1$temp
plot(temp, st1$depth)

# change type of scatterplot to line
plot(st1$temp, st1$depth, type = "l")

# sort data frame
st1 <- st1[order(st1$depth), ]
plot(st1$temp, st1$depth, type = "l")

# line/point scatterplot
plot(st1$temp, st1$depth, type = "b")

# colors
plot(st1$temp, st1$depth, col = "palegreen4")

# points
plot(st1$temp, st1$depth, col = "red", pch = 21)
# bg only good for pch = 21:25
plot(st1$temp, st1$depth, col = "red", bg = "green", pch = 19)
# change size with cex
plot(st1$temp, st1$depth, col = "red", pch = 19, cex = 0.5)

lt30 <- ifelse(st1$depth < 30, 1, 2)
plot(st1$temp, st1$depth, col = c("red", "green")[lt30])

depth.col <- ifelse(st1$depth < 30, "red", "green")
plot(st1$temp, st1$depth, col = depth.col)


st1.2 <- subset(ctd, station == "Station.1" &
                  sample_date %in% c("2010-01-06", "2010-07-13"))
st1.2$month <- ifelse(st1.2$sample_date == "2010-01-06", "Jan", "Jul")

month.col <- c(Jan = "blue", Feb = "lightblue",  Jul = "red")
plot(st1.2$temp, st1.2$depth, col = month.col[st1.2$month])

month.shape <- c(Jan = 8, Jul = 4)
plot(st1.2$temp, st1.2$depth, pch = month.shape[st1.2$month],
     col = month.col[st1.2$month])

# lines
plot(st1$temp, st1$depth, type = "l")
plot(st1$temp, st1$depth, type = "l", lty = "dashed")
plot(st1$temp, st1$depth, type = "l", lty = "dotted")
plot(st1$temp, st1$depth, type = "l", lwd = 4)
plot(st1$temp, st1$depth, type = "l", lwd = 4, col = "red")



# titles, labels
plot(st1$temp, st1$depth, main = "main title", sub = "sub title")
plot(st1$temp, st1$depth, xlab = "Temperature", ylab = "Depth")

plot(st1$temp, st1$depth, ann = FALSE)
plot(st1$temp, st1$depth, axes = FALSE)
plot(st1$temp, st1$depth, axes = FALSE, ann = FALSE)

# x,y limits
plot(st1$temp, st1$depth, xlim = c(12, 16), ylim = c(40, 60))
plot(st1$temp, st1$depth, ylim = rev(range(st1$depth)))

# axes
plot(
  st1$temp, st1$depth,
  ylim = rev(range(st1$depth)),
  type = "l", ann = FALSE, axes = FALSE
)
axis(1)
axis(1, at = c(13.5, 15))

# set xlimit to be range of "pretty"
xlim <- range(pretty(st1$temp))
plot(
  st1$temp, st1$depth,
  xlim = xlim,
  ylim = rev(range(st1$depth)),
  type = "l", ann = FALSE, axes = FALSE
)
# add min and max to standard pretty ticks
x.ticks <- c(pretty(xlim), range(st1$temp))
axis(1, at = x.ticks)
# las re-orients axis labels
axis(2, las = 1)
# draw l-shaped box
box(bty = "l")


plot(st1$temp, st1$depth, ylim = rev(range(st1$depth)))
points(c(14.5, 13.5), c(5, 20), pch = 19, col = c("red", "green"))
lines(c(14.5, 13.5), c(5, 20), col = "purple")
text(mean(range(st1$temp)), mean(range(st1$depth)), "Hello")

# two temperature/depth profiles on one figure
st1.jan <- subset(ctd, station == "Station.1" & sample_date == "2010-01-06")
st1.jul <- subset(ctd, station == "Station.1" & sample_date == "2010-07-13")
st1.jan <- st1.jan[order(st1.jan$depth), ]
st1.jul <- st1.jul[order(st1.jul$depth), ]


xlim <- range(pretty(c(st1.jan$temp, st1.jul$temp)))
ylim <- rev(range(pretty(c(st1.jan$depth, st1.jul$depth))))
plot.new()
plot.window(xlim = xlim, ylim = ylim)
lines(st1.jan$temp, st1.jan$depth, col = "blue")
lines(st1.jul$temp, st1.jul$depth, col = "red")
axis(1)
axis(2, las = 1)


plot(st1.jan$temp, st1.jan$depth, type = "l", xlim = xlim, ylim = ylim, col = "blue")
lines(st1.jul$temp, st1.jul$depth, col = "red")


# margins
op <- par(mar = c(5, 5, 2, 2) + 0.1, oma = c(3, 3, 3, 3), las = 0)
plot(st1.jan$temp, st1.jan$depth, type = "l", ann = F)
mtext("Temperature", side = 1, line = 3)
mtext("Depth", side = 2, line = 2, outer = TRUE)
mtext("Title", side = 3, line = 2, cex = 4)
mtext("a)", side = 3, adj = 0)
par(op)


# multiple panels

# mfrow/mfcol
op <- par(mar = c(6, 6, 6, 6), oma = c(3, 3, 3, 3), mfrow = c(2, 2))
hist(ctd$temp, ylab = "")
box(col = "red")
box("figure", col = "blue")
hist(ctd$salinity, ylab = "")
box(col = "red")
box("figure", col = "blue")
hist(ctd$dox, ylab = "")
box(col = "red")
box("figure", col = "blue")
hist(ctd$ph, ylab = "")
box(col = "red")
box("figure", col = "blue")
mtext("Frequency", line = 1.5, side = 2, outer = TRUE)
par(op)

# layout
lm <- matrix(c(1, 1, 2, 3), nrow = 2)
layout(lm, widths = c(2, 1), heights = c(3, 1))
hist(ctd$temp, ylab = "")
hist(ctd$salinity, ylab = "")
hist(ctd$dox, ylab = "")

# saving plots
pdf("plot example.pdf", width = 8.5, height = 11)
hist(ctd$temp, ylab = "")
hist(ctd$salinity, ylab = "")
hist(ctd$dox, ylab = "")
dev.off()
