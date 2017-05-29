rm(list = ls())

# extract station id and date from filename
# return data.frame of parsed info
parseFilenames <- function(fnames) {
  fnames <- gsub("\\.csv", "", fnames)
  f.split <- strsplit(fnames, " ")
  df <- as.data.frame(do.call(rbind, f.split))
  colnames(df) <- c("station", "date")

  # extract station number
  df$station <- as.numeric(gsub("Station\\.", "", df$station))

  # format date
  dt <- as.POSIXlt(df$date, format = "%Y-%m-%d")
  df$date <- as.POSIXct(dt)
  df$year <- dt$year + 1900
  df$month <- month.abb[dt$mon + 1]

  df
}


# load station data
fnames <- dir("ctd data", pattern = "Station\\.", full.names = TRUE)
ctd <- lapply(fnames, function(f) {
  df <- read.csv(f)
  st.dt <- parseFilenames(basename(f))
  cbind(st.dt, df)
})
ctd <- do.call(rbind, ctd)
ctd$month <- droplevels(factor(ctd$month, levels = month.abb))
ctd <- ctd[order(ctd$station, ctd$date, ctd$depth), ]
print(str(ctd))



# how many casts per station in August?
aug <- subset(ctd, month == "Aug")
aug$cast <- paste(aug$station, aug$date)
num.casts <- tapply(aug$cast, aug$station, function(x) length(unique(x)))
print(num.casts)



# summarize surface values at each station
smrzValue <- function(x) {
  x <- na.omit(x)
  c(
    n = length(x),
    min = min(x),
    lci = unname(quantile(x, 0.025)),
    mean = mean(x),
    median = median(x),
    uci = unname(quantile(x, 0.975)),
    max = max(x)
  )
}

# extract surface (1m) data
srfc <- subset(aug, depth == 1)
# create vector of ctd measurements
vals <- c("temp", "salinity", "dox", "ph", "pct_light")
# compute summary for each value
srfc.smry <- sapply(vals, function(x) {
  smry <- tapply(srfc[, x], srfc$station, smrzValue)
  do.call(rbind, smry)
}, simplify = FALSE)
# extract means
srfc.mean <- do.call(cbind, sapply(srfc.smry, function(x) x[, "mean"], simplify = FALSE))
srfc.mean <- as.data.frame(srfc.mean)
print(head(srfc.mean))


# are there outlier casts? (casts with 3 or more unusual surface measurements)
outliers.by.val <- sapply(vals, function(x) {
  lt.uci <- srfc[, x] < quantile(srfc[, x], 0.025, na.rm = TRUE)
  gt.uci <- srfc[, x] > quantile(srfc[, x], 0.975, na.rm = TRUE)
  i <- which(lt.uci | gt.uci)
}, simplify = FALSE)
outlier.freq <- table(unlist(outliers.by.val))
is.outlier <- outlier.freq[outlier.freq >= 3]
outlier.casts <- srfc[as.numeric(names(is.outlier)), "cast"]
print(subset(srfc, cast %in% outlier.casts))


# plot temperature trace summaries
plotTraces <- function(x, df, xlim, ylim) {
  plot.new()
  plot.window(xlim = xlim, ylim = ylim)
  for(st in df$station) {
    st.df <- subset(df, station == st)
    lines(st.df[, x], st.df$depth, col = "gray90")
  }
  med.x <- tapply(df[, x], df$depth, median, na.rm = TRUE)
  lines(med.x, as.numeric(names(med.x)), lwd = 1.5)
  box()
}
# extract 2016 data
ctd2016 <- droplevels(subset(ctd, year == 2016))
xlim <- range(ctd2016$temp)
ylim <- rev(range(ctd2016$depth))
op <- par(mar = c(0, 0, 0, 0), oma = c(5, 5, 0, 0))
lm <- matrix(1:4, nrow = 2, byrow = TRUE)
layout(lm)
for(mo in levels(ctd2016$month)) {
  df <- subset(ctd2016, month == mo)
  plotTraces("temp", df, xlim, ylim)
  i <- which(levels(ctd2016$month) == mo)
  if(i %in% 3:4) axis(1)
  if(i %in% c(1, 3)) axis(2, las = 1)
  text(min(xlim), min(ylim), mo, adj = c(0, 1), cex = 1.3, font = 2)
}
mtext("Temperature", side = 1, line = 3, outer = TRUE)
mtext("Depth", side = 2, line = 3, outer = TRUE)
layout(matrix(1))
par(op)


# plot station SST for 2016
pos <- read.csv(file.path("ctd data", "ctd positions.csv"))
srfc.mean$station <- as.numeric(rownames(srfc.mean))
srfc.mean <- merge(srfc.mean, pos, by.x = "station", by.y = "st.id", all.x = TRUE)

op <- par(mar = c(4, 3, 1, 1), oma = c(0, 0, 0 ,0))
xlim <- c(-117.5, -117.25)
ylim <- c(32.8, 33.1)
plot.new()
map("worldHires", xlim = xlim, ylim = ylim)
tempPal <- colorRampPalette(c("lightblue", "red"))
temp.col <- tempPal(20)[as.numeric(cut(srfc.mean$temp, breaks = 20))]
points(srfc.mean$longitude, srfc.mean$latitude, pch = 21, cex = 3, bg = temp.col)
text(pos$longitude, pos$latitude, pos$st.id)
axis(1)
axis(2, las = 1)
box()
par(op)


# Monthly temperature across years
op <- par(mar = c(4, 4, 1, 1), oma = c(0, 0, 0 ,0))
srfc <- subset(ctd, depth == 1)
month.date <- as.POSIXlt(srfc$date)
month.date$mday <- 1
srfc$date <- as.POSIXct(month.date)
x.tick <- as.POSIXct(paste0(2010:2017, "-01-01"))
xlim <- range(x.tick)
ylim <- range(srfc$temp)
plot.new()
plot.window(xlim = xlim, ylim = ylim)
for(st in unique(srfc$station)) {
  st.df <- subset(srfc, station == st)
  lines(st.df$date, st.df$temp, col = "gray90")
}
med.temp <- tapply(srfc$temp, srfc$date, median, na.rm = TRUE)
dt <- as.POSIXct(names(med.temp))
lines(dt, med.temp, lwd = 1.5)
axis.POSIXct(1, x.tick, at = x.tick)
axis(2, las = 1)
mtext("Year", 1, line = 2)
mtext("Temperature", 2, line = 3)
box()
par(op)

