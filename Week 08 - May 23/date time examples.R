# POSIXct POSIXlt

dt.lt <- as.POSIXlt("2011/08/23 6:05")

dt.ct <- as.POSIXct("2011/08/23 6:05")

# character to date/time
dt <- strptime(ctd$sample_date, format = "%Y-%m-%d")
ctd$date <- as.POSIXct(dt)

# numeric to date
as.POSIXlt(10000, origin = "2017-05-23")

as.POSIXlt(0, tz = "GMT", origin = "2017-05-23")



# date to character
now <- Sys.time()
timestamp()

now.ch <- strftime(now, format = "%Y-%m-%d %H hours & %M minutes")

format(now, "Year: %Y, Month: %m, Day: %d at %H%M", tz = "EST")



# elapsed time
xmas <- as.POSIXct("2017-12-25")
wait.time <- xmas - Sys.time()

difftime(xmas, Sys.time(), units = "hours")

Sys.time() + as.difftime(2, units = "days")


x <- sample(1:12, 25, replace = T)

x.m <- month.abb[x]

x.f <- factor(x.m, levels = month.abb)


myFunc <- function() {
  stopifnot(require(wmwmw))
  print(isBetween(4, c(3,8)))
  print("hello")
}
myFunc()



x <- 1
fname <- file.path("test folder", "x.rdata")
save(x, file = fname)