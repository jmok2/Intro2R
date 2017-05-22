rm(list = ls())

# distributions

dunif(1, 0, 10)
punif(0.95, 0, 100)
qunif(0.95, 0, 100)
runif(100, 0, 100)

sample(0:100, 100, replace = TRUE)

runif(5, 0, 100)

set.seed(10)
runif(5, 0, 100)

# simulated data

dox <- runif(100, 2, 10)
ph <- 7.5 + (dox * 0.09) + rnorm(100, 0, 0.2)
plot(dox, ph)

# linear modeling

pd.form <- ph ~ dox
pd.lm <- lm(pd.form)

slope <- lm(pd.form)$coefficients["dox"]

## how does slope estimate change with change in standard deviation?

sd.vec <- seq(0.01, 2, by = 0.01)
delta.slope <- sapply(sd.vec, function(sd) {
  dox <- runif(100, 2, 10)
  ph <- 7.5 + (dox * 0.09) + rnorm(100, 0, sd)
  slope <- lm(ph ~ dox)$coefficients["dox"]
  slope - 0.09
})
plot(sd.vec, delta.slope)


## plotting the fit

plot(dox, ph)
lines(dox, pd.lm$fitted.values, col = "red")

plot(dox, ph)
abline(pd.lm)
abline(v = 6)
abline(h = c(7.4, 7.9, 8.5))


plot(dox, ph)
abline(v = seq(2, 10, by = 2))
abline(h = seq(7.5, 8.5, by = 0.5))


plot(dox, ph)
abline(pd.lm, col = "blue")
abline(coef = c(7.5, 0.09), col = "red", lty = "dashed")

## prediction

new.df <- data.frame(dox = runif(5, 5, 15))
predict(pd.lm, new.df, interval = "confidence")

new.df$est.ph <- predict(pd.lm, new.df)


# categorical modelling

ctd <- read.csv("ctd.csv", stringsAsFactors = F)
surf <- subset(ctd, depth == 1 & station %in% paste0("Station.", 1:5))

# boxplots
boxplot(temp ~ station, surf)

# lm model
temp.lm <- lm(temp ~ station, surf)

# ANOVA
temp.aov <- aov(temp ~ station, surf)
summary(temp.aov)
# ...or with a linear model
anova(temp.lm)

# pairwise comparisons
TukeyHSD(temp.aov)


# simulated growth data
# gompertz: length ~ lab * e(k * (1 - e(-g * age)))

sim.growth.func <- function(age.range, lab, k, g, std.dev, sample.size) {
  ages <- runif(sample.size, age.range[1], age.range[2])
  expected.length <- lab * exp(k * (1 - exp(-g * ages)))
  length.err <- rnorm(sample.size, 0, std.dev)
  as.data.frame(cbind(age = ages, length = expected.length + length.err))
}

growth.df <- sim.growth.func(
  age.range = c(0, 65),
  lab = 10,
  k = 2,
  g = 0.25,
  std.dev = 5,
  sample.size = 200
)
plot(length ~ age, growth.df)

gr.form <- length ~ lab * exp(k * (1 - exp(-g * age)))
gr.nl <- nls(gr.form, growth.df, start = c(lab = 15, k = 1, g = 0.5))

# plot fit
grow.fit <- data.frame(
  age = seq(
    min(growth.df$age),
    max(growth.df$age),
    length.out = 100
  )
)
grow.fit$length <- predict(gr.nl, grow.fit)
plot(length ~ age, growth.df)
lines(grow.fit$age, grow.fit$length, col = "red", lty = "dashed", lwd = 2)


# statistical tests

# t-test
surf15 <- subset(surf, station %in% c("Station.1", "Station.5"))
boxplot(temp ~ station, surf15)

surf15.t <- t.test(temp ~ station, surf15)
surf15.t


# chi-squared test
ctd$year <- as.numeric(substr(ctd$sample_date, 1, 4))
ctd$mo <- as.numeric(substr(ctd$sample_date, 6, 7))

yr.freq <- table(ctd$year)
yr.chisq <- chisq.test(yr.freq)

yr.mo.freq <- table(ctd$mo, ctd$year)
yr.mo.chisq <- chisq.test(yr.mo.freq)

yr.mo.chisq <- chisq.test(yr.mo.freq, sim = T)


# bootstrap

obs.mean <- mean(surf15$temp)
boot.mean <- sapply(1:1000, function(x) {
  i <- sample(1:nrow(surf15), nrow(surf15), replace = TRUE)
  boot.i <- surf15[i, ]
  mean(boot.i$temp)
})


# permutation test

obs <- diff(tapply(surf15$temp, surf15$station, mean))

perm.df <- surf15

perm.diff <- sapply(1:1000, function(x) {
  perm.df$station <- sample(perm.df$station)
  diff(tapply(perm.df$temp, perm.df$station, mean))
})
plot(density(perm.diff))
