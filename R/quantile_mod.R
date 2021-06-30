rm(list=ls())
setwd('~/Desktop/marketStudies')

sp <- read.csv('data/sp1950.csv')

parse.date <- function(date.str) {
  bits <- unlist(strsplit(as.character(date.str), '/'))
  year <- as.numeric(bits[3])
  if (year >= 50) { year <- year + 1900 }
  else { year <- year + 2000 }
  sprintf('%d-%s-%s', year, bits[1], bits[2])
}

sp$Date <- sapply(sp$Date, parse.date)
sp$Date <- as.Date(sp$Date, format='%Y-%m-%d')
sp <- sp[, c(1, 6)]
head(sp)

add.windowed.quantile <- function(df, window, from.field) {
  x <- df[, from.field]
  field <- sprintf('w%d', window)
  df[, field] <- numeric(nrow(df))
  for (i in 1:nrow(df)) {
  	if (i < window) {
  	  df[i, field] <- NA
  	} else {
  	  windat <- x[(i - window + 1):(i)]
  	  df[i, field] <- order(windat)[window] / window
  	}
  }
  df
}


for (window in c(5, 10, 20, 40, 80, 160, 320)) {
  cat('starting window', window, '...\n')
  sp <- add.windowed.quantile(sp, window, 'Adj.Close')
}


add.diff.col <- function(df, lag, field) {
  x <- df[, field]
  name <- sprintf('%s.diff%d', field, lag)
  df[, name] <- c(rep(NA, lag), diff(x, lag=lag))
  df
}

for (lag in c(1, 2, 5, 10, 20, 40, 80, 160, 320)) {
  sp <- add.diff.col(sp, lag=lag, 'Adj.Close')
}

for (field in names(sp)[grepl('w\\d', names(sp))]) {
  sp <- add.diff.col(sp, lag=1, field)
}


add.forecast <- function(df, n.days, field='Adj.Close') {
  x <- c(df[(n.days + 1):nrow(df), field], rep(NA, n.days + 1))
  x <- x[1:nrow(df)]
  df[, sprintf('forecast%d', n.days)] <- x / df[, field]
  df
}

for (forecast in c(1, 5, 10, 20, 40)) {
  sp <- add.forecast(sp, forecast)
}

head(sp, 10)

par(mfrow=c(2, 1))
plot(sp$Date, sp$Adj.Close, type='l', log='y')
plot(sp$Date, sp$forecast40, type='l')

quenouille <- function(x, nu) {
  sign(x) * sqrt(nu - 1) * asinh(sqrt(x^2 / nu))
}

inv.quenouille <- function(y, nu) {
  sign(y) * sqrt(nu * (sinh(y / sqrt(nu - 1)))^2)
}

TEST <- -3.337
inv.quenouille(quenouille(TEST, 2), 2)

n <- nrow(sp) # [(n - 5000):n]
sp$forecast40T <- quenouille(sp$forecast40, 2)


mod40 <- lm(
  forecast40T ~ 
    Adj.Close 
    + (w5 + w10 + w20 + w40 + w80 + w160 + w320)^2
    + Adj.Close.diff1
    + Adj.Close.diff2
    + Adj.Close.diff5
    + Adj.Close.diff10
    + Adj.Close.diff20
    + Adj.Close.diff40
    + Adj.Close.diff80
    + Adj.Close.diff160
    + Adj.Close.diff320
    + w5.diff1 * w5
    + w10.diff1 * w10
    + w20.diff1 * w20
    + w40.diff1 * w40
    + w80.diff1 * w80
    + w160.diff1 * w160
    + w320.diff1 * w320,
  data=sp)
mod40 <- step(mod40)
summary(mod40)
par(mfrow=c(2, 2))
plot(mod40)

sp$pred40 <- inv.quenouille(c(rep(NA, 320), predict(mod40), rep(NA, 40)), 2)


mod20 <- lm(
  quenouille(forecast20, 2) ~ 
    Adj.Close 
    + (w5 + w10 + w20 + w40 + w80 + w160 + w320)^2
    + Adj.Close.diff1
    + Adj.Close.diff2
    + Adj.Close.diff5
    + Adj.Close.diff10
    + Adj.Close.diff20
    + Adj.Close.diff40
    + Adj.Close.diff80
    + Adj.Close.diff160
    + Adj.Close.diff320
    + w5.diff1 * w5
    + w10.diff1 * w10
    + w20.diff1 * w20
    + w40.diff1 * w40
    + w80.diff1 * w80
    + w160.diff1 * w160
    + w320.diff1 * w320,
  data=sp)
mod20 <- step(mod20)
summary(mod20)
par(mfrow=c(2, 2))
plot(mod20)

sp$pred20 <- inv.quenouille(c(rep(NA, 320), predict(mod20), rep(NA, 20)), 2)






par(mfrow=c(1, 1))
START <- as.Date('2008-01-01') 
sub <- subset(sp, Date >= START)
plot(sub$Date, sub$forecast20, type='l', log='y')
abline(h=1, col='grey')
lines(sub$Date, sub$pred20, col=2)

COL <- rgb(0, 0, 0, 0.1)
plot(sp$forecast20 ~ sp$pred20, pch=16, col=COL)
abline(h=1, col='grey')
abline(v=1, col='grey')
abline(0, 1, col=2)
abline(lm(sp$forecast40 ~ sp$pred40), col=4)