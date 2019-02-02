#---------#---------#---------#---------#---------#---------#---------#---------
rm(list = ls())
library(tseries)
load('~/Desktop/marketStudies/data/randomSignals.csv')

sp <- read.csv('~/Desktop/marketStudies/data/sp1950.csv')
class(sp$Adj.Close)
head(sp)
tail(sp)

plot(sp$Adj.Close, type = 'l')

fill <- function(x, direction) {
  if (direction == 'forward') {
  	for (i in 2:length(x)) {
  	  if (is.na(x[i])) {
  	    x[i] <- x[i - 1]
  	  }
  	}
  } else if (direction == 'back') {
  	for (i in (length(x) - 1):1) {
  	  if (is.na(x[i])) {
  	    x[i] <- x[i + 1]
  	  }
  	}
  } else {
  	warning('Bad direction.')
  }
  x
}

sp$Adj.Close <- fill(sp$Adj.Close, 'forward')
sp$Adj.Close <- fill(sp$Adj.Close, 'back')
sp$Volume <- fill(sp$Volume, 'forward')
sp$Volume <- fill(sp$Volume, 'back')

x <- sp$Adj.Close
n <- length(x)


moving.deviation.signals <- function(
    x, up.q, down.q, downswing.limit, upswing.limit, peak.percent, 
    downtrend.percent, bottom.percent, uptrend.percent, 
    actions=c('sell', 'buy', 'sell', 'buy')) {
  n <- length(x)
  qs <- quantile(x, probs=c(down.q, upswing.limit, downswing.limit, up.q))
  # peak signal
  if (x[n - 2] > qs[4] & x[n - 1] <= qs[4]) {
  	return (data.frame(signal=actions[1], percent=peak.percent)) 
  }
  # init upward trend signal
  if (x[n - 2] <= qs[2] & x[n - 1] > qs[2]) {
  	return (data.frame(signal=actions[2], percent=downtrend.percent))
  }
  # init down signal
  if (x[n - 2] > qs[3] & x[n - 1] <= qs[3]) {
    return (data.frame(signal=actions[3], percent=uptrend.percent))
  }
  # bottom signal
  if (x[n - 2] <= qs[1] & x[n - 1] > qs[1]) {
    return (data.frame(signal=actions[4], percent=bottom.percent))
  }
  return (data.frame(signal='hold', percent=0))
}



sliding.window <- function(x, window, func, ...) {
  n <- length(x)
  signals <- data.frame(
    signal = factor(rep(NA, n), levels = c('buy', 'hold', 'sell')), 
    percent = numeric(n))
  for (w in window:n) {
  	signal <- func(x[(w - window + 1) : w], ...)
  	signals[w, ] <- signal
  }
  return (signals)
}


apply.signals <- function(x, signals, initial.amount, initial.percent.invested) {
  signal <- signals$signal
  percent <- signals$percent
  signal[is.na(signal)] <- 'hold'
  percent[is.na(signal)] <- 0
  n <- nrow(signals)
  x.change <- c(NA, x[2:n] / x[1:(n-1)])
  amount <- amount.invested <- amount.reserve <- rep(NA, n)
  amount[1] <- initial.amount
  amount.invested[1] <- initial.percent.invested * initial.amount
  amount.reserve[1] <- amount[1] - amount.invested[1]  
  for (day in 2:n) {
  	amount.invested[day] <- amount.invested[day - 1] * x.change[day]
  	if (signal[day] == 'buy') {  
  	  amount.transfer <- percent[day] * amount.reserve[day - 1]
  	  amount.invested[day] <- amount.invested[day - 1] + amount.transfer
  	  amount.reserve[day] <- amount.reserve[day - 1] - amount.transfer
  	} else if (signal[day] == 'sell') {
  	  amount.transfer <- percent[day] * amount.invested[day - 1]
  	  amount.invested[day] <- amount.invested[day - 1] - amount.transfer
  	  amount.reserve[day] <- amount.reserve[day - 1] + amount.transfer
  	} else {
  	  amount.reserve[day] <- amount.reserve[day - 1]
  	}
  }
  amount <- amount.invested + amount.reserve
  as.data.frame(cbind(
    signal, percent, x.change, amount.invested, amount.reserve, amount))
}


x <- sp$Adj.Close
n <- length(x)
DATE_RANGE <- 1:n #(n-2500):n

random.color <- function() {
  max.bright <- 2
  r <- runif(1)
  g <- min(runif(1), 1)
  b <- min(runif(1, 0, max.bright - r - g), 1)
  rgb(r, g, b)
}


# MF 
# stocks
best <- 91929843 
best.params <- c(
  w=10, u=0.3655, d=0.1593, dl=0.4325, ul=0.9537, p=0.0500, b=0.9500, dp=0.9500, 
  up=0.8973)


# 401(k)
#best <- 2315186
#best.params <- c(
#  w=60, u=0.9885, d=0.99, dl=0.3387, ul=0.7602, p=0, b=0.9194, dp=0.2632, 
#  up=0)

COLOR1 <- random.color()
COLOR2 <- random.color()
iters <- 30
colors <- colorRampPalette(colors=c(COLOR1, COLOR2))(iters)
#colors <- sample(colors)
# add alpha channel
for (cl in 1:length(colors)) {
  colors[cl] <- paste(colors[cl], '88', sep='')
}

plot(x[DATE_RANGE], type='l', lwd=2, log='y', ylim=c(1, 1.1 * best))
abline(h=c(x[DATE_RANGE][1], best), col=rgb(0, 0, 0, 0.5))
abline(v=0)
t <- Sys.time()
this.best <- 91929843 
abline(h=this.best, col=rgb(0, 0, 0, 0.5), lty=4)
this.best.params <- c(
  w=10, u=0.3655, d=0.1593, dl=0.4325, ul=0.9537, p=0.0500, b=0.9500, dp=0.9500, 
  up=0.8973)
  std <- 0.004
# %s of amounts that can be moved
options <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)  
for (i in 1:iters) {	
  #w <- 60
  #w  <- runif(1, 100, 500)
  w  <- round(min(max(rnorm(1, this.best.params['w'], 100 * std), 10), 250)) 
  # signal locations
  #u  <- this.best.params['u'] 
  u <- min(max(rnorm(1, this.best.params['u'],  std), 0.01), 0.99)
  #d  <- this.best.params['d'] 
  d <- min(max(rnorm(1, this.best.params['d'],  std), 0.01), 0.99)
  #dl <- this.best.params['d'] 
  dl <- min(max(rnorm(1, this.best.params['dl'], std), 0.01), 0.98) 
  #ul <- this.best.params['ul'] 
  ul <- min(max(rnorm(1, this.best.params['ul'], std), 0.01), 0.98)
  # percentages...
  p  <- min(max(rnorm(1, this.best.params['p'],  std), 0.05), 0.95)
  b  <- min(max(rnorm(1, this.best.params['b'],  std), 0.05), 0.95)
  dp <- min(max(rnorm(1, this.best.params['dp'], std), 0.05), 0.95)
  up <- min(max(rnorm(1, this.best.params['up'], std), 0.05), 0.95)
  # ...or fixed options
  #p  <- sample(options, 1)
  #b  <- sample(options, 1)
  #dp <- sample(options, 1)
  #up <- sample(options, 1)
  signals <- sliding.window(x[DATE_RANGE], 
                            window=w, 
                            func=moving.deviation.signals, 
                            up.q=u, 
                            down.q=d,
                            downswing.limit=dl,
                            upswing.limit=ul, 
                            peak.percent=p, 
                            bottom.percent=b,
                            downtrend.percent=dp, 
                            uptrend.percent=up)
  apply.deviate.signals <- apply.signals(
    x[DATE_RANGE], signals, x[DATE_RANGE[1]], 1)
  adjustment <- 0
  if (apply.deviate.signals$amount[1] != x[DATE_RANGE[1]]) {
  	adjustment <- x[DATE_RANGE[1]] - apply.deviate.signals$amount[1]
  }
  apply.deviate.signals$amount <- apply.deviate.signals$amount + adjustment
  lines(apply.deviate.signals$amount, col=colors[i])
  final <- apply.deviate.signals$amount[length(apply.deviate.signals$amount)]
  #if (final >= 0.5 * best) {
  #	cat('\nWithin 25%: (', final, ')', w, u, d, dl, ul, p, b, dp, up, '\n')
  #}
  if (final > this.best) {
  	cat(
  	  '\nBest so far this round: (', final, ')', w, u, d, dl, ul, p, b, dp, up, 
  	  '\n')
  	this.best <- final
  	this.best.params <- c(w=w, u=u, d=d, dl=dl, ul=ul, p=p, b=b, dp=dp, up=up)
  }
  if (final > best) {
  	cat('\nNew Best!\n')
    best.params <- c(w, u, d, dl, ul, p, b, dp, up)
    best <- final
  }
  cat(100 * i / iters, '% complete\r', sep='')
}
Sys.time() - t
best
this.best.params
best.params








n <- length(x[DATE_RANGE])
qs <- quantile(x, probs = c(d, ul, dl, u))
# peak signal
if (x[n - 2] > qs[4] & x[n - 1] <= qs[4]) {
  return (data.frame(signal = 'sell', percent = peak.percent)) 
}

qs <- quantile(x, probs = c(d, ul, dl, u))
(x[n - 2] > qs[4] & x[n - 1] <= qs[4])
# Create a collection of signals from random window and quantile values
# (This will take a long time-- about <<time>> per iteration)
iters <- 500
#random.signals <- list()
for (i in 1:iters) {
  if (i %% 10 == 0) { cat('iteration:', i, Sys.time(), '\n') }
  window <- sample(20:500, 1)
  margin <- sample(10^seq(-3, -1.301, length = 100), 1)
  signals <- sliding.window(
    sp$Adj.Close, window, identify.signals, margin, 1 - margin, F)
  random.signals[[i]] <- list(
    window = window, margin = margin, signals = signals)
}





###save.image('~/Desktop/marketStudies/data/###')