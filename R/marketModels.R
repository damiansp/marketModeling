#---------#---------#---------#---------#---------#---------#---------#---------
rm(list = ls())
library(tseries)
load('~/Desktop/marketStudies/data/randomSignals.csv')

#get.hist.quote('^gspc', start = '1976-01-01', quote = c('Adj.Close, Volume'))

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

# A simple regression-line model that takes an N-day window, and buys when at a 
# given quantile below the trend and sells when a given quantile above.
# Test a variety of window and quantiles to find optimum
# Also test % to put in/out at each signal
regression.signals <- function(
  x, down.q, up.q, sell.percent = 1.0, buy.percent = 1.0, plot = T) {

  n <- length(x)
  periods <- 1:n
  
  trend <- lm(log(x) ~ periods)
  fitted <- predict(trend)
  resids <- log(x) - fitted
  signals <- quantile(resids, probs = c(down.q, up.q))
  today <- resids[length(resids)]
    
  if (plot) {
    par(mfrow = c(2, 1))
    par(mar = c(2, 4, 0.2, 0.2))
    plot(log(x) ~ periods, 
         type = 'l', 
         ylim = c(0.9 * min(log(x)), 1.1 * max(log(x))))
    abline(trend, col = 4)
    lines(fitted + signals[1], col = 2)
    lines(fitted + signals[2], col = 2)
  
    plot(resids ~ periods, type = 'l')
    abline(h = 0, col = 'grey')
    abline(h = signals, col = 2)
  }

  if (today <= signals[1]) {
  	return (data.frame(signal = 'buy', percent = buy.percent))
  } else if (today >= signals[2]) {
  	return (data.frame(signal = 'sell', percent = sell.percent))
  } else {
  	return (data.frame(signal = 'hold', percent = 1.0))
  }
}

# Test
regression.signals(sp$Adj.Close, 0.05, 0.95)


moving.deviation.signals <- function(x, 
                                     up.q = 0.99, 
                                     down.q = 0.01, 
                                     downswing.limit = 0.52,
                                     upswing.limit = 0.1, 
                                     peak.percent = 0.1, 
                                     downtrend.percent = 1.0, 
                                     bottom.percent = 1.0, 
                                     uptrend.percent = 1.0) {
  n <- length(x)
  qs <- quantile(x, probs = c(down.q, upswing.limit, downswing.limit, up.q))
  # peak signal
  if (x[n - 2] > qs[4] & x[n - 1] <= qs[4]) {
  	return (data.frame(signal = 'sell', percent = peak.percent)) 
  }
  # init upward trend signal
  if (x[n - 2] <= qs[2] & x[n - 1] > qs[2]) {
  	return (data.frame(signal = 'buy', percent = downtrend.percent))
  }
  # init down signal
  if (x[n - 2] > qs[3] & x[n - 1] <= qs[3]) {
    return (data.frame(signal = 'sell', percent = uptrend.percent))
  }
  # bottom signal
  if (x[n - 2] <= qs[1] & x[n - 1] > qs[1]) {
    return (data.frame(signal = 'buy', percent = bottom.percent))
  }
  return (data.frame(signal = 'hold', percent = 0))
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

#signals <- sliding.window(sp$Adj.Close, 20, regression.signals, 0.05, 0.95, F)
#DATE_RANGE <- 1:n
#x <- sp$Adj.Close[DATE_RANGE]
#signals <- sliding.window(sp$Adj.Close[DATE_RANGE], 
#                          window = 9, 
#                          func = moving.deviation.signals, 
#                          up.q = 0.99, 
#                          down.q = 0.01,
#                          downswing.limit = 0.5,
#                          upswing.limit = 0.5, 
#                          peak.percent = 0.0, 
#                          downtrend.percent = 1.0, 
#                          bottom.percent = 0.0, 
#                          uptrend.percent = 1.0)

#plot(log(x), 
#     type = 'l', 
#     xlim = c(n - 1000, n), 
#     ylim = range(log(x)[(n - 1000) : n]))
#plot(log(x), type = 'l')
#buy.idx <- which(signals$signal == 'buy')
#sell.idx <- which(signals$signal == 'sell')
#abline(v = buy.idx, col = rgb(0, 1, 0, 0.5))
#abline(v = sell.idx, col = rgb(1, 0, 0, 0.5))


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

apply.deviate.signals <- apply.signals(x, signals, x[1], 1)
plot(x, type='l', lwd=2, log='y', ylim=range(x, apply.deviate.signals$amount))
#buy.idx <- which(apply.deviate.signals$signal == 1)
#sell.idx <- which(apply.deviate.signals$signal == 3)
#abline(v = buy.idx, col = rgb(0, 1, 0, 0.2))
#abline(v = sell.idx, col = rgb(1, 0, 0, 0.2))
lines(apply.deviate.signals$amount, col = 8)

x <- sp$Adj.Close
n <- length(x)
DATE_RANGE <- 1:n

random.color <- function() {
  max.bright <- 2
  r <- runif(1)
  g <- min(runif(1), 1)
  b <- min(runif(1, 0, max.bright - r - g), 1)
  rgb(r, g, b)
}

# stocks
#best <- 1.963608e+13
#best <- x[n]
#best.params <- c(
# w   u        d       dl      ul      p       b        dp      up
#  6,  0.2479,  0.9402, 0.8232, 0.9597, 0.0000, 0.8538,  0.4802, 0.0000)
#  15  0.9856   0.99    0.1029  0.3331  0       0.9427   1.       0
#  15  0.8317   0.99    0.99    0.01    0       0        0.5762   0.7556
# 401(k)
best <- 11001.04
best.params <- c(
# w     u       d       dl      ul      p       b       dp      up
  133,  0.1649, 0.0100, 0.0123, 0.0100, 0.0000, 0.0261, 0.4540, 0.0000)
  134   0.1303  0.01    0.0114  0.0461  0       0.7980  0.4940  0 

COLOR1 <- random.color()
COLOR2 <- random.color()
iters <- 100
colors <- colorRampPalette(colors=c(COLOR1, COLOR2))(iters)
# add alpha channel
for (cl in 1:length(colors)) {
  colors[cl] <- paste(colors[cl], '88', sep='')
}

plot(x[DATE_RANGE], type='l', lwd=2, log='y', ylim=c(1, 1.1 * best))
abline(h=c(x[DATE_RANGE][1], best), col=rgb(0, 0, 0, 0.5))
abline(v=0)
t <- Sys.time()
for (i in 1:iters) {	
  #w  <- max(runif(1, 6, 200), 6)
  w  <- max(rnorm(1, 133, 4), 125) # 200 is full # n3xt
  u  <- min(max(rnorm(1, 0.1649, 0.02), 0.01), 0.99)
  d  <- min(max(rnorm(1, 0.0100, 0.02), 0.01), 0.99)
  dl <- min(max(rnorm(1, 0.0123, 0.02), 0.01), 0.99)
  ul <- min(max(rnorm(1, 0.0100, 0.02), 0.01), 0.99)
  p  <- min(max(rnorm(1, 0.0000, 0.02), 0.00), 1.00)
  b  <- min(max(rnorm(1, 0.0251, 0.64), 0.00), 1.00)
  dp <- min(max(rnorm(1, 0.4540, 0.02), 0.00), 1.00)
  up <- min(max(rnorm(1, 0.0000, 0.02), 0.00), 1.00)
  signals <- sliding.window(x[DATE_RANGE], 
                            window = w, 
                            func = moving.deviation.signals, 
                            up.q = u, 
                            down.q = d,
                            downswing.limit = dl,
                            upswing.limit = ul, 
                            peak.percent = p, 
                            bottom.percent = b,
                            downtrend.percent = dp, 
                            uptrend.percent = up)
  apply.deviate.signals <- apply.signals(
    x[DATE_RANGE], signals, x[1], 1)
  lines(apply.deviate.signals$amount, col = colors[i])
  final <- apply.deviate.signals$amount[length(apply.deviate.signals$amount)]
  if (final >= 0.5 * best) {
  	cat('Within 25%: ', w, u, d, dl, ul, p, b, dp, up, '\n')
  }
  if (final > best) {
  	cat('New Best!\n')
    best.params <- c(w, u, d, dl, ul, p, b, dp, up)
    best <- final
  }
}
Sys.time() - t
best
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