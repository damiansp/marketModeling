library(tseries)
get.hist.quote('^gspc', start = '1976-01-01', quote = c('Adj.Close, Volume'))

sp <- read.csv('~/Desktop/marketStudies/data/sp1976.csv')
head(sp)
tail(sp)

plot(sp$Adj.Close, type = 'l')

# A simple regression-line model that takes an N-day window, and buys when at a 
# given quantile below the trend and sells when a given quantile above.
# Test a variety of window and quantiles to find optimum
# Also test % to put in/out at each signal
identify.signals <- function(x, down.q, up.q, plot = T) {
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
  	return ('buy')
  } else if (today >= signals[2]) {
  	return ('sell')
  } else {
  	return ('hold')
  }
}

# Test
identify.signals(sp$Adj.Close, 0.05, 0.95)

sliding.window <- function(x, window, func, ...) {
  n <- length(x)
  signals <- factor(rep(NA, n), levels = c('buy', 'hold', 'sell'))
  
  for (w in window:n) {
  	signal <- func(x[(w - window + 1) : w], ...)
  	signals[w] <- signal
  }
  
  return (signals)
}

signals <- sliding.window(sp$Adj.Close, 20, identify.signals, 0.05, 0.95, F)

plot(log(x), type = 'l', xlim = c(n - 1000, n), ylim = range(log(x)[(n - 1000) : n]))
plot(log(x), type = 'l')
buy.idx <- which(signals == 'buy')
sell.idx <- which(signals == 'sell')
abline(v = buy.idx, col = rgb(0, 1, 0, 0.1))
abline(v = sell.idx, col = rgb(1, 0, 0, 0.1))


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
  random.signals[[i]] <- list(window = window, margin = margin, singals = signals)
}

save.image('~/Desktop/marketStudies/data/randomSignals.csv')


x = sp$Adj.Close
down.q = 0.05
up.q = 0.95
window = 30
w = window
func = identify.signals