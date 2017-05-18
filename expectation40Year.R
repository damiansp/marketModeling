library(tseries)
get.hist.quote('^gspc', start = '1976-01-01', quote = c('Adj.Close, Volume'))

sp <- read.csv('~/Desktop/marketStudies/data/sp1976.csv')
head(sp)
tail(sp)

n <- dim(sp)[1]
days <- 1:n

plot(sp$Adj.Close ~ days, type = 'l')
plot(sp$Adj.Close ~ days, type = 'l', log = 'y')
plot(log(sp$Adj.Close) ~ days, type = 'l')

trend <- lm(log(Adj.Close) ~ days, sp)
abline(trend, col = 2)

k <- 250 # number of market days in a year


# Get value of an investment a year later for all data but the last year
get.period.value <- function(period = k, daily.adjust = F) {
  n <- dim(sp)[1]
  gains <- rep(NA, n)
  for (i in 1:(n - period)) {
  	# Calcualte the overall return (as a product of the initial; e.g. 1 = break 
  	# even, 1.1 means 10% gain, 0.9 means 10% loss)
    gains[i] <- sp$Adj.Close[i + period] / sp$Adj.Close[i]
    
    if (daily.adjust) {
      # Convert overall return to the daily rate of return
      gains[i] <- gains[i]^(1 / period)
    }
  }	
  gains
}

y1val <- get.period.value(k, T)

COLOR1 <- 'black'
COLOR2 <- 'yellow'
colors <- colorRampPalette(colors = c(COLOR1, COLOR2))(length(seq(k, 5 * k, 5)))
# add alpha channel
for (cl in 1:length(colors)) {
  colors[cl] <- paste(colors[cl], '1A', sep = '')
}

par(mfrow = c(2, 1))
par(mar = c(4, 4, 0, 0))
plot(log(sp$Adj.Close) ~ days, 
     type = 'l', 
     xlab = 'Market days (0 = 01 Jan 1976)',
     ylab = 'log(S&P 500 Close (Adjusted))',
     bty = 'n')
abline(trend, col = 2)
lines(5 + sp$Volume / max(sp$Volume), col = rgb(0, 0, 1, 0.5))
legend('topleft', 
	   lty = 1, 
	   col = rgb(0, 0, 1, 0.5), 
	   legend = ('Volume (not to scale)'),
	   bty = 'n')
par(mar = c(1, 4, 0, 0))
plot(y1val, 
     type = 'l', 
     col = rgb(1, 0, 0, 0.1), 
     ylab = 'Daily return multiplier')
abline(h = 1, col = 'grey')
ci <- 2
for (i in seq(k, 5 * k, 5)) {
  val <- get.period.value(i, T)
  lines(val, col = colors[ci])
  ci <- ci + 1
}
legend('bottomleft', 
	   title = 'Time horizon', 
	   lty = 1, 
	   col = c(COLOR1, COLOR2), 
	   legend = c('1 year', '5 years'),
	   bty = 'n')
	   
rate.of.change <- function(a, b) {
  b / a
}


peaks = c(150, 1300, 1950, 2950, 6200, 8000, 8250)
troughs = c(550, 1650, 3000, 6850, 8375)
up <- c(NA, 1 * (sp$Adj.Close[2:n] > sp$Adj.Close[1:(n - 1)]))
volUp <- volDown <- sp$Volume
volUp[up == 0] <- NA
volDown[up == 1] <- NA





# START
par(mfrow = c(6, 1))
par(mar = c(1, 4, 0, 0))
plot(log(sp$Adj.Close) ~ days, 
     type = 'l', 
     xlab = 'Market days (0 = 01 Jan 1976)',
     ylab = 'log(S&P 500 Close (Adjusted))',
     bty = 'n')
abline(trend, col = 4)
#abline(v = peaks, col = 2)
#abline(v = troughs, col = 3)
abline(v = danger, col = rgb(0, 1, 0, 0.05))
abline(v = danger1, col = rgb(0, 0, 1, 0.05))
abline(v = bad, col = rgb(1, 0, 1, 0.05))

resids <- log(sp$Adj.Close) - predict(trend)
plot(resids, type = 'l')
abline(h = 0, col = 'grey')
abline(
  h = quantile(
    resids, 
    probs = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), 
    na.rm = T),
  lty = c(5:1, 2:5),
  col = colorRampPalette(colors = c('green', 'red'))(9))
abline(v = danger, col = rgb(0, 1, 0, 0.05))
abline(v = danger1, col = rgb(0, 0, 1, 0.05))
abline(v = bad, col = rgb(1, 0, 1, 0.05))

plot(sp$Volume ~ days, type = 'l', ylab = 'Volume (S&P 500)')
ema180 <-  EMA(sp$Volume, n = 180)
lines(ema180, col = 5)
#abline(v = peaks, col = 2)
#abline(v = troughs, col = 3)
abline(v = danger, col = rgb(0, 1, 0, 0.05))
abline(v = danger1, col = rgb(0, 0, 1, 0.05))
legend('topleft',   lty = 1, col = 5, legend = 'EMA(180)', bg = 'white')

ema.roc <- rep(NA, length(ema180))
n.change = 250
for (i in (n.change + 1):length(ema180)) {
  ema.roc[i] <- rate.of.change(ema180[i - n.change], ema180[i])
}

plot(ema.roc ~ days, type = 'l', ylab = '90 day rate of change for EMA(180)')
abline(h = 1, col = 'grey')
#abline(v = peaks, col = 2)
#abline(v = troughs, col = 3)
abline(v = danger1, col = rgb(0, 0, 1, 0.05))
abline(
  h = quantile(
    ema.roc[ema.roc > 1], 
    probs = c(0.5, 0.75, 0.9, 0.95, 0.99), 
    na.rm = T), 
  lty = 2, 
  col = c('grey', 'grey', 'magenta', 'red', 'red'))
#danger1 <- which(ema.roc > 1.4086)  

ema.roc2 <- rep(NA, length(ema180))
n.change2 <- round(n.change * 4)
for (i in (n.change2 + 1):length(ema180)) {
  ema.roc2[i] <- rate.of.change(ema.roc[i - n.change2], ema.roc[i])
}
plot(ema.roc2 ~ days, type = 'l', ylab = '')
abline(h = 1, col = 'grey')
#abline(v = peaks, col = 2)
#abline(v = troughs, col = 3)
abline(v = danger, col = rgb(0, 1, 0, 0.05))
abline(
  h = quantile(
    ema.roc2[ema.roc2 > 1], 
    probs = c(0.5, 0.75, 0.9, 0.95, 0.99), 
    na.rm = T), 
  lty = 2, 
  col = c('grey', 'grey', 'magenta', 'red', 'red'))
#danger <- which(ema.roc2 > 1.4043)

plot(y1val, 
     type = 'l', 
     col = rgb(1, 0, 0, 0.1), 
     ylab = 'Daily return multiplier')
abline(h = 1, col = 'grey')
ci <- 2
#vals <- y1val
for (i in seq(k, 5 * k, 5)) {
  val <- get.period.value(i, T)
  #vals <- rbind(vals, val)
  lines(val, col = colors[ci])
  ci <- ci + 1
}
#abline(v = peaks, col = 2)
#abline(v = troughs, col = 3)
lines(meanVals, col = 5)
abline(v = bad, col = rgb(1, 0, 0, 0.05))
legend('bottomleft', 
	   title = 'Time horizon', 
	   lty = 1, 
	   col = c(COLOR1, COLOR2), 
	   legend = c('1 year', '5 years'),
	   bty = 'n')
#meanVals <- apply(vals, 2, mean)
#bad <- which(meanVals < 1)


# IN A NUTSHELL:
# RED: Keep current investments in, but don't put any more money in.
# POSITIVE RESIDUAL: Above th 95%ile: Sell everything
#                    BLUE or GREEN: Sell everything
# NEGATIVE RESIDUAL: GREEN: Sell everything
#                    BLUE AND NOT GREEN: Ok to buy.