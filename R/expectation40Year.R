library(tseries)
library(TTR)

sp <- read.csv('~/Desktop/marketStudies/data/sp1950.csv')
head(sp)
tail(sp)
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

n <- dim(sp)[1]
days <- 1:n


trend <- lm(log(Adj.Close) ~ days, sp)
length(days)
length(predict(trend, newdata = data.frame(days = 1:n)))

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

COLOR1 <- 'blue'
COLOR2 <- 'yellow'
colors <- colorRampPalette(colors = c(COLOR1, COLOR2))(length(seq(k, 5 * k, 30)))
# add alpha channel
for (cl in 1:length(colors)) {
  colors[cl] <- paste(colors[cl], '4E', sep = '')
}

	   
rate.of.change <- function(a, b) {
  b / a
}


up <- c(NA, 1 * (sp$Adj.Close[2:n] > sp$Adj.Close[1:(n - 1)]))
volUp <- volDown <- sp$Volume
volUp[up == 0] <- NA
volDown[up == 1] <- NA


twoK.downtrend <- c(12500, 13500)
housing.crash <- c(14500, 15000)

# START
par(mfrow = c(6, 1))
par(mar = c(1, 4, 0, 0))
plot(log(sp$Adj.Close) ~ days, 
     type = 'l', 
     xlab = 'Market days (0 = 01 Jan 1976)',
     ylab = 'log(S&P 500 Close (Adjusted))',
     bty = 'n')
abline(trend, col = 4)
abline(v = danger, col = rgb(1, 1, 0, 0.1))
abline(v = danger1, col = rgb(0, 0, 1, 0.05))
abline(v = bad, col = rgb(1, 0, 0, 0.05))

resids <- log(sp$Adj.Close) - predict(trend, newdata = data.frame(days = 1:n))
plot(resids, type = 'l')
abline(h = 0, col = 'grey')
abline(
  h = quantile(
    resids, 
    probs = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), 
    na.rm = T),
  lty = c(5:1, 2:5),
  col = colorRampPalette(colors = c('green', 'red'))(9))
abline(v = danger, col = rgb(1, 1, 0, 0.1))
abline(v = danger1, col = rgb(0, 0, 1, 0.05))
abline(v = bad, col = rgb(1, 0, 0, 0.05))


plot(sp$Volume ~ days, type = 'l', ylab = 'Volume (S&P 500)')
ema90 <- EMA(sp$Volume, n = 90)
lines(ema90, col = 5)
abline(v = danger, col = rgb(1, 1, 0, 0.1))
abline(v = danger1, col = rgb(0, 0, 1, 0.05))
legend('topleft',   lty = 1, col = 5, legend = 'EMA(90)', bg = 'white')

ema.roc <- rep(NA, length(ema90))
n.change = 500
for (i in (n.change + 1):length(ema90)) {
  ema.roc[i] <- rate.of.change(ema90[i - n.change], ema90[i])
}

plot(ema.roc ~ days, type = 'l', ylab = '90 day rate of change for EMA(180)')
ema2 <-  EMA(ema.roc, n = 90)
lines(ema2, col = 5)

abline(h = 1, col = 'grey')
abline(v = danger1, col = rgb(0, 0, 1, 0.05))
qs <- quantile(ema.roc[ema.roc > 1], 
               probs = c(0.5, 0.75, 0.9, 0.95, 0.99), 
               na.rm = T)
abline(
  h = qs, 
  lty = 2, 
  col = c('grey', 'grey', 'magenta', 'red', 'red'))
danger1 <- which(ema.roc > qs['90%'])  

ema.roc2 <- rep(NA, length(ema90))
n.change2 <- round(n.change * 2)
for (i in (n.change2 + 1):length(ema90)) {
  ema.roc2[i] <- rate.of.change(ema2[i - n.change2], ema2[i])
}
plot(ema.roc2 ~ days, type = 'l', ylab = '')
abline(h = 1, col = 'grey')
abline(v = danger, col = rgb(1, 1, 0, 0.1))
qs <- quantile(ema.roc2[ema.roc2 > 1], 
               probs = c(0.5, 0.75, 0.9, 0.95, 0.99), 
               na.rm = T)
abline(
  h = qs, 
  lty = 2, 
  col = c('grey', 'grey', 'magenta', 'red', 'red'))
danger <- which(ema.roc2 > qs['90%'])

plot(y1val, 
     type = 'l', 
     col = colors[1], 
     ylab = 'Daily return multiplier')
abline(h = 1, col = 'grey')
ci <- 2
vals <- y1val
for (i in seq(k, 5 * k, 30)) {
  val <- get.period.value(i, T)
  vals <- rbind(vals, val)
  lines(val, col = colors[ci])
  ci <- ci + 1
}
abline(v = bad, col = rgb(1, 0, 0, 0.05))
legend('bottomleft', 
	   title = 'Time horizon', 
	   lty = 1, 
	   col = c(COLOR1, COLOR2), 
	   legend = c('1 year', '5 years'),
	   bty = 'n')
meanVals <- apply(vals, 2, mean)
lines(meanVals, col = 5)
bad <- which(meanVals < 1)





DAYS <- 1000 
quartz()
n <- length(sp$Adj.Close)
par(mfrow = c(6, 1))
par(mar = c(1, 4, 0, 0))
plot(log(sp$Adj.Close) ~ days, 
     type = 'l', 
     xlab = 'Market days (0 = 01 Jan 1950)',
     ylab = 'log(S&P 500 Close (Adjusted))',
     xlim = c((n - DAYS), n),
     ylim = range(log(sp$Adj.Close)[(n - DAYS):n], na.rm = T),
     bty = 'n')
abline((trend), col = 4)
abline(v = danger, col = rgb(1, 1, 0, 0.7))
abline(v = danger1, col = rgb(0, 0, 1, 0.7))
abline(v = bad, col = rgb(1, 0, 0, 0.7))

resids <- log(sp$Adj.Close) - predict(trend, newdata = data.frame(days = 1:n))
plot(resids, 
     type = 'l',     
     xlim = c((n - DAYS), n),
     ylim = range(resids[(n - DAYS):n], na.rm = T))
abline(h = 0, col = 'grey')
abline(
  h = quantile(
    resids, 
    probs = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), 
    na.rm = T),
  lty = c(5:1, 2:5),
  col = colorRampPalette(colors = c('green', 'red'))(9))
abline(v = danger, col = rgb(1, 1, 0, 0.7))
abline(v = danger1, col = rgb(0, 0, 1, 0.7))
abline(v = bad, col = rgb(1, 0, 0, 0.7))


plot(sp$Volume ~ days, 
     type = 'l', 
     ylab = 'Volume (S&P 500)',
     xlim = c((n - DAYS), n),
     ylim = range(sp$Volume[(n - DAYS):n], na.rm = T))

ema180 <-  EMA(sp$Volume, n = 180)
lines(ema180, col = 5)
abline(v = danger, col = rgb(1, 1, 0, 0.7))
abline(v = danger1, col = rgb(0, 0, 1, 0.7))
legend(
  'topleft',   lty = 1, col = 5, legend = 'EMA(90)', bg = 'white', bty = 'n')

ema.roc <- rep(NA, length(ema180))
n.change = 500
for (i in (n.change + 1):length(ema180)) {
  ema.roc[i] <- rate.of.change(ema180[i - n.change], ema180[i])
}

plot(ema.roc ~ days, 
     type = 'l', 
     ylab = '90 day rate of change for EMA(180)',
     xlim = c((n - DAYS), n),
     ylim = range(ema.roc[(n - DAYS):n], na.rm = T))
ema2 <-  EMA(ema.roc, n = 90)
lines(ema2, col = 5)

abline(h = 1, col = 'grey')
abline(v = danger1, col = rgb(0, 0, 1, 0.7))
qs <- quantile(ema.roc[ema.roc > 1], 
               probs = c(0.5, 0.75, 0.9, 0.95, 0.99), 
               na.rm = T)
abline(
  h = qs, 
  lty = 2, 
  col = c('grey', 'grey', 'magenta', 'red', 'red'))
danger1 <- which(ema.roc > qs['90%'])  

ema.roc2 <- rep(NA, length(ema180))
n.change2 <- round(n.change * 2)
for (i in (n.change2 + 1):length(ema180)) {
  ema.roc2[i] <- rate.of.change(ema2[i - n.change2], ema2[i])
}
plot(ema.roc2 ~ days, 
     type = 'l', 
     ylab = '',
     xlim = c((n - DAYS), n),
     ylim = range(ema.roc2[(n - DAYS):n], na.rm = T))
abline(h = 1, col = 'grey')
abline(v = danger, col = rgb(1, 1, 0, 0.7))
qs <- quantile(ema.roc2[ema.roc2 > 1], 
               probs = c(0.5, 0.75, 0.9, 0.95, 0.99), 
               na.rm = T)
abline(
  h = qs, 
  lty = 2, 
  col = c('grey', 'grey', 'magenta', 'red', 'red'))
danger <- which(ema.roc2 > qs['90%'])

COLOR1 <- 'blue'
COLOR2 <- 'yellow'
colors <- colorRampPalette(colors = c(COLOR1, COLOR2))(length(seq(k, 5 * k, 30)))
# add alpha channel
for (cl in 1:length(colors)) {
  colors[cl] <- paste(colors[cl], '9C', sep = '')
}

plot(y1val, 
     type = 'l', 
     col = colors[1], 
     ylab = 'Daily return multiplier',
     xlim = c((n - DAYS), n),
     ylim = range(y1val[(n - DAYS):n], na.rm = T))
abline(h = 1, col = 'grey')
ci <- 2
vals <- y1val
for (i in seq(k, 5 * k, 30)) {
  val <- get.period.value(i, T)
  vals <- rbind(vals, val)
  lines(val, col = colors[ci])
  ci <- ci + 1
}
abline(v = bad, col = rgb(1, 0, 0, 0.7))
legend('bottomleft', 
	   title = 'Time horizon', 
	   lty = 1, 
	   col = c(COLOR1, COLOR2), 
	   legend = c('1 year', '5 years'),
	   bty = 'n')
meanVals <- apply(vals, 2, mean)
lines(meanVals, col = 5)
bad <- which(meanVals < 1)

# IN A NUTSHELL:
# RED: Keep current investments in, but don't put any more money in.
# YELLOW OR BLUE: Sell ..... when it drops below 99%ile again, OK to buy








movingDev <- function(
  x, 
  window = 70, 
  probs = c(0.005, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.995)) {

  md <- matrix(nrow = length(x), ncol = length(probs))
  init <- quantile(
    x[1:window], 
    probs = probs,
    na.rm = T)
  init <- matrix(rep(init, window), ncol = length(probs), byrow = T)
  md[1:window, ] <- init

  for (w in (window + 1):length(x)) {
  	md[w, ] <- quantile(
  	  x[(w - window):w],
  	  probs = probs,
      na.rm = T)
  }
  
  md
}

w <- 15
u <- 0.8317
d <- 0.99
dl <- 0.99
ul <- 0.0100

w2 <- 134
u2 <- 0.1303
d2 <- 0.0100
dl2 <- 0.0114
ul2 <- 0.0461

movMedDev <- movingDev(
  sp$Adj.Close, 
  window = w, 
  probs = c(d, dl, ul, u))

movMedDev2 <- movingDev(
  sp$Adj.Close, 
  window = w2, 
  probs = c(d2, dl2, ul2, u2))


DAYS <- 4000 # 16 yrs; rep to halve
quartz()
MULT <- 0.011
ALPHA <- 0.2

volume.weighted <- function(x, volume, lambda, gamma) {
  num <- den <- numeric(length(x))
  num[1] <- volume[1] * x[1]
  den[1] <- volume[1]
  
  for(i in 2:length(num)) {
    num[i] <- lambda * num[i - 1] + volume[i]^gamma * x[i]
    den[i] <- lambda * den[i - 1] + volume[i]^gamma
  }
  num / den
}


par(mfrow = c(2, 1))
par(mar = rep(0.5, 4))

plot(sp$Adj.Close, 
     log = 'y', 
     type = 'l', 
     xlim = c((n - DAYS), n),
     #xlim = twoK.downtrend,
     ylim = range(sp$Adj.Close[(n - DAYS):n], na.rm = T),
     #ylim = range(sp$Adj.Close[twoK.downtrend[1]:twoK.downtrend[2]], na.rm = T),
     xaxt = 'n')
lines(movMedDev[, 1], lty = 1, col = '#A98765')
lines(movMedDev[, 2], lty = 1, col = '#A9876588')
lines(movMedDev[, 3], lty = 1, col = '#A9876588')
lines(movMedDev[, 4], lty = 1, col = '#A98765')

make.poly <- function(movMedDev, x, lower, upper, col = '#A9876554') {
  polygon(c(1:length(x), length(x):1),
          c((movMedDev[, lower]), rev(movMedDev[, upper])),
          col = col,
          border = NA)
}

make.all.poly <- function(movMedDev, x, lowers, uppers, col = '#A9876554') { 
  for (i in 1:length(lowers)) {
  	make.poly(movMedDev, x, lowers[i], uppers[i])
  }
}

make.all.poly(movMedDev, sp$Adj.Close, c(1, 2), c(4, 3))
abline(v = danger, col = rgb(1, 1, 0, 0.2))
abline(v = danger1, col = rgb(0, 0, 1, 0.2))
abline(v = bad, col = rgb(1, 0, 0, 0.2))


        
plot(sp$Adj.Close, 
     log = 'y', 
     type = 'l', 
     xlim = c((n - DAYS), n),
     #xlim = housing.crash,
     ylim = range(sp$Adj.Close[(n - DAYS):n], na.rm = T),
     #ylim = range(sp$Adj.Close[housing.crash[1]:housing.crash[2]], na.rm = T),
     xaxt = 'n')
lines(movMedDev2[, 1], lty = 1, col = '#A98765')
lines(movMedDev2[, 2], lty = 1, col = '#A9876588')
lines(movMedDev2[, 3], lty = 1, col = '#A9876588')
lines(movMedDev2[, 4], lty = 1, col = '#A98765')

make.all.poly(movMedDev2, sp$Adj.Close, c(1, 2), c(4, 3))
abline(v = danger, col = rgb(1, 1, 0, 0.2))
abline(v = danger1, col = rgb(0, 0, 1, 0.2))
abline(v = bad, col = rgb(1, 0, 0, 0.2))

DAYS <- round(DAYS / 2)
        
# IN A NUTSHELL:
# YELLOW OR BLUE: Sell ..... when it drops below 99%ile again, OK to buy

# IF upward trend: 
#   SELL OFF 10% whenver dropping down into both bands (-> to bonds)
#   ALL IN whenever climbing back up into the color band 

# SELL OFF EVERYTHING whenever dropping below the median band (bonds)
# ALL IN whenever climbing above the median band

# IF downward trend: 
#   ALL IN whenever re-entering from below 99% line 
#   SELL OFF everything whenever dropping down into the color band (-> to bonds)


# STOCKS
# CURRENT BEST PARAMETERS:				15-day window
# WHEN REENTERING FROM PEAK: 			Sell: 0.0%	p	(at the 83.2%ile 	u)
# WHEN DROPPING BELOW THE FALLING LINE:	Sell: 57.6%	dp	(at the 99%ile		dl)
# WHEN CLIMBING ABOVE THE RISING LINE:	Buy:  75.6%	up  (at the 1%ile		ul)
# WHEN REENTERING FROM BOTTOM:			Buy:  0%	b	(at the 99%ile		d)

#           
# 99.0-----\---------------------------------------------------------------------
#           S: 57.6%   
# 83.2----------------/\---------------------------------------------------------
#                       Nothing   B: 75.6%
# 1   ---------------------------/-----------------------------------------------


# 401(k)
# CURRENT BEST PARAMETERS:				134-day window
# WHEN REENTERING FROM PEAK: 			Sell: 0%	p	(at the 13.0%ile  	u)
# WHEN DROPPING BELOW THE FALLING LINE:	Sell: 49.4%	dp	(at the 1.1%ile		dl)
# WHEN CLIMBING ABOVE THE RISING LINE:	Buy:  0%	up  (at the 4.6%ile		ul)
# WHEN REENTERING FROM BOTTOM:			Buy:  79.8%	b	(at the 1%ile		d)

# 13.1---/\----------------------------------------------------------------------
#          Nothing   Nothing
# 4.6 --------------/------------------------------------------------------------
#          
# 1.1 ----------------------\----------------------------------------------------
#                            S: 49.4     B: 79.8
# 1.0 ---------------------------------\/----------------------------------------

# Precedence: peak > upward > downward > bottom
# (i.e., if both peak and downward happen on same day, use 'peak' signal)

# On "equals" days:
# <= peak = peak
# > upward = upward
# <= downward = downward
# > bottom = bottom