#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
library(tseries)
library(TTR)

PATH <- '~/Desktop/marketStudies/data/sp1950.csv'

random.color <- function() {
  digits <- sample(unlist(strsplit('0123456789ABCDEF', '')), 6, replace=T)
  number <- '#'
  for (d in digits) {
  	number <- paste(number, d, sep='')
  }  
  number
}


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


read.and.prep.data <- function() {
  sp <- read.csv(PATH)
  sp$Adj.Close <- fill(sp$Adj.Close, 'forward')
  sp$Adj.Close <- fill(sp$Adj.Close, 'back')
  sp$Volume <- fill(sp$Volume, 'forward')
  sp$Volume <- fill(sp$Volume, 'back')
  sp
}

get.trend <- function(ts, days) {
  trend <- lm(log(Adj.Close) ~ days, data=ts)
  trend
}


# Get value of an investment a year later for all data but the last year
get.period.value <- function(ts, n, period=k, daily.adjust=F) {
  n <- dim(ts)[1]
  gains <- rep(NA, n)
  for (i in 1:(n - period)) {
  	# Calcualte the overall return (as a product of the initial; e.g. 1 = break 
  	# even, 1.1 means 10% gain, 0.9 means 10% loss)
    gains[i] <- ts$Adj.Close[i + period] / ts$Adj.Close[i]
    
    if (daily.adjust) {
      # Convert overall return to the daily rate of return
      gains[i] <- gains[i]^(1 / period)
    }
  }	
  gains
}


get.colors <- function(k) {
  COLOR1 <- random.color()
  COLOR2 <- random.color()
  colors <- colorRampPalette(
    colors = c(COLOR1, COLOR2))(length(seq(k, 5 * k, 30)))
  # Add alpha channel
  for (cl in 1:length(colors)) {
    colors[cl] <- paste(colors[cl], '4E', sep = '')
  }
  list(c1=COLOR1, c2=COLOR2, colors=colors)
}


vol.up.down <- function(ts, n) {
  up <- c(NA, 1 * (ts$Adj.Close[2:n] > ts$Adj.Close[1:(n - 1)]))
  volUp <- volDown <- ts$Volume
  volUp[up == 0] <- NA
  volDown[up == 1] <- NA
  list(volUp=volUp, volDown=volDown)
}


ts.plot <- function(ts, days, trend) {
  plot(log(ts$Adj.Close) ~ days, 
       type = 'l', 
       xlab = 'Market days (0 = 01 Jan 1976)',
       ylab = 'log(S&P 500 Close (Adjusted))',
       bty = 'n')
  abline(trend, col=4)
  
  legend('topleft', legend=paste('Slope:', round(coef(trend)[2], 5)), bty='n')
}


get.resids <- function(ts, trend, days, n) {
  log(ts$Adj.Close) - predict(trend, newdata = data.frame(days=1:n))	
}


resid.plot <- function(resids) {
  plot(resids, type='l')
  abline(h=0, col='grey')
  abline(
    h=quantile(
      resids, 
      probs=c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), 
      na.rm=T),
    lty=c(5:1, 2:5),
    col=colorRampPalette(colors = c('green', 'red'))(9))
  most.extreme <- quantile(resids, probs=c(0.01, 0.99), na.rm=T)
  n <- length(resids)
  if (resids[n] <= most.extreme[1] | resids[n] > most.extreme[2]) {
  	legend('topleft', legend='Extreme Condition', bg='white')
  }
}


vol.plot <- function(ts, days, ema) {
  plot(ts$Volume ~ days, type='l', ylab='Volume (S&P 500)')
  lines(ema, col=5)
  legend('topleft', lty=1, col=5, legend='EMA', bg='white')
}


projection.plot <- function(y1.val, c.list, short=k, long=5*k, ts, n) {
  plot(y1.val, 
       type = 'l', 
       col = c.list$colors[1], 
       ylab = 'Daily return multiplier')
  abline(h = 1, col = 'grey')

  ci <- 2
  for (i in seq(short, long, 30)) {
    val <- get.period.value(ts, n, i, T)
    vals <- rbind(y1.val, val)
    lines(val, col = c.list$colors[ci])
    ci <- ci + 1
  }

  legend('bottomleft', 
	     title = 'Time horizon', 
	     lty = 1, 
	     col = c(c.list$c1, c.list$c2), 
	     legend = c('1 year', '5 years'),
	     bty = 'n')
}


var.plot <- function(ts, windows=c(20, 60), colors=c('red', 'blue')) {
  ts <- ts$Adj.Close
  n <- length(ts)
  diffs <- c(NA, ts[2:n] / ts[1:(n - 1)])
  vars <- matrix(nrow=length(diffs), ncol=length(windows))

  for (w in 1:length(windows)) {
  	for (i in windows[w]:n) {
  	  vars[i, w] <- var(ts[i:(i - windows[w])])
  	}
  }
  
  matplot(vars, col=colors, type='l', lty=1, log='y')  
  legend('bottomright', 
         title='Moving Variances', 
         lty=1, 
         col=colors, 
         legend=windows, bty='n')
}


all.plots <- function(
  ts, days, trend, resids, y1.val, c.list, k, n, ema, short, long, proj=T) {
  
  if (proj) { n.plots=5 } else { n.plots=4 }
  par(mfrow = c(n.plots, 1))
  par(mar = c(1, 4, 0, 0))

  ts.plot(ts, days, trend)
  resid.plot(resids)
  var.plot(ts)
  vol.plot(ts, days, ema)
  
  if (proj) {
    projection.plot(y1.val, c.list, short, long, ts, n)
  }
}


plot.for.ts <- function(ts, k=250, short=250, long=5*250, proj=T) {
  n <- dim(ts)[1]              
  days <- 1:n
  trend <- get.trend(ts, days)
  y1.val <- get.period.value(ts, n, k, T)
  c.list <- get.colors(k)
  vols <- vol.up.down(ts, n)
  volUp <- vols$volUp
  volDown <- vols$volDown
  resids <- get.resids(ts, trend, days, n)
  ema90 <- EMA(ts$Volume, n=90)
  cat('n: ', n)
  all.plots(
    ts, days, trend, resids, y1.val, c.list, k, n, ema90, short, long, proj)
}


clip.series <- function(ts, n.days=NULL, day.range=NULL) {
  if (!is.null(n.days)) {
  	n <- dim(ts)[1]
  	return(ts[(n - n.days + 1):n, ])
  }
  
  return(ts[day.range[1]:day.range[2], ])
}

#===============================================================================
twoK.downtrend <- c(12500, 13500)
housing.crash <- c(14500, 15000)

# 50% out
sp <- read.and.prep.data() 
plot.for.ts(sp)
# 100% in

# 37.5% out
quartz()
sp.1k.days <- clip.series(sp, n.days=20*250)
plot.for.ts(sp.1k.days, long=20*250)
# 85% in

# 24% out
quartz()
sp.1k.days <- clip.series(sp, n.days=10*250)
plot.for.ts(sp.1k.days, long=10*250)
# 75% in

# 12.5% out
quartz()
sp.1k.days <- clip.series(sp, n.days=5*250)
plot.for.ts(sp.1k.days, long=5*250)
# 50% in

# 7.5% out
quartz()
sp.1k.days <- clip.series(sp, n.days=round(2.5*250))
plot.for.ts(sp.1k.days, long=round(2.5*250))
# 25% in

# 3% out
quartz()
sp.1yr <- clip.series(sp, n.days=round(1.25*250))
plot.for.ts(sp.1yr, long=round(1.25*250), proj=F)
# 12% in


#quartz()
#twoK <- clip.series(sp, day.range=twoK.downtrend)
#plot.for.ts(twoK, proj=F)

#quartz()
#hc <- clip.series(sp, day.range=housing.crash)
#plot.for.ts(hc, proj=F)
#===============================================================================

movingDev <- function(x, window, probs) {
  # probs should be listed as upward first, then downward
  md <- matrix(nrow=length(x), ncol=length(probs))
  init <- quantile(x[1:window], probs=probs, na.rm=T)
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

# stocks
w  <- 201
probs <- c(0.7396, 0.7630, 0.1166, # upward
           0.7617, 0.2846, 0.3369) # downward

# 401-k
w2  <- 60
u2  <- 0.9870
d2  <- 0.9894
dl2 <- 0.3384
ul2 <- 0.7604

movMedDev <- movingDev(sp$Adj.Close, window=w, probs=probs)

movMedDev2 <- movingDev(
  sp$Adj.Close, 
  window = w2, 
  probs = c(d2, dl2, ul2, u2))

n <- dim(sp)[1]
DAYS_AGO <- 2000
DAYS <- c(n - DAYS_AGO, n)
#DAYS <- c(1, n)
#DAYS <- twoK.downtrend
#DAYS <- housing.crash

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


par(mfrow=c(2, 1))
par(mar=c(2, 0.5, 0.5, 0.5))
























plot(sp$Adj.Close, 
     log='y', 
     type='l', 
     xlim=DAYS,
     ylim=range(sp$Adj.Close[DAYS[1]:DAYS[2]], na.rm = T))
lines(movMedDev[, 1], col=3)
lines(movMedDev[, 2], col=3)
lines(movMedDev[, 3], col=3)
lines(movMedDev[, 4], col=2)
lines(movMedDev[, 5], col=2)
lines(movMedDev[, 6], col=2)

#make.poly <- function(movMedDev, x, lower, upper, col = '#A9876554') {
#  polygon(c(1:length(x), length(x):1),
#          c((movMedDev[, lower]), rev(movMedDev[, upper])),
#          col = col,
#          border = NA)
#}

#make.all.poly <- function(movMedDev, x, lowers, uppers, col = '#A9876554') { 
#  for (i in 1:length(lowers)) {
#  	make.poly(movMedDev, x, lowers[i], uppers[i])
#  }
#}

#make.all.poly(movMedDev, sp$Adj.Close, c(1, 2), c(4, 3))


        
#plot(sp$Adj.Close, 
#     log = 'y', 
#     type = 'l', 
#     xlim = DAYS,
#     ylim = range(sp$Adj.Close[DAYS[1]:DAYS[2]], na.rm = T),
#     xaxt = 'n')
#lines(movMedDev2[, 1], lty = 1, col = '#A98765')
#lines(movMedDev2[, 2], lty = 1, col = '#A9876588')
#lines(movMedDev2[, 3], lty = 1, col = '#A9876588')
#lines(movMedDev2[, 4], lty = 1, col = '#A98765')

#make.all.poly(movMedDev2, sp$Adj.Close, c(1, 2), c(4, 3))
DAYS_AGO <- round(DAYS_AGO / 2)
DAYS <- c(n - DAYS_AGO, n)


#DAYS <- c(2*DAYS[1], DAYS[2])
#DAYS <- DAYS * 4
# ON the line is BELOW the line
        
# FOR Fidelity: Motley Fool--Buy and Hold but follow these recs.
# STOCKS (E*Trade)--These
# CURRENT BEST PARAMETERS:				10-day window
# WHEN REENTERING FROM PEAK: 			Sell: 05.00%	 p	(at the 36.55%ile u)
# WHEN DROPPING BELOW THE FALLING LINE:	Sell: 95.00% dp (at the 43.25%ile dl)
# WHEN CLIMBING ABOVE THE RISING LINE:	Buy:  89.73% up (at the 95.37%ile ul)
# WHEN REENTERING FROM BOTTOM:			Buy:  95.00% b	(at the 15.93%ile d)
#
#                        B: 89.73 
# 4) 95.37 -------------/----------------r-----------------------------------------
#
# 2) 43.25 -------\----------------------f-----------------------------------------
#                  S: 95 
# 1) 36.55 --\---------------------------p-----------------------------------------
#             S: 5                B: 95
# 3) 15.93 ----------------------/-------b----------------------------------------- 




# 401(k)
# CURRENT BEST PARAMETERS:				60-day window
# WHEN REENTERING FROM PEAK: 			Sell:     0%	 p  (at the 98.70%ile  	u)
# WHEN DROPPING BELOW THE FALLING LINE:	Sell: 26.35%	 dp	(at the 33.84%ile	dl)
# WHEN CLIMBING ABOVE THE RISING LINE:	Buy:      0% up (at the 76.04%ile	ul)
# WHEN REENTERING FROM BOTTOM:			Buy:  92.08% b	(at the 98.94%ile	d)

#            B: 92.08
# 98.94 ----/--------------------------------b--------------------------------
#
# 98.70 --------\----------------------------p--------------------------------
#                 S: 0  B: 0
# 76.04 ---------------/---------------------r--------------------------------
#
# 33.84 ---------------------\---------------f--------------------------------
#                             S: 26.35


# Precedence: peak > upward > downward > bottom
# (i.e., if both peak and downward happen on same day, use 'peak' signal)

# On the line = below the line