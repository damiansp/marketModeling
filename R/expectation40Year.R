#-------#---------#---------#---------#---------#---------#---------#---------
#install.packages(c('tseries', 'TTR'))
library(tseries)
library(TTR)

SOURCE <- 'sp1950.csv' # 'nasdaq1965.csv' # 'nya1965.csv'
PATH <- sprintf('~/Learning/marketModeling/data/%s', SOURCE)

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

# linear trend
get.linear.trend <- function(ts, days) {
  trend <- lm(log(Adj.Close) ~ days, data=ts)
  trend
}

# EMA-trend
get.trend <- function(series, window, lambda=0.95) {
  w <- rev(lambda^(0:(window - 1)))
  trend <- WMA(log(series$Adj.Close), n=window, wts=w)
  trend
}


# Get value of an investment a year later for all data but the last year
get.period.value <- function(ts, n, period=k, daily.adjust=F) {
  n <- dim(ts)[1]
  gains <- rep(NA, n)
  for (i in 1:(n - period)) {
  	# Calcualte the overall return (as a product of the initial; e.g. 1 = 
  	# break even, 1.1 means 10% gain, 0.9 means 10% loss)
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

ts.plot <- function(ts, days, trend, linear.trend) {
  plot(log(ts$Adj.Close) ~ days, type='l')
  lines(trend ~ days, col=2)
  abline(linear.trend, col=4)
}


get.linear.resids <- function(ts, trend, days, n) {
  log(ts$Adj.Close) - predict(trend, newdata = data.frame(days=1:n))	
}

get.resids <- function(ts, trend) {
  log(ts$Adj.Close) - trend
}


resid.plot <- function(resids, condition) {
  plot(resids, type='l')
  abline(h=0, col='grey')
  ps <- c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)
  qs <- quantile(resids, probs=ps, na.rm=T)
  abline(
    h=qs, lty=c(5:1, 2:5), col=colorRampPalette(colors = c('green', 'red'))(9))
  most.extreme <- quantile(resids, probs=c(0.01, 0.99), na.rm=T)
  near.extreme <- quantile(resids, probs=c(0.05, 0.95), na.rm=T)
  med <- median(resids, na.rm=T)
  n <- length(resids)
  position <- ifelse(condition == 'buy', 'topleft', 'bottomleft')
  pos2 <- ifelse(condition == 'buy', 'bottomright', 'topright')
  if (resids[n] <= most.extreme[1] | resids[n] > most.extreme[2]) {
  	legend(position, legend='Extreme Condition', bg='white')
  } else if (resids[n] <= near.extreme[1] | resids[n] > near.extreme[2]) {
  	legend(position, legend='Near-Extreme Condition', bg='white')
  } else if ((resids[n - 1] < med & resids[n] >= med) 
             | resids[n - 1] > med & resids[n] <= med) {
  	legend(pos2, legend='Median Crossed', bg='white')
  }
  legend('left',
         legend=c(sprintf('now: %.4f', resids[n]), 
                  sprintf('%.2f: %.4f', ps[7], qs[7]),
                  sprintf('Cash in: %d', 1 * (resids[n] <= qs[7]))),
         bg='white')
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
    ts, days, trend, resids, linear.trend, linear.resids, y1.val, c.list, k, n, 
    ema, short, long, proj=T) {
  if (proj) { n.plots <- 5 } else { n.plots <- 4 }
  par(mfrow=c(n.plots, 1))
  par(mar=c(1, 4, 0, 0))
  ts.plot(ts, days, trend, linear.trend) # +lt
  resid.plot(linear.resids, 'sell')
  legend('topleft', legend='Sell Guide', bg='white')
  resid.plot(resids, 'buy')
  legend('bottomleft', legend='Buy Guide', bg='white')
  var.plot(ts)
  #vol.plot(ts, days, ema)
  if (proj) {
    projection.plot(y1.val, c.list, short, long, ts, n)
  }
}


plot.for.ts <- function(ts, k=250, short=250, long=5*250, proj=F) {
  n <- dim(ts)[1]              
  days <- 1:n
  linear.trend <- get.linear.trend(ts, days)
  ### ADJUST HERE ###
  trend <- get.trend(ts, window=round(n / 10), lambda=0.99)  
  y1.val <- get.period.value(ts, n, k, T)
  c.list <- get.colors(k)
  vols <- vol.up.down(ts, n)
  volUp <- vols$volUp
  volDown <- vols$volDown
  linear.resids <- get.linear.resids(ts, linear.trend, days, n)
  resids <- get.resids(ts, trend)
  ema90 <- EMA(ts$Volume, n=90)
  cat('n: ', n)
  all.plots(ts, 
            days, 
            trend, 
            resids, 
            linear.trend, 
            linear.resids, 
            y1.val, 
            c.list, 
            k, 
            n, 
            ema90, 
            short, 
            long, 
            proj)
}


clip.series <- function(ts, n.days=NULL, day.range=NULL) {
  if (!is.null(n.days)) {
  	n <- dim(ts)[1]
  	return(ts[(n - n.days + 1):n, ])
  }
  return(ts[day.range[1]:day.range[2], ])
}

#=============================================================================
twoK.downtrend <- c(12500, 13500)
housing.crash <- c(14500, 15000)
sp <- read.and.prep.data() 
tail(sp)


fracs.out <- c()
fracs.in <- c()

# Money back in at the median line----------
plot.for.ts(sp)
# 75%/37% out; 100/50% in; Extreme (top/bottom)
# 37%/18% out; 50/25% in; Near-Extreme (top/bottom)
f.out <- 0  # 
f.in <- 0 # 
fracs.out <- c(fracs.out, f.out)
fracs.in <- c(fracs.in, f.in)

sp.1k.days <- clip.series(sp, n.days=20*250)
plot.for.ts(sp.1k.days, long=20*250)
# 60%/30% out; 95/48% in; Extreme (top/bottom)
# 30%/15% out; 48/23% in; Near-Extreme (top/bottom)
f.out <- 0
f.in <- 0  #
fracs.out <- c(fracs.out, f.out)
fracs.in <- c(fracs.in, f.in)

sp.1k.days <- clip.series(sp, n.days=10*250)
plot.for.ts(sp.1k.days, long=10*250)
# 50%25% out; 75/38% in; Extreme (top/bottom)
# 25%12% out; 38/19% in; Near-Extreme (top/bottom)
f.out <- 0  #
f.in <- 0.38  # bottom extr
fracs.out <- c(fracs.out, f.out)
fracs.in <- c(fracs.in, f.in)

sp.1k.days <- clip.series(sp, n.days=5*250)
plot.for.ts(sp.1k.days, long=5*250)
# 40%/20% out; 50/25% in; Extreme (top/bottom)
# 20%/10% out; 25/13% in; Near-Extreme (top/bottom)
f.out <- 0
f.in <- 0  #
fracs.out <- c(fracs.out, f.out)
fracs.in <- c(fracs.in, f.in)

sp.1k.days <- clip.series(sp, n.days=round(2.5*250))
plot.for.ts(sp.1k.days, long=round(2.5*250))
# 30%/15% out; 25/13% in; Extreme (top/bottom)
# 15/7% out; 12/6% in; Near Extreme (top/bottom)
f.out <- 0.07 # bottom near extr
f.in <- 0.25  # top extr. 
fracs.out <- c(fracs.out, f.out)
fracs.in <- c(fracs.in, f.in)

sp.1yr <- clip.series(sp, n.days=round(1.25*250))
plot.for.ts(sp.1yr, long=round(1.25*250), proj=F)
# 20%/10% out; 12/6% in; Extreme (top/bottom)
# 10/5% out; 6/3% in; Near Extreme (top/bottom)
f.out <- 0.06 # bottom extr
f.in <- 0.12  # top extr
fracs.out <- c(fracs.out, f.out)
fracs.in <- c(fracs.in, f.in)

sp.6mos <- clip.series(sp, n.days=round(0.5*250))
plot.for.ts(sp.6mos, long=round(0.5*250), proj=F)
# 10%/5% out; 6/3% in; Extreme (top/bottom)
# 5%/2% out; 3/1% in; Near Extreme (top/bottom)
f.out <- 0.1  # top
f.in <- 0  # 
fracs.out <- c(fracs.out, f.out)
fracs.in <- c(fracs.in, f.in)

# 0.81  # init amt: $18463
plot.for.ts(sp.1yr, long=round(1*250), proj=F)
#=============================================================================
if (length(fracs.out) != 7 | length(fracs.in) != 7) {
	cat('STOP: missing a fraction out.  Rerun')
}

remove.subsequent.fractions <- function(fractions) {
	amt.in <- 1
	for (f in fractions) {
		amt.in <- amt.in - f*amt.in
	}
	amt.in
}

add.subsequent.fractions <- function(fractions) {
	reserves <- 1
	for (f in fractions) {
		reserves <- reserves - f*reserves
	}
	1 - reserves
}


(pct.invested <- remove.subsequent.fractions(fracs.out))
(pct.reserves.to.add <- add.subsequent.fractions(fracs.in))