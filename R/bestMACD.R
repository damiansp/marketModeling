# What parameters for the MACD oscillator actually capture market movement most accurately?  
# For a variety of parameters for the MACD, determine under total gains and total losses for when it indicates an upward or downward trajectory.  Return the parameters that have highest gains for up, and lowest for down.

rm(list=ls())
#source('~/Desktop/R/Common R Codes/get.hist.quote2.R')
SOURCE <- 'nya1965'
file <- sprintf('~/Desktop/marketStudies/data/%s.csv', SOURCE)
dat <- read.csv(file)
names(dat)[which(names(dat) == 'Adj.Close')] <- 'AdjClose'
head(dat)

myMACD <- function(prices, nFast=75, nSlow=155, sig=35, years=NULL, verbose=F) {
  # replace non-leading NAs with last known value
  # Find any NAs that aren't at the beginning or End
  naList <- which(is.na(prices))
  nonNAs <- which(!is.na(prices))
  firstNonNA <- nonNAs[1]
  nonLeadingNAs <- naList[naList > firstNonNA]
  # compute their value to be the last recorded value
  if (length(nonLeadingNAs) >= 1) {
    for (k in 1:length(nonLeadingNAs)) {				
      prices[nonLeadingNAs[k]] = prices[nonLeadingNAs[k] - 1]
    }	
  }
  n <- length(prices) 
  emaFast <- EMA(prices, nFast)
  emaSlow <- EMA(prices, nSlow)
  macd <- emaFast - emaSlow
  signal <- EMA(macd, sig)
  histo <- macd - signal
  if (!is.null(years)) k <- years * 250
  else k <- n - 1	
  plot(macd[(n - k):n], 
	   type='l', 
	   ylim=c(min(histo, macd, signal, na.rm=T), 
			  max(histo, macd, signal, na.rm=T)))
  lines(signal, col=2)
  abline(h=0)
  lines(histo, col=4)
  legend('bottomleft', lty=1, col=c(1, 2, 4), 
		 legend=c(paste('MACD', nFast, '-', nSlow, sep=''), 
		   		  paste('Signal', sig), 
		   		  'Histo'))
  if (verbose) {
	#return(merge(emaFast, emaSlow, macd, signal, histo))
	return(histo)
  }
}

n <- length(dat$AdjClose)
dat$daily <- c(NA, dat[2:n, 'AdjClose'] / dat[1:(n - 1), 'AdjClose'])
bestMACD <- myMACD(dat$AdjClose, 10, 151, 14, verbose=T)
bestMACD <- bestMACD[(n - 687):n]

fast <- c(5, 9, 10, 11, 20, 25)
slow <- c(147, 150, 151, 152, 153, 154)
signal <- c(13, 14, 15, 16, 19)

for (f in 1:6) {
  print('Next f')
  for (s in 1:6) {
  	for (sig in signal) {
  	  h <- myMACD(
  	    dat$AdjClose, nFast=fast[f], nSlow=slow[s], sig=sig, verbose=T)
  	  nh <- length(h)
  	  h.dir <- 1 * c(NA, h[2:nh] >= h[1:(nh - 1)])
  	  dat <- cbind(dat, h.dir)
#	  h.dir <- 1 * (h > 0)    # Use above two for direction...
#	  sp <- cbind(sp, h.dir) # These two for Hist +/-
  	  names(dat)[length(names(dat))] <- paste('p', fast[f], slow[s], sig, sep='.')
  	}
  }
}


# TODAY'S MACD is serving to predict TOMORROW'S SP direction so shift SP values 
# one day
#lines(sp$p.160.250.90 * 10, col = 'cyan')
dat[1:n, 'daily'] <- c(dat[2:n, 'daily'], NA)
dat <- dat[complete.cases(dat), ]
head(dat)

test <- data.frame(
  parameters=character(), cumUp=numeric(), cumDown=numeric(), cumAll=numeric())
for (clm in 3:ncol(dat)) {
  up   <- subset(dat[, c(2, clm)], dat[, clm] == 1)
  down <- subset(dat[, c(2, clm)], dat[, clm] == 0)
  df <- data.frame(parameters=names(dat)[clm], 
                   cumUp=prod(c(1, up$daily)), 
                   cumDown=prod(c(1, down$daily)),
                   cumAll=prod(c(1, dat$daily)))
  test <- rbind(test, df)
}

max(test$cumUp)
max(test$cumDown)
max(test$cumAll)
test[which.max(test$cumDown), ] # max = 7.6553 if hist change +/-

# Best parameters: slow: 10, fast: 151: sig: 14