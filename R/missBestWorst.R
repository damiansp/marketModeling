#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
library(tseries)
library(TTR)

PATH <- '~/Desktop/marketStudies/data/sp1950.csv'

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
  names(sp)[1] <- 'Date'
  sp
}


sp <- read.and.prep.data()[, c('Date', 'Adj.Close')]
head(sp)
tail(sp)

TWENTY_YEARS_AGO <- '4/21/99'
first.row <- which(sp$Date == TWENTY_YEARS_AGO)
n <- dim(sp)[1]
sp <- sp[first.row:n, ]
head(sp)

n <- dim(sp)[1]
sp$change <- c(NA, sp$Adj.Close[2:n] / sp$Adj.Close[1:(n-1)]) - 1
sp <- sp[2:n, ]
n <- dim(sp)[1]
par(mfrow=c(2, 1))
plot(sp$Adj.Close, type='l', log='y')
plot(sp$change, type='l')
abline(h=0, col='grey')

range(sp$change)
sp$rank <- 0
sp$rank[order(sp$change)] <- 1:n
head(sp)
sp[sp$rank == 1, ] # worst day
sp[sp$rank == n, ] # best day

remove.best <- function(sp, best.n) {
  n <- dim(sp)[1]
  sp[sp$rank < (n - best.n + 1), ]
}

remove.worst <- function(sp, worst.n) {
  sp[sp$rank > worst.n, ]
}

get.value <- function(sp, best.n, worst.n, initial=10000) {
  sp <- remove.best(sp, best.n)
  sp <- remove.worst(sp, worst.n)
  sp$value <- 0
  sp$value[1] <- initial
  for (i in 2:dim(sp)[1]) {
  	sp$value[i] <- sp$value[i - 1] * (1 + sp$change[i])
  }
  sp
}

test <- get.value(sp, 0, 0)
head(test)

plot(sp$Adj.Close, type='l', log='y')
plot(test$value, type='l', log='y')

my.colors <- c('black', 'red', 'orange', 'green', 'darkgreen', 'blue', 'purple')


# missed the best n days
final.performance.best <- numeric(7)
par(mfrow=c(2, 1))
for (i in 1:7) {
  n.missed <- 10*(i - 1)
  cat(sprintf('Running for best %d days missed...\n', n.missed))
  sp.scenario <- get.value(sp, n.missed, 0)
  final.performance.best[i] <- sp.scenario$value[dim(sp.scenario)[1]]
  if (i == 1) { 
  	plot(sp.scenario$value, 
  	     type='l', 
  	     col=my.colors[i], 
  	     log='y', 
  	     ylim=c(500, 20000),
  	     xlab='Days since 20 Years ago',
  	     ylab='Value of $10K initial')
  }
  else { lines(sp.scenario$value, col=my.colors[i]) }
}
legend('bottomleft', 
       lty=1, 
       col=my.colors, 
       legend=seq(0, 60, 10), 
       title='Best n days\nmissed', 
       bty='n',
       cex=0.5)
       
# missed the best and worst n days
final.performance.bestworst <- numeric(7)
for (i in 1:7) {
  n.missed <- 10*(i - 1)
  cat(sprintf('Running for best and worst %d days missed...\n', n.missed))
  sp.scenario <- get.value(sp, n.missed, n.missed)
  final.performance.bestworst[i] <- sp.scenario$value[dim(sp.scenario)[1]]
  if (i == 1) { 
  	plot(sp.scenario$value, 
  	     type='l', 
  	     col=my.colors[i], 
  	     log='y', 
  	     ylim=c(500, 30000),
  	     xlab='Days since 20 Years ago',
  	     ylab='Value of $10K initial')
  }
  else { lines(sp.scenario$value, col=my.colors[i]) }
}
legend('bottomleft', 
       lty=1, 
       col=my.colors, 
       legend=seq(0, 60, 10), 
       title='Best and worst\nn days missed', 
       bty='n',
       cex=0.5)
       
barplot(final.performance.best, 
        names.arg=seq(0, 60, 10), 
        xlab='Missed best n days',
        ylab='Final value of $10K investment',
        col=c(3, 3, rep(2, 5)))
abline(h=10000, lty=2)  

barplot(final.performance.bestworst, 
        names.arg=seq(0, 60, 10), 
        xlab='Missed best and worst n days',
        ylab='Final value of $10K investment',
        col=3)
abline(h=10000, lty=2)  