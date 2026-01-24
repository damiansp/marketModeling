#---------#---------#---------#---------#---------#---------#---------#---------
#install.packages('stringr')
library(stringr)

load('~/Learning/marketModeling/data/myHistoric.RData')
head(df)
tail(df)

get.daily.change <- function(x) {
  (x[2:length(x)] - x[1:(length(x) - 1)]) / x[1:(length(x) - 1)]
}


prev.last <- dim(df)[1] 


sp.actual <- c(
  df$sp.actual,          6858,  6902,  6945,  6921,  6921,  6966,  6977,  6964,  
    6927,  6944,  6940,  6797,  6876,  6913,  6916)

fid.in <- c( 
  df$fid.in,           172299,172299,172299,172299,172299,172299,172299,172299,
  172299,172299,172299,172299,172299,172299,172299)
fid.val <- c(
  df$fid.val,          290094,294181,296138,295822,295959,297678,299111,296633,
  294777,293860,293302,287389,287978,289990,291633)

f1k.in <- c(
  df$f1k.in,            66140, 66140, 66140, 66140, 66140, 66140, 66140, 66140,
   66140, 66140, 66140, 66140, 66140, 66140, 66140)  # (1125.13)
f1k.val <- c(
  df$f1k.val,          119907,120000,121051,121811,121648,121593,122330,122629,
  122190,121532,121802,121723,119151,120259,120965)  #

self.mng.in <- c(
  df$self.mng.in,       25017, 25017, 25017, 25017, 25017, 25017, 25017, 25017, 
   25017, 25017, 25017, 25017, 25017, 25017, 25017)  #
self.mng.val <- c(
  df$self.mng.val,      35889, 36262, 36552, 36362, 36617, 36757, 36901, 36708, 
   36729, 36738, 36701, 36155, 36359, 36498, 36440)  #

et.in <- c(
  df$et.in,            101275,101275,101275,101275,101275,101275,101275,101275,
  101275,102706,102706,102870,102870,102870,102870)  #
et.val <-c(
  df$et.val,           259715,263830,267624,266268,268148,270396,273467,270489,
  270690,274413,274306,268626,270182,271636,272747)  #

sim1 <- (c(
                100000,100000,100000,100000,100002, 99964, 99955,100013,100151,
  100118, 99925, 99476, 99209, 99429,100134,100957)
  / 100000)
sim1.res <- 149915
mw <- read.csv('~/Downloads/Holdings - Damian Satterthwaite-Phillips.csv')
mw$Value <- str_replace(mw$Value, ",", "")
mw$Value <- as.numeric(str_replace(mw$Value, "\\$", ""))
sim1.val <- sum(mw$Value)
sim1.name <- 'adelaide'

sim2 <- (c(
                100000,100003,100020,100064, 99976,100074,100105,100219, 99945,
  100237, 99802, 99357, 98890, 99145,100149,100860)
  / 100000)
sim2.res <- 132411
mw <- read.csv('~/Downloads/Holdings - Damian Satterthwaite-Phillips(1).csv')
mw$Value <- str_replace(mw$Value, ",", "")
mw$Value <- as.numeric(str_replace(mw$Value, "\\$", ""))
sim2.val <- sum(mw$Value)
sim2.name <- 'baloney'

sim3 <- (c(
                100000,100000,100000,100000,100004, 99982, 99974,100079,100309,
  100122, 99803, 99289, 98552, 98847,100121,101035)
  / 100000)
sim3.res <- 124227
mw <- read.csv('~/Downloads/Holdings - Damian Satterthwaite-Phillips(2).csv')
mw$Value <- str_replace(mw$Value, ",", "")
mw$Value <- as.numeric(str_replace(mw$Value, "\\$", ""))
sim3.val <- sum(mw$Value)
sim3.name <- 'cacaphony'


sim4 <- (c(
                100000,100000,100000,100000,100000,100000,100000,100000,100000,
  100000,100000,100000,100093,100276,100529,100688)
  / 100000)
sim4.res <- 195802
mw <- read.csv('~/Downloads/Holdings - Damian Satterthwaite-Phillips(3).csv')
mw$Value <- str_replace(mw$Value, ",", "")
mw$Value <- as.numeric(str_replace(mw$Value, "\\$", ""))
sim4.val <- sum(mw$Value)
sim4.name <- 'degroovy'

sim5 <- (c(
                100000,100275,102541,104265,102645,102968,104032,105979,105712,
  106029,106530,106626,103021,104818,106646,105788)
  / 100000)
sim5.res <- 373
mw <- read.csv('~/Downloads/Holdings - Damian Satterthwaite-Phillips(4).csv')
mw$Value <- str_replace(mw$Value, ",", "")
mw$Value <- as.numeric(str_replace(mw$Value, "\\$", ""))
sim5.val <- sum(mw$Value)
sim5.name <- 'elemental'

extr <- (c(
                100000,100000,100000,100000,100020, 89687, 99597,100151,101444,
  101135,101125,100388, 98194, 98419,100872,102571)
  / 100000)
extr.res <- 55595
#mw <- read.csv('~/Downloads/Holdings - Damian Satterthwaite-Phillips(5).csv')
#mw$Value <- str_replace(mw$Value, ",", "")
#mw$Value <- as.numeric(str_replace(mw$Value, "\\$", ""))
#extr.val <- sum(mw$Value)
#extr.name <- 'extrema'


normalize.to.index <- function(x, index) {
	x / x[index]
}


sp <- normalize.to.index(sp.actual, prev.last)
spd <- get.daily.change(sp)
fid <- fid.val / fid.in
fid <- normalize.to.index(fid, prev.last)
fidd <- get.daily.change(fid)
f1k <- f1k.val / f1k.in
f1k <- normalize.to.index(f1k, prev.last)
self.mng <- self.mng.val / self.mng.in
self.mng <- normalize.to.index(self.mng, prev.last)
self.mng.d <- get.daily.change(self.mng)
et <- et.val / et.in
et <- normalize.to.index(et, prev.last)
etd <- get.daily.change(et)
tot.in <- fid.in + f1k.in + et.in
tot.val <- fid.val + f1k.val + et.val
tot <- tot.val / tot.in
tot <- normalize.to.index(tot, prev.last)
mine.in <- fid.in + et.in + self.mng.in
mine.val <- fid.val + et.val + self.mng.val
mine <- mine.val / mine.in
mine <- normalize.to.index(mine, prev.last)

# Calculate Sharpe Ratios
get.sharpe.ratio <- function(returns, window=length(returns), rnd=3, ignore0=F) {
  if (length(returns) > window) {
    returns <- returns[(length(returns) - window):length(returns)]	  	
  }	
  returns <- returns[!is.na(returns)]
  if (ignore0) {
  	retruns <- returns[returns != 0]
  }
  round((sqrt(252) * mean(returns)) / sd(returns), rnd)
}


get.daily.returns <- function(x) { 
  n <- length(x)
  x[2:n] / x[1:(n - 1)] - 1 
}

get.sharpe.from.daily.values <- function(x, rnd=3, ignore0=F) {
  x <- x[!is.na(x)]
  get.sharpe.ratio(get.daily.returns(x), rnd=rnd, ignore0=ignore0)
}



m <- length(sp)
n <- length(sp) - prev.last
prev <- m - n
this.year <- (prev):m
days <- -prev:(n - 1) + 1
new.days <- 0:(n - 1)
year.start <- length(tot) - length(new.days)

sp.sh       <- get.sharpe.from.daily.values(sp[this.year])
self.mng.sh <- get.sharpe.from.daily.values(self.mng[this.year], ignore0=T)
tot.sh      <- get.sharpe.from.daily.values(tot[this.year])
mine.sh     <- get.sharpe.from.daily.values(mine[this.year])
f1k.sh      <- get.sharpe.from.daily.values(f1k[this.year]) 
fid.sh      <- get.sharpe.from.daily.values(fid[this.year]) 
et.sh       <- get.sharpe.from.daily.values(et[this.year])
sim1.sh     <- get.sharpe.from.daily.values(sim1)
sim2.sh     <- get.sharpe.from.daily.values(sim2)
sim3.sh     <- get.sharpe.from.daily.values(sim3)
sim4.sh     <- get.sharpe.from.daily.values(sim4)
sim5.sh     <- get.sharpe.from.daily.values(sim5)
extr.sh     <- get.sharpe.from.daily.values(extr)


# PLOT 1: This year's returns ---------------------------------------------------
plot(
  days, 
  sp / sp[year.start], 
  type='l', 
  col='orange',
  lwd=3, 
  xlim=range(days[days >= 0]), 
  ylim=c(0.8, 1.2),
  log='y')
abline(h=seq(0, 8, 0.1), lty=2, col=rgb(0, 0, 0, 0.2))
abline(h=seq(0, 8, 0.5), lty=4, col=rgb(0, 0, 0, 0.8))
abline(v=0, lwd=2)
lines(days, tot / tot[year.start], lwd=3)
lines(days, mine / mine[year.start], col='cadetblue', lwd=3)
lines(days, fid / fid[year.start], col='darkgreen', lwd=3)
lines(days, et / et[year.start], col='purple', lwd=3)
lines(days, self.mng, col='sienna', lwd=3)
lines(days, f1k / f1k[year.start], lwd=3, col='maroon')
lines(0:(length(sim1) - 1), sim1, lwd=3, col='hotpink')
lines(0:(length(sim2) - 1), sim2, lwd=3, col='limegreen')
lines(0:(length(sim3) - 1), sim3, lwd=3, col='cyan')
lines(0:(length(sim4) - 1), sim4, lwd=3, col='burlywood4')
lines(0:(length(sim5) - 1), sim5, lwd=3, col='coral')
lines(0:(length(extr) - 1), extr, lwd=2, col='green')
lines(days, (1 + tot - sp) / (1 + tot - sp)[year.start], col=2)
legend(
  'topleft',
  lty=1,
  lwd=c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1),
  col=c(
    'orange', 'darkgreen', 'purple', 'sienna', 'maroon', 'cadetblue', 'black', 
    'hotpink', 'limegreen', 'cyan', 'burlywood4', 'coral', 'green', 'red'),
  legend=c(
    paste(sp.sh, 'S&P'),
    paste(fid.sh, 'Fid'),
    paste(et.sh, 'ETrade'),
    paste(self.mng.sh, 'Self-Managed'),
    paste(f1k.sh, '401(k) All'),
    paste(mine.sh, 'Mine'),
    paste(tot.sh, 'Total'),
    paste(sim1.sh, sim1.name),
    paste(sim2.sh, sim2.name),
    paste(sim3.sh, sim3.name),
    paste(sim4.sh, sim4.name),
    paste(sim5.sh, sim5.name),
    paste(extr.sh, 'extrema'),
    paste('Diff(SP)')),
  bg=rgb(1, 1, 1, alpha=0.8))
#--------------------------------------------------------------------------------


exp.mod <- function(funds, x) {
  savings.exp.mod <- lm(log(funds) ~ x)
  log.y <- coef(savings.exp.mod)[1] + x*coef(savings.exp.mod)[2]
  exp(log.y)  
}


start <- tot.val[prev.last]
now <- tot.val[length(tot.val)]
low <- min(tot.val[(prev.last):(length(tot.val))])
high <- max(tot.val[(prev.last):(length(tot.val))])


# PLOT 2: Total funds over time --------------------------------------------------
quartz()
x <- 1:length(tot.val)
plot(
  x, 
  tot.val, 
  type='l', 
  ylim=c(0, max(tot.val)), 
  main='1 = 01 Jan 2018')#, log='y')
lines(x, exp.mod(tot.val, x), col=rgb(0, 0, 0, 1))
abline(h=start, col='grey')
abline(h=now, col='grey')
abline(h=low, col=2)
abline(h=high, col=3)
old.f1k.val <- f1k.val
old.f1k.val[790:length(f1k.val)] <- 0
lines(fid.val + old.f1k.val, col=4)
lines(x, exp.mod(fid.val + old.f1k.val, x), col=rgb(0, 0, 1, 1))
lines(f1k.val[790:length(f1k.val)], col=5)
lines(
  x[790:length(f1k.val) - 789], 
  exp.mod(f1k.val[790:length(f1k.val)], x[790:length(f1k.val)]), 
  col=rgb(0, 1, 1, 1))
lines(et.val, col=6)
lines(x, exp.mod(et.val, x), col=rgb(1, 0, 1, 1))
smv <- self.mng.val[self.mng.val > 1]
x <- 1:length(smv)
lines(smv, col='sienna')
lines(x, exp.mod(smv, x), col='sienna')
legend(
  'topleft',
  lty=1,
  col=c('black', 'blue', 'magenta', 'cyan', 'sienna'),
  legend=c(
    paste('Total: ', tot.val[length(tot.val)]),
    'Fidelity', 
    'ETrade', 
    '401(k)', 
    'Self-Managed'),
  bg='white')
#--------------------------------------------------------------------------------


x <- 1:length(tot.val)
savings.exp.mod <- lm(log(tot.val) ~ x)
x <- prev.last + seq(250, 3750, by=250)[1:13]
log.y <- coef(savings.exp.mod)[1] + x*coef(savings.exp.mod)[2]
y <- round(exp(log.y))
for (p in y) { cat(p, '\n') }

(fid.amt <- fid.val[length(fid.val)])                # [179159 217844]
(et.amt <- et.val[length(et.val)])                   # [130010 154288]
(f1k.total <- f1k.val[length(f1k.val)])              # [ 18063  41548] 
(self.mng.amt <- self.mng.val[length(self.mng.val)]) # [  9514  16089]
(round(sim1.val) + (sim1.res))
(round(sim2.val) + (sim2.res))
(round(sim3.val) + (sim3.res))
(round(sim4.val) + (sim4.res))
(round(sim5.val) + (sim5.res))
(pct.invested)

n <- length(sim1)
NDAYS.LOOKBACK <- 8 * 5
sim1[n] / sim1[max(n - NDAYS.LOOKBACK, 1)]
sim2[n] / sim2[max(n - NDAYS.LOOKBACK, 1)]
sim3[n] / sim3[max(n - NDAYS.LOOKBACK, 1)]
sim4[n] / sim4[max(n - NDAYS.LOOKBACK, 1)]
sim5[n] / sim5[max(n - NDAYS.LOOKBACK, 1)]

n <- length(fid.val)
fid[n] / fid[n - NDAYS.LOOKBACK]
n <- length(et.val)
et[n] / et[n - NDAYS.LOOKBACK]
n <- length(self.mng.val)
self.mng[n] / self.mng[n - NDAYS.LOOKBACK]


# Perf:    1st 2nd 3rd  Pts(3x2x1)  : weeks 12
# sim1       6   2   3  25
# fid            1   4   6
# et         3   7   2  25
# self.mng   3   2   3  16


#--------------------------------------------------------
# To Save New `df` at the end of each year:
#df.test <- data.frame(
#  sp.actual=sp.actual, fid.in=fid.in, fid.val=fid.val, f1k.in=f1k.in, 
#  f1k.val=f1k.val, et.in=et.in, et.val=et.val, self.mng.in=self.mng.in, 
#  self.mng.val=self.mng.val)
#matplot(df.test, type='l', lty=1, col=1:7)
#legend('topleft', lty=1, col=1:7, legend=names(df.test))
# Backed up previous?
#save.to = '~/Learning/marketModeling/data/myHistoricTest.RData'
#save(df.test, file=save.to)
#rm(list=ls())
#load(save.to)
#ls()
#matplot(df.test, type='l', lty=1, col=1:7)
#legend('topleft', lty=1, col=1:7, legend=names(df.test))
# OK?
#df <- df.test
#save(df, file='~/Learning/marketModeling/data/myHistoric.RData')
# Delete test and backups
