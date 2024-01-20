#---------#---------#---------#---------#---------#---------#---------#---------
load('~/Learning/marketModeling/data/myHistoric.RData')
head(df)
tail(df)

get.daily.change <- function(x) {
  (x[2:length(x)] - x[1:(length(x) - 1)]) / x[1:(length(x) - 1)]
}


prev.last <- dim(df)[1]


sp.actual <- c(
  df$sp.actual,     4743,  4705,  4689,  4697,  4764,  4757,  4783,  4780,  4784,
      4766,  4739,  4781,  4840)

fid.in <- c(
  df$fid.in,      167137,167137,167137,167137,167137,167137,167137,167137,167137,
    167137,167137,167137,167137) #
fid.val <- c(
  df$fid.val,     226255,222123,222288,222370,226164,226085,226787,226918,226111,
    224232,223153,225424,226784) #

f1k.in <- c(
  df$f1k.in,       36528, 36528, 36528, 36528, 36528, 36528, 36528, 36528, 36528,
     36528, 36528, 36528, 37334) # (1062.63)
f1k.val <- c(
  df$f1k.val,      48986, 48283, 47658, 47523, 47500, 48441, 48481, 48834, 48889,
     48787, 48421, 48149, 49621) #

self.mng.in <- c(
  df$self.mng.in,  13857, 13857, 13857, 13857, 13857, 13857, 13857, 13857, 13857,
     13857, 13857, 13857, 13857)  #
self.mng.val <- c(
  df$self.mng.val, 16169, 15852, 15887, 15861, 16152, 16065, 16106, 16048, 15949,
     15728, 15640, 15791, 15827)  #

et.in <- c(
  df$et.in,        86278, 86278, 86278, 86278, 86278, 86278, 86278, 86278, 86588,
     86588, 86588, 86588, 86588) #
et.val <-c(
  df$et.val,      182169,178447,178828,178839,181753,181913,181774,181776,181803,
    179567,178132,180171,181513) #

sim1 <- (c(
           100000,100023, 99980, 99994, 99987,100290,100240,100401,100414,100410,
    100359,100355,100424,100650)
  / 100000)
sim1.name <- 'adelaide'

sim2 <- (c(
           100000,100005, 99939, 99944, 99916,100348,100284,100473,100482,100456,
    100392,100352,100383,100598)
  / 100000)
sim2.name <- 'aei'

sim3 <- (c(
           100000, 99994, 99910, 99915, 99882,100396,100317,100507,100498,100453,
    100374,100335,100273,100497)
  / 100000)
sim3.name <- 'simsims'


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
sim1.sh      <- get.sharpe.from.daily.values(sim1)
sim2.sh      <- get.sharpe.from.daily.values(sim2)
sim3.sh      <- get.sharpe.from.daily.values(sim3)

# PLOT 1: This year's returns ---------------------------------------------------
plot(
  days, 
  sp / sp[year.start], 
  type='l', 
  col='orange',
  lwd=3, 
  xlim=range(days[days >= 0]), 
  ylim=c(0.9, 1.1),
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
lines(days, (1 + tot - sp) / (1 + tot - sp)[year.start], col=2)
legend(
  'topleft',
  lty=1,
  lwd=c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1),
  col=c(
    'orange', 'darkgreen', 'purple', 'sienna', 'maroon', 'cadetblue', 'black', 
    'hotpink', 'limegreen', 'cyan', 'red'),
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
#---------------------------------------------------------------------------------


x <- 1:length(tot.val)
savings.exp.mod <- lm(log(tot.val) ~ x)
x <- prev.last + seq(250, 3750, by=250)[1:12]
log.y <- coef(savings.exp.mod)[1] + x*coef(savings.exp.mod)[2]
y <- round(exp(log.y))
for (p in y) { cat(p, '\n') }

(fid.amt <- fid.val[length(fid.val)])                # [179159 217844]
(et.amt <- et.val[length(et.val)])                   # [130010 154288]
(f1k.total <- f1k.val[length(f1k.val)])              # [ 18063  41548] 
(self.mng.amt <- self.mng.val[length(self.mng.val)]) # [  9514  16089]
(sim1[length(sim1)] * 100000)
(sim2[length(sim2)] * 100000)
(sim3[length(sim3)] * 100000)
(pct.invested)

n <- length(sim1)
NDAYS.LOOKBACK <- 5
sim1[n] / sim1[n - NDAYS.LOOKBACK]
sim2[n] / sim2[n - NDAYS.LOOKBACK]
sim3[n] / sim3[n - NDAYS.LOOKBACK]


#--------------------------------------------------------
# To Save New `df` at the end of each year:
#df.test <- data.frame(
#  sp.actual=sp.actual, fid.in=fid.in, fid.val=fid.val, f1k.in=f1k.in, 
#  f1k.val=f1k.val, et.in=et.in, et.val=et.val, self.mng.in=self.mng.in, 
#  self.mng.val=self.mng.val)
#matplot(df.test, type='l', lty=1, col=1:7)
#legend('topleft', lty=1, col=1:7, legend=names(df.test))
# Backed up previous?
#save(df.test, file='~/Learning/marketModeling/data/myHistoricTest.RData')
#rm(list=ls())
#load('~/Learning/marketModeling/data/myHistoricTest.RData')
#ls()
#matplot(df.test, type='l', lty=1, col=1:7)
#legend('topleft', lty=1, col=1:7, legend=names(df.test))
# OK?
#df <- df.test
#save(df, file='~/Learning/marketModeling/data/myHistoric.RData')
# Delete test and backups
