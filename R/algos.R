#---------#---------#---------#---------#---------#---------#---------#---------
load('~/Learning/marketModeling/data/myHistoric.RData')
head(df)
tail(df)

get.daily.change <- function(x) {
  (x[2:length(x)] - x[1:(length(x) - 1)]) / x[1:(length(x) - 1)]
}


prev.last <- dim(df)[1]


sp.actual <- c(
  df$sp.actual,    3824,  3853,  3808,  3895,  3892,  3919,  3970,  3987,  3999,  
     3991,  3929,  3899,  3973,  4020,  4017,  4016,  4060,  4071,  4018,  4077,
     4119,  4180,  4136,  4111,  4164,  4118,  4082,  4090,  4137,  4136,  4148,
     4090, 4079)

fid.in <- c(
  df$fid.in,     161785,161785,161785,161785,161785,161785,161785,162813,162813,
   162813,162813,162813,162813,162813,162813,162813,162813,162813,164033,164033,
   164033,164033,164033,164033,164033,164033,164033,164033,164033,164744,164744,
   164744,164744) #
fid.val <- c(
  df$fid.val,    172350,175073,172978,176119,179159,182181,185241,188878,190980,
   192214,188563,187137,191483,194849,193782,193310,196551,199449,197584,201990,
   205594,210527,207532,206046,207349,205374,203592,202266,202929,204324,206660,
   204405,203374) #

f1k.in <- c(
  df$f1k.in,      18031, 18031, 18031, 18031, 18031, 18031, 18031, 18031, 18031,
    18031, 18031, 18031, 18933, 18933, 18933, 18933, 18933, 18933, 18933, 18933,
    18933, 19964, 19964, 19964, 19964, 19964, 19964, 19964, 19964, 19964, 19964,
    19964, 19964) # (1133.49)
f1k.val <- c(
  df$f1k.val,     17787, 17715, 17856, 17766, 18063, 18219, 18434, 18750, 18928,
    19082, 19184, 18853, 19601, 20022, 20342, 20315, 20283, 20555, 20750, 20388,
    20810, 22316, 22718, 22406, 22175, 22444, 22233, 22010, 21976, 22192, 22195,
    22447, 22104) #

self.mng.in <- c(
  df$self.mng.in,  9350,  9350,  9350,  9350,  9350,  9350,  9350,  9350,  9350,
     9350,  9350,  9350,  9350,  9350,  9350,  9350,  9350,  9350,  9350,  9350,
     9350,  9350,  9350,  9350,  9350,  9350,  9350,  9350,  9350,  9350,  9350,
     9350,  9350)  #
self.mng.val <- c(
  df$self.mng.val, 9189,  9333,  9243,  9351,  9514,  9655,  9818,  9938, 10033,
    10109,  9904,  9785,  9978, 10155, 10111, 10134, 10231, 10379, 10181, 10433,
    10669, 10791, 10687, 10560, 10609, 10508, 10402, 10401, 10461, 10418, 10602,
    10493, 10474)

et.in <- c(
  df$et.in,       76890, 78443, 78443, 78443, 78443, 78443, 78443, 78443, 78443,
    78443, 78443, 78443, 78443, 78443, 78443, 78443, 78443, 78443, 78443, 78443,
    78443, 78443, 78443, 78443, 78443, 78443, 78443, 78443, 78443, 78443, 78443,
    78443, 78443) #
et.val <-c(
  df$et.val,     123916,128480,126726,128449,130010,131945,134582,135693,137333,
   138457,136709,134475,138675,142233,140836,140850,143385,145591,142337,144726,
   148227,152885,149707,147752,149447,147340,146307,145299,146165,146828,149580,
   147802,147197) #
  
rsi <- (c(
          100000,100000,100000,100000,100000,100106,100424,101112,101311,101720,
   102502,101669,101073,102367,103652,103480,103414,104617,105376,103906,105659,
   107500,109571,107624,106488,107881,106734,105919,105350,106198,106639,108007,
   106947,106668)
  / 100000)

adl <- (c(
          100000, 99991,100215, 99629,100129, 99946,100875,100664,101863,102240,
   102191,100411, 99404,100700,101797,101580,102024,102616,102316,100999,103948,
   104631,107992,107866,104794,106118,104872,105104,105180,105220,105264,106306,
   105166,105702)
  / 100000)

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
adl.sh      <- get.sharpe.from.daily.values(adl)
rsi.sh      <- get.sharpe.from.daily.values(rsi)

# PLOT 1: This year's returns -----------------------------------------------------
plot(
  days, 
  sp / sp[year.start], 
  type='l', 
  col='orange',
  lwd=3, 
  xlim=range(days[days >= 0]), 
  ylim=c(0.95, 1.3),
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
lines(days, (1 + tot - sp) / (1 + tot - sp)[year.start + 1], col=2)
lines(0:(length(adl) - 1), adl, lwd=3, col='limegreen')
lines(0:(length(rsi) - 1), rsi, lwd=3, col='hotpink')
legend(
  'topleft',
  lty=1,
  lwd=c(3, 3, 3, 3, 3, 3, 3, 3, 3, 1),
  col=c(
    'orange', 'darkgreen', 'purple', 'sienna', 'maroon', 'cadetblue', 'black', 
    'limegreen', 'hotpink', 'red'),
  legend=c(
    paste(sp.sh, 'S&P'),
    paste(fid.sh, 'Fid'),
    paste(et.sh, 'ETrade'),
    paste(self.mng.sh, 'Self-Managed'),
    paste(f1k.sh, '401(k) All'),
    paste(mine.sh, 'Mine'),
    paste(tot.sh, 'Total'),
    paste(adl.sh, 'Adelaide'),
    paste(rsi.sh, 'RSI'),
    paste('Diff(SP)')),
  bg=rgb(1, 1, 1, alpha=0.8))
#---------------------------------------------------------------------------------


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

(fid.amt <- fid.val[length(fid.val)])                # [179159 210527]
(et.amt <- et.val[length(et.val)])                   # [130010 152885]
(f1k.total <- f1k.val[length(f1k.val)])              # [ 18063  22718] 
(self.mng.amt <- self.mng.val[length(self.mng.val)]) # [  9514  10791]



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






