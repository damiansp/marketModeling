#---------#---------#---------#---------#---------#---------#---------#---------
load('~/Learning/marketModeling/data/myHistoric.RData')
head(df)
tail(df)

get.daily.change <- function(x) {
  (x[2:length(x)] - x[1:(length(x) - 1)]) / x[1:(length(x) - 1)]
}


prev.last <- dim(df)[1]


sp.actual <- c(
  df$sp.actual,    3824,  3853,  3808,  3895)

fid.in <- c(
  df$fid.in,     161785,161785,161785,161785) #
fid.val <- c(
  df$fid.val,    172350,175073,172978,176119) #

f1k.in <- c(
  df$f1k.in,      18031, 18031, 18031, 18031) # (961.47)
f1k.val <- c(
  df$f1k.val,     17787, 17715, 17856, 17766) #

self.mng.in <- c(
  df$self.mng.in,  9350,  9350,  9350,  9350)  ##
self.mng.val <- c(
  df$self.mng.val, 9189,  9333,  9243,  9351)

et.in <- c(
  df$et.in,       76890, 78443, 78443, 78443) #
et.val <-c(
  df$et.val,     123916,128480,126726,128449) #
  
adl <- (c(
          100000, 99991,100215, 99629,100129)
  / 100000)
rsi <- (c(
          100000,100000,100000,100000,100000)
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
  ylim=c(0.95, 1.05),
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

(fid.amt <- fid.val[length(fid.val)])                # [80533, 258258]
(et.amt <- et.val[length(et.val)])                   # [92495, 135486]
(f1k.total <- f1k.val[length(f1k.val)])              # [    0,  17553] 
(self.mng.amt <- self.mng.val[length(self.mng.val)]) # [ 4321,  10571]



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






