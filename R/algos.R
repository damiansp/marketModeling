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
  df$sp.actual,   5827,  5836,  5843,  5950,  5937,  5997,  6049, 6086,  6119,
    6101,  6012,  6068,  6039,  6071,  6041,  5995,  6038,  6061, 6084,  6025,
    6066,  6069,  6052,  6115,  6115,  6125,  6144,  6118,  6013, 5983,  5955,
    5956,  5862,  5955,  5850,  5778,  5843,  5739,  5770,  5615, 5572,  5599,
    5522,  5639,  5675,  5615,  5675,  5663,  5668,  5768,  5777, 5712,  5693,
    5581)

fid.in <- c(
  df$fid.in,    172299,172299,172299,172299,172299,172299,172299,172299,172299,
  172299,172299,172299,172299,172299,172299,172299,172299,172299,172299,172299,
  172299,172299,172299,172299,172299,172299,172299,172299,172299,172299,172299,
  172299,172299,172299,172299,172299,172299,172299,172299,172299,172299,172299,
  172299,172299,172299,172299,172299,172299,172299,172299,172299,172299,172299,
  172299) #
fid.val <- c(
  df$fid.val,   259076,259900,260932,262076,263078,263121,264259,264409,265029,
  264323,265178,266072,266541,268886,266966,266306,266836,267983,268732,266087,
  268507,268281,268110,270148,268388,269065,268667,266446,263805,263235,261933,
  261355,258084,259909,256280,256129,258779,253878,256583,249283,249278,252498,
  247741,254830,256844,252872,256130,254530,255483,262252,262641,259418,258463,
  254475) #

f1k.in <- c(
  df$f1k.in,     52664, 52664, 52664, 52664, 53385, 53385, 53385, 53385, 53385,
   54088, 54088, 54088, 54088, 54088, 54088, 54088, 54088, 54088, 54794, 54794,
   54794, 54794, 54794, 54794, 54794, 54794, 54794, 54794, 55489, 55489, 55489,
   55489, 55489, 55489, 55489, 55489, 55489, 55489, 55489, 55489, 55489, 55489,
   55489, 55489, 56261, 56261, 56261, 56261, 56261, 56261, 56261, 56261, 56261,
   56261) # (1125.13)
f1k.val <- c(
  df$f1k.val,    82324, 82169, 82228, 82282, 83645, 83548, 84187, 84966, 85417,
   86877, 86687, 85482, 86377, 86028, 86597, 86145, 85505, 86165, 87586, 86826,
   87055, 87728, 87640, 87666, 88496, 88400, 88401, 88703, 89247, 87588, 87074,
   86512, 86566, 84856, 86022, 84215, 83600, 85664, 84111, 84519, 81975, 81798,
   82429, 80876, 82952, 83761, 82618, 84794, 84480, 84617, 86436, 86506, 85319,
   84878) #

self.mng.in <- c(
  df$self.mng.in,13857, 13857, 13857, 13857, 13857, 13857, 13857, 13857, 13857,
   13857, 13857, 13857, 13857, 13857, 13857, 13857, 13857, 13857, 13857, 13857,
   13857, 13857, 13857, 13857, 13857, 13857, 13857, 13857, 13857, 13857, 13857,
   13857, 13857, 13857, 13857, 13857, 17835, 17835, 17835, 17835, 17835, 17835,
   17835, 17835, 17835, 17835, 17835, 17835, 17835, 17835, 17835, 17835, 17835,
   17835)  #
self.mng.val <- c(
  df$self.mng.val, 
                 17468, 17480, 17547, 17681, 17776, 17783, 17934, 17906, 17923,
   17937, 17950, 17966, 17951, 18100, 17981, 17925, 18049, 18151, 18153, 18079,
   18230, 18310, 18420, 18626, 18544, 18695, 18750, 18598, 18341, 18240, 18122,
   18091, 17754, 17846, 17434, 17415, 22661, 22389, 22488, 21777, 21853, 22120,
   21613, 22269, 22704, 22274, 22682, 22508, 22536, 23233, 23172, 22806, 22608,
   22134)  #

et.in <- c(
  df$et.in,      92607, 92997, 93120, 93120, 93120, 93120, 93120, 93120, 93120,
   93120, 93120, 93120, 93120, 93330, 93330, 93330, 93330, 93330, 93330, 93330,
   93330, 93330, 93330, 93474, 93474, 93474, 93474, 93474, 93474, 93867, 94356,
   94356, 94849, 94849, 94849, 95147, 95147, 95147, 95147, 95147, 95818, 95818,
   95937, 95937, 95937, 95937, 95937, 95937, 95937, 95937, 95937, 95937, 95937,
   96450) #
et.val <-c(
  df$et.val,    211043,212988,214224,214870,216300,216008,216174,216336,216595,
  215513,217232,218083,218359,220065,218102,217139,217036,217815,218562,215875,
  217701,217972,217679,219633,217747,216918,216923,215338,213512,214406,215088,
  214842,213440,215769,211701,211811,213685,208814,211161,204546,206256,209575,
  205102,211458,212928,208711,211230,209235,209926,216520,216205,212795,211339,
  208292) #

sim1 <- (c(
         100000,100000,100181,100920,102382,102281,101603,103160,104907,104306,
  104823,106238,107565,107041,109842,109042,107404,107212,110907,112705,111026,
  113902,110691,109092,109888,107978,109033,109033,106661,105044,103855,104994,
  106595, 99771,101143, 95632, 95166, 97095, 92383, 93447, 86507, 89072, 93245,
   87294, 92401, 94503, 90356, 94051, 92502, 94408,100349,100298, 97681, 96326,
   93529)
  / 100000)
sim1.res <- 29054
mw <- read.csv('~/Downloads/Holdings - Damian Satterthwaite-Phillips.csv')
mw$Value <- str_replace(mw$Value, ",", "")
mw$Value <- as.numeric(str_replace(mw$Value, "\\$", ""))
sim1.val <- sum(mw$Value)
sim1.name <- 'adelaide'

sim2 <- (c(
         100000,100000,100158,100885,102728,102840,102406,104599,106418,105942,
  106168,106180,109058,108848,110943,109775,107708,107689,109993,111953,109278,
  111321,108203,107087,109465,109914,114203,114203,111413,108263,106084,104194,
  107459,101726,102048, 95862, 96433, 97906, 91932, 93096, 87305, 90154, 93564,
   87291, 93629, 96691, 91832, 95760, 93989, 96026,101238,101461, 98644, 96648,
   93091)
  / 100000)
sim2.res <- -1570
mw <- read.csv('~/Downloads/Holdings - Damian Satterthwaite-Phillips(1).csv')
mw$Value <- str_replace(mw$Value, ",", "")
mw$Value <- as.numeric(str_replace(mw$Value, "\\$", ""))
sim2.val <- sum(mw$Value)
sim2.name <- 'boisterous'

sim3 <- (c(
         100000,100000,100368,101189,102743,102670,102045,103523,105270,104372,
  104101,105513,107951,107052,109536,108744,106686,106743,110295,111389,108375,
  108678,107977,105346,108174,106194,108843,108843,106548,109798,108570,105940,
  107653,100876,101690, 97486, 97629, 99421, 95709, 98280, 93997, 94152, 94379,
   90535, 94619, 98534, 97148, 99559, 96462, 96604, 99618, 98917, 96959, 97144,
   94667)
  / 100000)
sim3.res <- 7206
mw <- read.csv('~/Downloads/Holdings - Damian Satterthwaite-Phillips(2).csv')
mw$Value <- str_replace(mw$Value, ",", "")
mw$Value <- as.numeric(str_replace(mw$Value, "\\$", ""))
sim3.val <- sum(mw$Value)
sim3.name <- 'cantankerous'


sim4 <- (c(
         100000,100000,100403,101316,103055,102786,102118,103324,104838,103757,
  103325,105111,106746,106348,109165,108556,106282,106247,109595,111455,108656,
  109590,108722,106499,110335,108146,110844,110844,108496,113240,112673,109393,
  110120,106259,107288,102506,102410,102956, 98081,100882, 94809, 95977, 99419,
   92178, 97754, 99753, 95185, 98773, 96755, 98748,104460,104824,101361, 98789,
   95433)
  / 100000)
sim4.res <- 21199
mw <- read.csv('~/Downloads/Holdings - Damian Satterthwaite-Phillips(3).csv')
mw$Value <- str_replace(mw$Value, ",", "")
mw$Value <- as.numeric(str_replace(mw$Value, "\\$", ""))
sim4.val <- sum(mw$Value)
sim4.name <- 'deeelite'

sim5 <- (c(
         100000,100000,100329,101080,102579,102888,102376,104699,106291,106069,
  105501,106753,108745,108704,110745,109261,106735,106840,109390,110814,107887,
  109289,107924,107408,110091,109686,111857,111857,109423,108935,107752,106739,
  107837,103843,104795, 99577, 99140,101146, 95783, 97220, 91911, 93789, 97338,
   91695, 98056,101523, 96790,100652, 98850,100448,106316,106341,102724,100948,
   96663)
  / 100000)
sim5.res <- 1763
mw <- read.csv('~/Downloads/Holdings - Damian Satterthwaite-Phillips(4).csv')
mw$Value <- str_replace(mw$Value, ",", "")
mw$Value <- as.numeric(str_replace(mw$Value, "\\$", ""))
sim5.val <- sum(mw$Value)
sim5.name <- 'edjucated'

extr <- (c(
         100000,100000,100000,100497,101274,101045,100579,101267,101191,101483,
   99729, 99185,101753,100747,103563,101707, 98628, 98969,103628,106584,102428,
  107160,101994, 99580,101897,102595,111417,111417,107488,102407, 97749, 93230,
  100231, 94367, 96022, 89898, 88809, 91493, 85395, 88217, 82404, 83606, 86002,
   83217, 88937, 89904, 86645, 89867, 88796, 89465, 95782, 96066, 90150, 87898,
   83953)
  / 100000)
extr.res <- -2588
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
fid.val[n] / fid.val[n - NDAYS.LOOKBACK]
n <- length(et.val)
et.val[n] / et.val[n - NDAYS.LOOKBACK]
n <- length(self.mng.val)
self.mng.val[n] / self.mng.val[n - NDAYS.LOOKBACK]


# Perf:    1st 2nd 3rd  Pts(3x2x1)
# sim1       4   1   2    16
# fid            2   3     7
# et         1   5   1    20
# self.mng   3   2   3    16 --?


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
