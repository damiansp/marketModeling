#---------#---------#---------#---------#---------#---------#---------#---------
rm(list = ls())
source('~/Desktop/marketStudies/R/stocks.R')

RSI <- 63.93

macdHisto <- c(
  -51.081,-40.236,-40.248,-26.475,-13.121,  0.593, 12.105, 21.920, 28.504, 30.697,
   35.788, 39.555, 44.333, 51.849, 50.450, 49.208, 47.780, 48.992, 41.742, 44.053,
   48.018, 50.002)
hn <- length(macdHisto)
histDir <- ifelse(macdHisto[hn] >= macdHisto[hn - 1], 'up', 'down')
#histoDiff <- diff(macdHisto)

SP <- c(
  2507,2510,2448,2532,2550,2574,2585,2597,2596,2583,2610,2616,2636,2671,2633,2639,
  2642,2665,2652,2640,2681,2704,2707)

dailyChange <- function(x) {
  (x[2:length(x)] - x[1:(length(x) - 1)]) / x[1:(length(x) - 1)]
}

SP <- SP / SP[1]
SPd <- dailyChange(SP)

FidIn <- c(
  44468,44468,44468,44468,44468,44468,44468,44468,44468,44468,44468,45015,45015,
  45015,45015,45015,45015,45015,45015,45015,45015,45015,45427) #
FidVal <- c(
  45906,46052,45493,46088,47191,47852,48916,49049,48798,48236,49201,49406,49967,
  50698,48705,48711,49409,49903,49040,48365,49351,50699,51076)
Fid <- FidVal / FidIn
Fid <- Fid / Fid[1]
Fidd <- dailyChange(Fid)

f1kIn <- c(
  42902,42902,42902,42902,42902,42902,42902,42902,42902,42902,43833,43833,43833,
  43833,43833,43833,43833,43833,43833,43833,43833,44758,44758) #
f1kVal <- c(
  42436,42647,42639,42779,42780,43015,43187,43337,43413,43394,44316,44471,44646,
  44647,44866,44594,44592,44680,44640,44618,44619,45550,46084)
f1k <- f1kVal / f1kIn
f1k <- f1k / f1k[1]
f1kd <- dailyChange(f1k)

OHIn <- c(
  35270,35270,35270,35270,35270,35270,35270,35270,35270,35270,35270,35270,35270,
  35270,35270,35270,35270,35270,35270,35270,35270,35270,35270) #
  
OHVal <- c(
  35943,35992,35768,36109,37060,37368,37821,37530,37526,37167,37167,37114,37539,
  38110,37414,37624,37624,37624,37624,37624,37624,38228,38620)
OH <- OHVal / OHIn
OH <- OH / OH[1]
OHd <- dailyChange(OH)

totalFunds <- FidVal + f1kVal + OHVal


# StockAdvisorBest (bestVice)
m1 <- (c(
  100000,100070, 99875,100184,100264,101004,101717,102476,102538,101972,103017,
  103532,103739,104730,102937,102745,103366,104059,100474,100343,102561,105615,
  105145)
  / 100000)
  # Recent okta nvda dis bkng amzn ha tdg aapl fico masi tlk 
m1.name <- 'Stock Advisor (Best)'
  
# StockAdvisorBestI/O (bestViceIO)
m2 <- (c(
  100000,100050, 99908,100132,100245,100276,100727,101402,101444,101074,101074,
  101657,102044,103922,100436,101226,101237,101240,101227,101247,101249,105138,
  104156)
  / 100000) 
m2.name <- 'Stock Advisor (I/O)'

# BlastOff (NugOWar)
m3 <- (c(
  100000,100137,100120,100249,104779,108012,110785,109025,108080,107225,111803,
  110915,114130,117553,110143,110172,113035,115918,113852,110846,115919,121610,
  122838)
  / 100000)
m3.name <- 'Blast Off'

# BlastOffI/O (bodyOdorIO)
m4 <- (c(
  100000,100140, 99886,100249,104624,107774,110390,108433,107423,106187,106187,
  104890,107233,110191,103863,104891,104903,104918,104911,104900,104923,108816,
  110128)
  / 100000)
m4.name <- 'Blast Off (I/O)'
  
# MixedBag (nn mod; no IO except devs...)
m5 <- (c(
  100000,100257, 99764,100108,104034,106448,109642,110059,109630,108669,111093,
  110224,113017,116620,109466,109202,111683,114246,112030,109756,114351,119980,
  120889)
  / 100000)
m5.name <- 'Mixed Bag (NN)'  

n <- length(m1)
totin <- FidIn + f1kIn + OHIn
totval <- FidVal + f1kVal + OHVal
tot <- totval / totin
tot <- tot / tot[1]
minein <- FidIn + OHIn
mineval <- FidVal + OHVal
mine <- mineval / minein
mine <- mine / mine[1]

m1 <- m1 / m1[1]
m2 <- m2 / m2[1]
m3 <- m3 / m3[1]
m4 <- m4 / m4[1]
m5 <- m5 / m5[1]


# Calculate Sharpe Ratios
Sharpe <- function(returns, window=length(returns)) {
  if (length(returns) > window) {
    returns <- returns[(length(returns) - window):length(returns)]	  	
  }
	
  returns <- returns[!is.na(returns)]
  (sqrt(252) * mean(returns)) / sd(returns)
}

get.daily.returns <- function(x) { 
  n <- length(x)
  x[2:n] / x[1:(n - 1)] - 1 
}

SP.sh  <- round(Sharpe(get.daily.returns(SP)), 3)
tot.sh <- round(Sharpe(get.daily.returns(tot)), 3)
mine.sh <- round(Sharpe(get.daily.returns(mine)), 3)
m1.sh  <- round(Sharpe(get.daily.returns(m1)), 3)
m2.sh  <- round(Sharpe(get.daily.returns(m2)), 3)
m3.sh  <- round(Sharpe(get.daily.returns(m3)), 3)
m4.sh  <- round(Sharpe(get.daily.returns(m4)), 3)
m5.sh  <- round(Sharpe(get.daily.returns(m5)), 3)

plot(m1, 
     type='l', 
     ylim=range(c(SP,  tot, 1 + tot - SP,  m1,  m2, m3, m4, m5)), 
     lwd=3, 
     log='y')
abline(h=1, lty=4)
abline(h=seq(0, 3, 0.1), lty=2, col=rgb(0, 0, 0, 0.2))
lines(tot, col=2, lwd=3)
lines(mine, col='cadetblue', lwd=3)
lines(SP, col='yellow', lwd=3)
lines(m2, lwd=3, col='coral')
lines(m3, lwd=3, col='blue')
lines(m4, lwd=3, col='purple')
lines(m5, lwd=3, col='cyan')
lines(1 + tot - SP, col='#880000')
legend(
  'topleft', 
  lty=1, 
  lwd=c(3, 3, 3, 3, 3, 3, 3, 3, 1), 
  col=c('black', 'red ', 'cadetblue', 'yellow',  'coral', 'blue', 'purple', 'cyan', 
        '#880000'),
  legend=c(paste(m1.sh, m1.name), 
           paste(tot.sh, 'Actual'), 
           paste(mine.sh, 'Mine'),
           paste(SP.sh, 'S&P'), 
           paste(m2.sh, m2.name), 
           paste(m3.sh, m3.name), 
           paste(m4.sh, m4.name), 
           paste(m5.sh, m5.name), 
           'Actual Diff'),
  bty='n')
# ______________________________________________________________________________


quartz()
x <- 1:length(totalFunds)
plot(x, 
     totalFunds, 
     type = 'l', 
     ylim=c(0, max(totalFunds)), 
     main='1 = 01 Jan 2018')#, log='y')
#savings.mod <- lm(totalFunds ~ x)

exp.mod <- function(funds, x) {
  savings.exp.mod <- lm(log(funds) ~ x)
  log.y <- coef(savings.exp.mod)[1] + x*coef(savings.exp.mod)[2]
  exp(log.y)  
}


#abline(savings.mod, col=rgb(0, 0, 0, 0.25))
lines(x, exp.mod(totalFunds, x), col=rgb(0, 0, 0, 0.25))
abline(h=totalFunds[length(totalFunds)], col='grey')
abline(h=totalFunds[1], col='grey')
abline(h=min(totalFunds), col=2)
abline(h=max(totalFunds), col=3)

lines(FidVal, col=4)
lines(x, exp.mod(FidVal, x), col=rgb(0, 0, 1, 0.25))

lines(f1kVal, col=5)
lines(x, exp.mod(f1kVal, x), col=rgb(0, 1, 1, 0.25))

lines(OHVal, col=6)
lines(x, exp.mod(OHVal, x), col=rgb(1, 0, 1, 0.25))

legend('topleft', 
       text.col=c('green', 'black', 'red'), 
       legend=c(sprintf('$%d', max(totalFunds)),
                sprintf('$%d', totalFunds[length(totalFunds)]),
                sprintf('$%d', min(totalFunds))), 
       bg=rgb(1, 1, 1, 0.75))
legend('bottomright',
       lty=1,
       col=c(4, 5, 6, 1),
       legend=c('Fidelity', '401(k)', 'E*Trade', 'Total'),
       bg=rgb(1, 1, 1, 0.75))

day <- length(f1kIn) - 1 # length(<some new algo>) - 1
# Pad with leading NAs to make all vectors of equal length
tLength <- length(SP)
#Fid     <- c(rep(NA, tLength - length(Fid)),     Fid)
#OH      <- c(rep(NA, tLength - length(OH)),      OH)
#f1k     <- c(rep(NA, tLength - length(f1k)),     f1k)
days <- seq(to=day, by=1, length=tLength)

d <- data.frame(SP, Fid, OH, f1k)
realWts <- matrix(data=c(FidVal, OHVal, f1kVal), ncol=3)
realWts <- realWts / rowSums(realWts, na.rm = T)
realWts[is.na(realWts)] <- 0 
nWts <- length(realWts[, 1])
meanReal <- (Fid[(length(Fid) - nWts + 1):length(Fid)] * realWts[, 1] +
             OH[(length(OH)   - nWts + 1):length(OH)]  * realWts[, 2] +
             f1k[(length(OH)  - nWts + 1):length(f1k)] * realWts[, 3])
#meanReal <- c(rep(NA, 249), meanReal)
colors <- c(SP='yellow', Fid='red', OH='green', f1k='#cc0088', meanReal='grey')			   
quartz()
matplot(days, d, type = 'l', lty = 1, col = colors, 
		lwd = 2, main = 'Performance of Funds (2018)',
	  	xlab = 'Market Days Elapsed', ylab = 'Times Original Value', 
	  	log = 'y', 
	  	xlim = c(0, day + 5), 
	  	ylim = c(min(d[days >= 0, ], meanReal - SP + 1, na.rm=T), 
	  			 max(d[days >= 0, ], meanReal - SP + 1, na.rm=T)))
lines(days, (meanReal - SP) + 1, lwd=1)
lines(days, d[, 'SP'], col = 'yellow', lwd=3)
lines(days, meanReal, col = colors['meanReal'], lwd=1)

abline(h = 1, lwd = 2)
abline(v = 0, lwd = 2)
#abline(v=c(30, 75), lty=4, col=rgb(0, 0, 0, 0.4))
abline(h=seq(0.1, 5.0, 0.1), col=rgb(0, 0, 0, 0.4), lty=2:1)

SP.Sharpe   <- round(Sharpe(SPd),  3)
Fid.Sharpe  <- round(Sharpe(Fidd), 3)
OH.Sharpe   <- round(Sharpe(OHd),  3)
f1k.Sharpe  <- round(Sharpe(f1kd), 3)
real.Sharpe <- round(Sharpe(dailyChange(meanReal)), 3)   

SP.Sharpe1yr   <- round(Sharpe(SPd,  250), 3)
Fid.Sharpe1yr  <- round(Sharpe(Fidd, 250), 3)
OH.Sharpe1yr   <- round(Sharpe(OHd,  250), 3)
f1k.Sharpe1yr  <- round(Sharpe(f1kd, 250), 3)
real.Sharpe1yr <- round(Sharpe(dailyChange(meanReal), 250), 3)

legend(
  'topleft', 
  title='All data',
  legend=c(
    sprintf('%11s: %+.3f', 'SP500',  SP.Sharpe), 
    sprintf('%15s: %+.3f', 'Fid',    Fid.Sharpe), 
    sprintf('%13s: %+.3f', 'OH',     OH.Sharpe), 
    sprintf('%15s: %+.3f', 'f1k',    f1k.Sharpe), 
    sprintf('%s: %+.3f', 'meanReal', real.Sharpe),
    sprintf('%s', 'diff')),
  lty=1, 
  col=c(colors, 'black'), 
  lwd=c(2, 2, 2, 2, 1, 1), 
  bg=rgb(1, 1, 1, 0.8), 
  cex=0.6)
legend(
  'bottomleft', 
  title='Last year only',
  legend = c(
    sprintf('%11s: %+.3f', 'SP500',  SP.Sharpe1yr), 
    sprintf('%15s: %+.3f', 'Fid',    Fid.Sharpe1yr), 
    sprintf('%13s: %+.3f', 'OH',     OH.Sharpe1yr), 
    sprintf('%15s: %+.3f', 'f1k',    f1k.Sharpe1yr), 
    sprintf('%s: %+.3f', 'meanReal', real.Sharpe1yr),
    sprintf('%s', 'diff')),
  lty=1, 
  col=c(colors, 'black'), 
  lwd=c(2, 2, 2, 2, 2, 1, 1), 
  bg=rgb(1, 1, 1, 0.8), 
  cex=0.6)




#----------------------------------------
today.signal <- 'rising' # HARD CODE
#today.signal <- 'falling'  # HARD CODE

signals <- c('none', 'peak', 'falling', 'bottom', 'rising')
actions <- c('none', 'sell', 'sell',    'buy',    'buy')
percents <- c(0,      0.0500, 0.9500,    0.9500,   0.8973) # stocks
#percents <- c(0,      0,      0.319,     0.9285,   0) # 401(k)

ind <- which(signals == today.signal)
action <- actions[ind]
percent <- percents[ind]

make.decision <- function(action, invested, reserve) {
  if (action == 'buy') {
    spend.amt <- reserve * percent
    cat('Today\'s signal: ', today.signal, '--', action, ' ', percent, ': ', 
        spend.amt, sep = '')
  } else if (action == 'sell') {
    sell.amt <- invested * percent
    cat('Today\'s signal: ', today.signal, '--', action, ' ', percent, ': ', 
        sell.amt, sep = '')
  } else {
    cat('Today\'s signal: ', today.signal, '--', action, ' ', 
        '(Sell only if stock gives signal; buy to replace value)', sep = '')
  }	
}

#----------------------------------------


# OH (e*trade) - traditional strategy
oh.amt <- 37625 # [29240, 37625]
oh.inv <- 0
OH.RESERVE <- 201
(total.amt <- oh.amt - OH.RESERVE)
(reserve <- total.amt - oh.inv)
make.decision(action, oh.inv, reserve)  # B 33581


# Fid (buy and hold form Partnership Portfolio)
fid.amt <- 49351 # [33193, 49351]
(roll.inv <- 15279 - (4714)) # total - cash
(roth.inv <- 31381 - (6385))
(si.inv   <- 2700 - 1189)
fid.inv <- roll.inv + roth.inv + si.inv
FID.RESERVE <- 296
(total.amt <- fid.amt - FID.RESERVE)
(reserve <- total.amt - fid.inv)
make.decision(action, fid.inv, reserve) # Buy: 10752


# For 401(k): 
(f1k.total <- f1kVal[length(f1kVal)]) # 40585
RESERVE <- 243
(fik.total <- f1k.total - RESERVE)    # 37692
#----------------------------------------






x <- 1:length(totalFunds)
savings.exp.mod <- lm(log(totalFunds) ~ x)
x <- seq(250, 3750, by=250)[1:8]
log.y <- coef(savings.exp.mod)[1] + x*coef(savings.exp.mod)[2]
(y <- round(exp(log.y)))

#sample(stocks, 3)