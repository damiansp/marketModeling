#---------#---------#---------#---------#---------#---------#---------#---------
load('~/Learning/marketModeling/data/myHistoric.RData')
head(df)
tail(df)

dailyChange <- function(x) {
  (x[2:length(x)] - x[1:(length(x) - 1)]) / x[1:(length(x) - 1)]
}

m <- dim(df)[1]
last <- m

# damiansp1976
wlsh.osc <- c(
  -77, -73, -80, -88, -97,-106,-110,-112,-122,-132,-151,-173,-199,-233,-261,-294,
 -325,-357, -375)

SPActual <- c(df$SPActual,
    4797,4794,4701,4696,4677,4670,4713,4726,4659,4663,4577,4533,4483,4398,4410,
    4356,4350,4327,4432)
SP <- SPActual / SPActual[last]
SPd <- dailyChange(SP)

FidIn <- c(df$FidIn,
   57134, 57134, 57134, 57134, 57134, 57134, 57134, 57134, 57134, 57134, 61104,
   61104, 62434, 62434, 62434, 62434, 62434,160743,160743) # 1490 
  
FidVal <- c(df$FidVal,
   94096, 90621, 86085, 86306, 84787, 85416, 87205, 87106, 83943, 83649, 84545,
   84094, 85649, 82388, 83399, 81424, 80533,218550,222221) #
Fid <- FidVal / FidIn
Fid <- Fid / Fid[last]
Fidd <- dailyChange(Fid)

f1kIn <- c(df$f1kIn,
   98297, 98297, 98297, 98297, 98297, 98297, 98297, 98297, 98297, 98297, 98297,
   98297, 98297, 98297, 98297, 98297, 98297,0.6935,0.6935) # 
f1kVal <- c(df$f1kVal,
  157563,158404,157348,153323,153323,151838,151838,153394,153734,150997,151082,
  147813,146496,145095,141741,141741,141741,     1,     1) #
n <- length(f1kIn)
#payment * f1kIn[n] / f1kVal[n] + f1kIn[n]

f1k <- f1kVal / f1kIn
f1k <- f1k / f1k[last]
f1kd <- dailyChange(f1k)
f1k[is.na(f1k)] <- NA
f1kd[is.na(f1kd)] <- NA

OHIn <- c(df$OHIn,
    49269, 49269, 49269, 49269, 49269, 49269, 49269, 49269, 49269, 49269, 49269,
    49269, 49269, 49269, 49269, 49269, 49269, 49269, 49269) # 1143
OHVal <- c(df$OHVal, 
   102557,100422, 97844, 98121, 97375, 97932, 98939, 99051, 97002, 96791, 95665,
    95391, 95163, 93596, 94389, 93136, 92819, 92495, 94639) #
OH <- OHVal / OHIn
OH <- OH / OH[last]
OHd <- dailyChange(OH)
totalFunds <- FidVal + f1kVal + OHVal


# Mon
m1 <- (c(
  100000, 99734, 97021, 92607, 93071, 91736, 92132, 94069, 93531, 90683, 90091,
   87564, 87271, 86460, 84269, 85206, 82751, 81848, 80050, 82238)
  / 100000)
qp.start <- 92
m1.name <- 'sim1'
  
# Tues
m2 <- (c(
  100000, 99519, 96234, 91473, 91946, 90337, 90702, 92795, 92113, 88699, 88134,
   85502, 84905, 84010, 81444, 82682, 80076, 79132, 77316, 79826)
  / 100000) 
m2.name <- 'sim2'

# Wed
m3.raw <- c(
  100000, 99890, 96310, 91049, 91683, 89443, 89246, 91155, 90781, 88037, 87453, 
   85588, 85124, 85345, 82818, 83265, 80641, 79763, 77546, 79590)
m3 <- m3.raw / 100000
m3.name <- 'sim3'

# Thur
m4.raw <- c(
  100000, 99289, 96063, 91314, 91644, 90074, 90327, 92360, 91830, 88380, 88025,
   85472, 84872, 84283, 81799, 82759, 80245, 79590, 78131, 80209)
m4 <- m4.raw / 100000
m4.name <- 'sim4'

# Fri
m5 <- (c(
  100000, 99775, 96651, 91870, 92422, 91307, 92182, 94394, 93768, 89786, 89177,
   86127, 85615, 84895, 81979, 83919, 81050, 80084, 77906, 80564)
  / 100000)
m5.name <- 'sim5'  

# Sat
m6 <- (c(
  100000,100012, 98204, 96007, 96400, 95686, 96971, 97980, 98605, 95083, 95146,
   93567, 93268, 93246, 90907, 92420, 90796, 90046, 88126, 91095)
  / 100000)
m6.name <- 'sim6'  

# Sun
m7 <- (c(
  100000, 99901, 98262, 96147, 96334, 95340, 95359, 96898, 96645, 95102, 94744,
   93378, 92032, 92532, 90635, 91392, 89184, 88026, 85549, 87371)
  / 100000)
m7.name <- 'sim7'  


totin <- FidIn + f1kIn + OHIn
totval <- FidVal + f1kVal + OHVal
tot <- totval / totin 
tot <- tot / tot[last]
minein <- FidIn + OHIn
mineval <- FidVal + OHVal
mine <- mineval / minein
mine <- mine / mine[last]
m1 <- m1 / m1[1]
m2 <- m2 / m2[1]
m3 <- m3 / m3[1]
m4 <- m4 / m4[1]
m5 <- m5 / m5[1]
m6 <- m6 / m6[1]
m7 <- m7 / m7[1]

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

n <- length(m1) 
m <- length(SP)
this.year <- (m-n):m

SP.sh  <- round(Sharpe(get.daily.returns(SP[this.year])), 3)
tot.sh <- round(Sharpe(get.daily.returns(tot[this.year])), 3)
mine.sh <- round(Sharpe(get.daily.returns(mine[this.year])), 3)
m1.sh  <- round(Sharpe(get.daily.returns(m1)), 3)
m2.sh  <- round(Sharpe(get.daily.returns(m2)), 3)
m3.sh  <- round(Sharpe(get.daily.returns(m3)), 3)
m4.sh  <- round(Sharpe(get.daily.returns(m4)), 3)
m5.sh  <- round(Sharpe(get.daily.returns(m5)), 3)
m6.sh  <- round(Sharpe(get.daily.returns(m6)), 3)
m7.sh  <- round(Sharpe(get.daily.returns(m7)), 3)
prev <- m - n
days <- -prev:(n - 1)
new.days <- 0:(n - 1)
plot(new.days,
     m1, 
     type='l', 
     xlim=range(days),
     ylim=range(c(SP,  tot, 1 + tot - SP, mine, m1,  m2, m3, m4, m5, m6, m7)), 
     lwd=3, 
     log='y')
abline(h=seq(0, 8, 0.1), lty=2, col=rgb(0, 0, 0, 0.2))
abline(h=seq(0, 8, 0.5), lty=4, col=rgb(0, 0, 0, 0.8))
abline(v=0, lwd=2)
lines(days, tot, col=2, lwd=3)
lines(days, mine, col='cadetblue', lwd=3)
lines(days, SP, col='yellow', lwd=3)
lines(new.days, m2, lwd=3, col='coral')
lines(new.days, m3, lwd=3, col='blue')
lines(new.days, m4, lwd=3, col='purple')
lines(new.days, m5, lwd=3, col='cyan')
lines(new.days, m6lwd=3, col='grey')
lines(new.days, m7 lwd=3, col='magenta')
lines(days, 1 + tot - SP, col='#880000')
legend(
  'topleft', 
  lty=1, 
  lwd=c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1), 
  col=c('black', 'red ', 'cadetblue', 'yellow',  'coral', 'blue', 'purple', 
        'cyan', 'grey', 'magenta', '#880000'),
  legend=c(paste(m1.sh, m1.name), 
           paste(tot.sh, 'Actual'), 
           paste(mine.sh, 'Mine'),
           paste(SP.sh, 'S&P'), 
           paste(m2.sh, m2.name), 
           paste(m3.sh, m3.name), 
           paste(m4.sh, m4.name), 
           paste(m5.sh, m5.name), 
           paste(m6.sh, m6.name), 
           paste(m7.sh, m7.name), 
           'Actual Diff'),
  bty='n')
abline(v=qp.start)
  
quartz()  
plot(new.days,
     m1, 
     type='l', 
     xlim=range(days[days >= 0]),
     ylim=c(0.6,
            max(c(SP,  tot, 1 + tot - SP, mine, m1,  m2, m3, m4, m5, m6, m7))), 
     lwd=3, 
     log='y')
abline(h=seq(0, 8, 0.1), lty=2, col=rgb(0, 0, 0, 0.2))
abline(h=seq(0, 8, 0.5), lty=4, col=rgb(0, 0, 0, 0.8))
abline(v=0, lwd=2)
lines(days, tot, col=2, lwd=3)
lines(days, mine, col='cadetblue', lwd=3)
lines(days, SP, col='yellow', lwd=3)
lines(new.days, m2, lwd=3, col='coral')
lines(new.days, m3, lwd=3, col='blue')
lines(new.days, m4, lwd=3, col='purple')
lines(new.days, m5, lwd=3, col='cyan')
lines(new.days, m6, lwd=3, col='grey')
lines(new.days, m7, lwd=3, col='magenta')
lines(days, 1 + tot - SP, col='#880000')
legend(
  'topleft', 
  lty=1, 
  lwd=c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1), 
  col=c('black', 'red ', 'cadetblue', 'yellow',  'coral', 'blue', 'purple', 
        'cyan', 'grey', 'magenta', '#880000'),
  legend=c(paste(m1.sh, m1.name), 
           paste(tot.sh, 'Actual'), 
           paste(mine.sh, 'Mine'),
           paste(SP.sh, 'S&P'), 
           paste(m2.sh, m2.name), 
           paste(m3.sh, m3.name), 
           paste(m4.sh, m4.name), 
           paste(m5.sh, m5.name), 
           paste(m6.sh, m6.name), 
           paste(m7.sh, m7.name), 
           'Actual Diff'),
  bty='n')
abline(v=qp.start)
abline(h=1, lty=4)
  
# Downward vals only
sp.start <- which(SP == 1)
n.sp <- length(SP)
sp.rec <- SP[sp.start:n.sp]
n.sp <- length(sp.rec)
sp.ch <- sp.rec[2:n.sp] / sp.rec[1:(n.sp-1)]
m1.ch <- m1[2:n.sp] / m1[1:(n.sp-1)]
m2.ch <- m2[2:n.sp] / m2[1:(n.sp-1)]
m3.ch <- m3[2:n.sp] / m3[1:(n.sp-1)]
m4.ch <- m4[2:n.sp] / m4[1:(n.sp-1)]
m5.ch <- m5[2:n.sp] / m5[1:(n.sp-1)]
m6.ch <- m6[2:n.sp] / m6[1:(n.sp-1)]
m7.ch <- m7[2:n.sp] / m7[1:(n.sp-1)]
down.days <- which(sp.ch <= 1)
up.days <- which(sp.ch > 1)
quartz()
plot(cumprod(c(1, sp.ch[down.days])), 
     type='l', 
     col='yellow', 
     ylim=range(cumprod(c(1, m1.ch[down.days])), 
                cumprod(c(1, m2.ch[down.days])), 
                cumprod(c(1, m3.ch[down.days])), 
                cumprod(c(1, m4.ch[down.days])), 
                cumprod(c(1, m5.ch[down.days])), 
                cumprod(c(1, m6.ch[down.days])), 
                cumprod(c(1, m7.ch[down.days]))), 
     log='y',
     lwd=2,
     main='Downward Days Only',
     ylab='Change')
lines(cumprod(c(1, m1.ch[down.days])), col='black', lwd=2)  
lines(cumprod(c(1, m2.ch[down.days])), col='coral', lwd=2)  
lines(cumprod(c(1, m3.ch[down.days])), col='blue', lwd=2)  
lines(cumprod(c(1, m4.ch[down.days])), col='purple', lwd=2)  
lines(cumprod(c(1, m5.ch[down.days])), col='cyan', lwd=2)  
lines(cumprod(c(1, m6.ch[down.days])), col='grey', lwd=2)  
lines(cumprod(c(1, m7.ch[down.days])), col='magenta', lwd=2)  
legend('topright', 
       lty=1,
       lwd=2, 
       col=c('yellow', 'black', 'coral', 'blue', 'purple', 'cyan', 'grey', 
             'magenta'),
       legend=c('SP', m1.name, m2.name, m3.name, m4.name, m5.name, m6.name, 
                m7.name))
abline(h=1, lty=4)                
quartz()
plot(cumprod(c(1, sp.ch[up.days])), 
     type='l', 
     col='yellow', 
     ylim=range(cumprod(c(1, sp.ch[up.days])),
                cumprod(c(1, m1.ch[up.days])), 
                cumprod(c(1, m2.ch[up.days])), 
                cumprod(c(1, m3.ch[up.days])), 
                cumprod(c(1, m4.ch[up.days])), 
                cumprod(c(1, m5.ch[up.days])), 
                cumprod(c(1, m6.ch[up.days])), 
                cumprod(c(1, m7.ch[up.days]))), 

     log='y',
     lwd=2,
     main='Upward Days Only',
     ylab='Change')
lines(cumprod(c(1, m1.ch[up.days])), col='black', lwd=2)  
lines(cumprod(c(1, m2.ch[up.days])), col='coral', lwd=2)  
lines(cumprod(c(1, m3.ch[up.days])), col='blue', lwd=2)  
lines(cumprod(c(1, m4.ch[up.days])), col='purple', lwd=2)  
lines(cumprod(c(1, m5.ch[up.days])), col='cyan', lwd=2)  
lines(cumprod(c(1, m6.ch[up.days])), col='grey', lwd=2)  
lines(cumprod(c(1, m7.ch[up.days])), col='magenta', lwd=2)  
legend('topleft', 
       lty=1,
       lwd=2, 
       col=c('yellow', 'black', 'coral', 'blue', 'purple', 'cyan', 'grey', 
             'magenta'),
       legend=c('SP', m1.name, m2.name, m3.name, m4.name, m5.name, m6.name, 
                m7.name))
abline(h=1, lty=4)

#______________________________________________________________________________


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
start <- totalFunds[last]
now <- totalFunds[length(totalFunds)]
low <- min(totalFunds[(last):(length(totalFunds))])
high <- max(totalFunds[(last):(length(totalFunds))])
earned <- high - start
down.from.high <- high - now
amt.down <- round(-100 *  down.from.high / earned, 2)
lines(x, exp.mod(totalFunds, x), col=rgb(0, 0, 0, 1))
abline(h=start, col='grey')
abline(h=now, col='grey')
abline(h=low, col=2)
abline(h=high, col=3)

lines(FidVal + f1kVal, col=4)
lines(x, exp.mod(FidVal + f1kVal, x), col=rgb(0, 0, 1, 1))

#lines(FidVal, col=4)
#lines(x, exp.mod(FidVal, x), col=rgb(0, 0, 1, 1))

#lines(f1kVal, col=5)#
#lines(x, exp.mod(f1kVal, x), col=rgb(0, 1, 1, 1))

lines(OHVal, col=6)
lines(x, exp.mod(OHVal, x), col=rgb(1, 0, 1, 1))

# FIX HERE
#amt.down <- round(100 * pct, 2)
amt.up <- round(100 * ((totalFunds[length(totalFunds)] / start) - 1), 2)
legend(
  'topleft', 
  text.col=c('green', 'black', 'red'), 
  legend=c(sprintf('$%d', max(totalFunds)),
  sprintf('$%d\n(%.2f%% down\n  %.2f%% up)',
          totalFunds[length(totalFunds)],
          amt.down,
          amt.up),
          sprintf('$%d', low)), 
  bg=rgb(1, 1, 1, 0.75))
legend('bottomright',
       lty=1,
       col=c(4, 6, 1),
       legend=c('Fidelity (+rollover)', 'E*Trade', 'Total'),
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

abline(h=1, lwd = 2)
abline(v=0, lwd = 2)
abline(v=last - 1)
#abline(v=c(30, 75), lty=4, col=rgb(0, 0, 0, 0.4))
abline(h=seq(0.1, 5.0, 0.1), col=rgb(0, 0, 0, 0.4), lty=2:1)

SP.Sharpe   <- round(Sharpe(SPd),  3)
Fid.Sharpe  <- round(Sharpe(Fidd), 3)
OH.Sharpe   <- round(Sharpe(OHd),  3)
f1k.Sharpe  <- round(Sharpe(f1kd), 3)
real.Sharpe <- round(Sharpe(dailyChange(meanReal)), 3)   

all.n <- length(SPd)
SP.Sharpe1yr   <- round(Sharpe(SPd[last:all.n],  250), 3)
Fid.Sharpe1yr  <- round(Sharpe(Fidd[last:all.n], 250), 3)
OH.Sharpe1yr   <- round(Sharpe(OHd[last:all.n],  250), 3)
f1k.Sharpe1yr  <- round(Sharpe(f1kd[last:all.n], 250), 3)
real.Sharpe1yr <- round(Sharpe(dailyChange(meanReal)[last:all.n], 250), 3)

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




#-------------------------------------------------------------
x <- 1:length(totalFunds)
savings.exp.mod <- lm(log(totalFunds) ~ x)
x <- last + seq(250, 3750, by=250)[1:10]
log.y <- coef(savings.exp.mod)[1] + x*coef(savings.exp.mod)[2]
y <- round(exp(log.y))
for (p in y) { cat(p, '\n') }
#-------------------------------------------------------------



#-------------------------------------------------------------
n <- length(wlsh.osc) 
ds <- abs(diff(wlsh.osc))
(thresh <- quantile(ds, p=0.05))
dif <- wlsh.osc[n] - wlsh.osc[n - 1]
if (dif == 0) { 
  cat('No change. Handle this case') 
} else { change <- ifelse(dif > 0, 'up', 'down') }
powET  <- ifelse(change == 'up', 2, 0)
powFid <- ifelse(change == 'up', 1, -0.1)
below.thresh <- abs(dif) <= thresh
	
	
cat(
  sprintf(
    'Direction is: %s %s\n  Set ET exp to %.2f and Fid exp to %.2f', 
    change,
    ifelse(below.thresh, sprintf('(but below threshold: %.4f)', thresh), ''),
    powET, 
    powFid))

CASH_OUT
CASH_IN

(cash.out.fid <- CASH_OUT * FidVal[length(FidVal)])
cash.out.fid <- 26500 # Update ONLY when new threshold crossed

(cash.out.et <- CASH_OUT * OHVal[length(OHVal)])
cash.out.et <- 19573

crypto.frac <- 0.003
(target.crypo.amt <- crypto.frac * totval[length(totval)]) # in: 1100

# weight by history length
f <- sqrt
w <- c(f(50), # nya 50
       f(40), # nya 40
       f(30), # nya 30
       f(20), #     20...
       f(10), 
       f(5),
       f(3),  # nya 3
       f(50), # sp 50
       f(40), # sp 40
       f(30), # sp 35
       f(20), #    20...
       f(10), 
       f(5),
       f(3))  # sp 3
(w <- w / sum(w))

# 0.0001 >= 0.00005 = 5e-5
nya50 <- c(1, 0.7249)
nas50 <- c(1, 0.0098)

nya40 <- c(1, 1)
nas40 <- c(0.9986, 0.0144)

wil30 <- c(1, 1)
nas30 <- c(1, 1)

wil20 <- c(0.9974, 0.9992)
nas20 <- c(0.1596, 0.9999)

wil10 <- c(1, 1)
nas10 <- c(1, 0.9299)

wil5 <- c(1, 0.5091)
nas5 <- c(1, 0.0691)

wil3 <- c(1, 0.0652)
nas3 <- c(0.0101, 1)


# Change must be ≥ 10%
# UPDATE: Fridays or when a percent-in value has changed
# Fid (buy and hold form Partnership Portfolio) [2-param]
(fid.amt <- FidVal[length(FidVal)]) # [80533, 222221]
(fid.pct.in <- w %*% c(
   nya50[2], nya40[2], wil30[2], wil20[2], wil10[2], wil5[2], wil3[2], 
   nas50[2], nas40[2], nas30[2], nas20[2], nas10[2], nas5[2], nas3[2]))
# 6685
fid.amt.in <- fid.amt * fid.pct.in
(fid.res <- fid.amt - fid.amt.in)

# OH (e*trade) - traditional strategy [3-param]
(oh.amt <- OHVal[length(OHVal)]) # [92495, 100422]
(oh.pct.in <- w %*% c(
   nya50[1], nya40[1], wil30[1],  wil20[1], wil10[1], wil5[1], wil3[1],
   nas50[1], nas40[1], nas30[1],  nas20[1], nas10[1], nas5[1], nas3[1])) 
# 9099
oh.amt.in <- oh.amt * oh.pct.in
(oh.res <- oh.amt - oh.amt.in)

# For 401(k): # Next:13 Feb (≥10%)
(f1k.total <- f1kVal[length(f1kVal)]) # [141741, 158404] [2-param]
#--------------------------------------------------------

x <- c(1, 2, 4)
x <- x / sum(x)
x * c(f1k.pct.in)

#--------------------------------------------------------
# To Save New `df` at the end of each year:
#df.test <- data.frame(
#  SPActual=SPActual, FidIn=FidIn, FidVal=FidVal, f1kIn=f1kIn, f1kVal=f1kVal, 
#  OHIn=OHIn, OHVal=OHVal)
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