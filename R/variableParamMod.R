#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Desktop/marketStudies')

library(mgcv)

DATA <- 'data'
NOW <- as.Date(Sys.time())
THIS_YEAR <- as.numeric(unlist(strsplit(as.character(NOW), '-')))[1] - 2000
MAX_LAG <- 10
LAGS <- c(1:MAX_LAG, 30, 60, 90, 180, 365)

adjust.date <- function(d) {
  if (d > NOW) {
    d <- d - 365*100 - 25
  }
  d
}


lag.n <- function(x, n) {
  c(rep(NA, n), x[1:(length(x) - n)])
}


add.changes <- function(df) {
  cat('Adding changes...\n')
  for (lg in LAGS) {
  	name <- paste('Change', lg, sep='')
    df[, name] <- c(rep(NA, lg), sp$Value[(lg + 1):n] / sp$Value[1:(n - lg)]) 		
  }
  df
}


add.lags <- function(df, colname) {
  cat('Adding lags...\n')
  for (lag in LAGS) {
  	lagged <- lag.n(df[, colname], lag)
  	new.name <- paste(colname, 'L', lag, sep='')
  	df[, new.name] <- lagged
  }
  df
}


get.time.to.change <- function(df, col, new.value) {
  cat('Adding time to increase ', new.value, '...\n', sep='')
  n <- nrow(df)
  new.name <- paste(col, new.value, sep='')
  df[, new.name] <- rep(NA, n)
  for (i in 1:(n - 1)) {
  	current <- df[i, col]
  	goal <- current * new.value
  	if (new.value >= 1) {
  	  for (j in (i + 1):n) {
        if (df[j, col] >= goal) {
      	  df[i, new.name] <- j - i
      	  break
        }
  	  }
  	} else {
  	  for (j in (i + 1):n) {
        if (df[j, col] <= goal) {
      	  df[i, new.name] <- j - i
      	  break
        }
  	  }  		
  	}
  }
  df
}


prep.data <- function(path) {
  sp <- read.csv(path)
  sp$Date <- as.Date(sp$Date, format='%m/%d/%y')
  sp$Date <- sapply(sp$Date, adjust.date)
  sp$Date <- as.Date(sp$Date, origin='1970-01-01')
  sp <- sp[, c('Date', 'Adj.Close', 'Volume')]
  n <- nrow(sp)
  names(sp)[2] <- 'Value'
  sp <- add.changes(sp)
  sp <- add.lags(sp, 'Value')
  sp <- sp[complete.cases(sp), ]
  sp <- get.time.to.change(sp, 'Value', 1.05)
  sp <- get.time.to.change(sp, 'Value', 0.95)
  sp$Value0.9[is.na(sp$Value0.9)] <- max(sp$Value0.9, na.rm=T)
  sp$foresight <- log(sp$Value0.9 / sp$Value1.1)
  sp
}
path <- paste(DATA, 'sp1950.csv', sep='/')
data <- prep.data(path)
head(data)
tail(data)

par(mfrow=c(2, 1))
plot(data$Date, log(data$Value), type='l', ylim=c(2.5, 8.5))
plot(data$Date, data$Change365, type='l')
abline(h=1, col='grey')

plot(data$Date, data$Value1.1, type='l')
lines(data$Date, data$Value0.9, col=2)
plot(data$Date, data$foresight, type='l')
abline(h=0, col='grey')

head(data)

g1 <- gam(
  foresight ~ s(Value) + s(Volume) + s(Change1) + s(Change2) 
    + s(Change3) + s(Change4) + s(Change5) + s(Change6) + s(Change7) 
    + s(Change8) + s(Change9) + s(Change10) + s(Change30) + s(Change60) 
    + s(Change90) + s(Change180) + s(Change365) + s(ValueL1) + s(ValueL2) 
    + s(ValueL3) + s(ValueL4) + s(ValueL5) + s(ValueL6) + s(ValueL7) 
    + s(ValueL8) + s(ValueL9) + s(ValueL10) + s(ValueL30) + s(ValueL60) 
    + s(ValueL90) + s(ValueL180) + s(ValueL365), 
  data=data)

# Copy and reduce here
g1 <- gam(
  foresight ~ s(Value) + s(Volume) + s(Change2) 
    + s(Change6)
    + s(Change30) + s(Change60) 
    + s(Change90) + s(Change180) + s(Change365)
    + s(ValueL3)
    + s(ValueL30) + s(ValueL60) 
    + s(ValueL90) + s(ValueL180) + s(ValueL365), 
  data=data)
summary(g1)

par(mfrow=c(3, 5))
plot(g1)

    
mod1 <- lm(
  foresight ~ 
    Value + Volume + log(Volume) + Change1 + Change2 + I(1*(Change2 >= 1)) 
      + Change3 + Change4 + Change5 + Change6 + I(Change6^2) + Change7 + Change8 
      + Change9 + Change10 + Change30 + I(1*(Change30 >= 1)) + Change60 
      + I(1*(Change60 >= 1)) + Change90 + I(1*(Change90 >= 1)) + Change180 
      + log(Change180) + Change365 + I(1/Change365) + ValueL1 + ValueL2 
      + ValueL3 + I(ValueL3^2) + ValueL4 + ValueL5 + ValueL6 + ValueL7 + ValueL8 
      + ValueL9 + ValueL10 + ValueL30 + I(ValueL30^2) + ValueL60 
      + log(ValueL60) + ValueL90 + log(ValueL90) + ValueL180 + I(ValueL180^2) 
      + ValueL365 + I(ValueL365^2),
  data=data)
mod1 <- step(mod1)
summary(mod1)

# Copy and add Interactions
mod2 <- lm(
  foresight ~ 
    (Volume * log(Volume)) + Change2 + Change3 + I(1*(Change30 >= 1)) 
    + (Change60 * I(1*(Change60 >= 1))) + (Change90 * I(1*(Change90 >= 1))) 
    + (Change180 * log(Change180)) + Change365 + I(1/Change365) + ValueL3 
    + I(ValueL3^2) + I(ValueL30^2) + log(ValueL60) + log(ValueL90) 
    + I(ValueL180^2) + ValueL365 + I(ValueL365^2),
  data=data)
mod2 <- step(mod2)
summary(mod2)

head(data)
mod2 <- lm(foresight ~ 
  Volume + log(Volume) + Change1 + Change2 + Change3 + Change4 + Change5
    + Change6 + Change7 + Change8 + Change9 + Change10 + Change30
    + Change60 + I(1 * (Change60 >= 1)) + Change90 
    + I(1 * (Change90 >= 1)) + Change180 + log(Change180) + Change365 
    + I(1/Change365) + Value +  + ValueL1 + ValueL2 + ValueL3 
    + I(ValueL3^2) + ValueL4 + ValueL5 + ValueL6 + ValueL7 + ValueL8 + ValueL9 
    + ValueL10 +  + ValueL30 + I(ValueL30^2) +  + ValueL60 + log(ValueL60) 
    +  + ValueL90 + log(ValueL90) +  + ValueL180 + I(ValueL180^2) + ValueL365 
    + I(ValueL365^2) + Volume:log(Volume) + 
    Change60:I(1 * (Change60 >= 1)) + Change180:log(Change180), 
    data=data)
mod2 <- step(mod2)
summary(mod2)

par(mfrow=c(2, 1))
plot(data$Date, data$foresight, type='l')
abline(h=0, col='grey')
preds <- predict(mod2, newdata=data)
lines(data$Date, preds, col=rgb(1, 0, 0, 0.6))
data$preds <- preds

recent <- subset(data, Date >= as.Date('2017-03-01'))
plot(recent$Date, 
     recent$foresight, 
     type='l', 
     ylim=c(range(c(recent$foresight, recent$preds), na.rm=T)))
abline(h=0, col='grey')
lines(recent$Date, recent$preds, col=rgb(1, 0, 0, 0.6))


plot(recent$Date, recent$Value, type='l')
plot(recent$Date, 
     recent$foresight, 
     type='l', 
     ylim=c(range(c(recent$foresight, recent$preds), na.rm=T)))
abline(h=0, col='grey')
lines(recent$Date, recent$preds, col=rgb(1, 0, 0, 0.6))
