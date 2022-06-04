library(depmixS4)

DATA <- '~/Learning/marketModeling/data'

sp <- read.csv(sprintf('%s/sp1950.csv', DATA))
nasdaq <- read.csv(sprintf('%s/nasdaq1965.csv', DATA))
wilsh <- read.csv(sprintf('%s/wilshire1990.csv', DATA))


get.daily.returns <- function(ts) {
  n <- length(ts)
  c(NA, ts[2:n] / ts[1:(n - 1)])
}


format.data <- function(filename) {
  df <- read.csv(sprintf('%s/%s.csv', DATA, filename))
  df$date <- as.Date(df$Date)
  df$value <- df$Adj.Close
  df$returns <- get.daily.returns(df$value)
  n <- nrow(df)
  df[2:n, c('date', 'value', 'returns')]
}

sp <- format.data('sp1950')
nas <- format.data('nasdaq1965')
wil <- format.data('wilshire1990')
head(nas)


run.model <- function(df, n.states=5) {
  hmm.mod <- depmix(returns ~ 1, family=gaussian(), nstates=n.states, data=df)
  mod.fit <- fit(hmm.mod)
  mod.fit
}


plot.model <- function(df, fit.mod, show.last.n=250) {
  post.probs <- posterior(fit.mod)
  n <- nrow(df)
  sub <- df[(n - show.last.n):n, ]
  states <- post.probs$state[(n - show.last.n):n]
  par(mfrow=c(2, 1))
  YMN <- 0.9*min(sub$returns)
  YMX <- 1.1*max(sub$returns)
  par(mar=c(0, 4, 1, 1))
  plot(sub$returns, type='l', ylim=c(YMN, YMX), xaxt='n')
  lapply(
    0:length(sub$returns) - 1, 
    function(i) {
      rect(
  	    i, 
  	    YMN, 
  	    (i + 1),
  	    YMN + 0.05,
  	    #col=post.probs$state[i + 1],
  	    col=states[i + 1],
  	    border=NA)
    })
  par(mar=c(3, 4, 1, 1))
  plot(sub$value ~ sub$date, type='l', log='y')  
}


find.best <- function(df, n.states=c(3, 4, 5, 6)) {
  best.aic <- Inf
  best.mod <- NULL
  for (states in n.states) {
  	cat('n states: ', states, '\n')
  	fit <- run.model(df, states)
  	aic <- AIC(fit)
  	if (aic < best.aic) {
  	  best.aic <- aic
  	  best.mod <- fit
  	}
  }
  best.mod
}

# States:
# Black:   1
# Red:     2
# Green:   3
# Blue:    4
# Cyan:    5
# Magenta: 6
# Yellow:  7
# Grey:    8

sp.fit <- find.best(sp, c(8))
plot.model(sp, sp.fit, 5*250)
summary(sp.fit)  # 8
post.probs <- posterior(sp.fit)
current.state <- post.probs$state[length(post.probs$state)]
tr.mat <- sp.fit@trDens[1, , ]
n.states <- nrow(tr.mat)
mus <- summary(sp.fit)[1:n.states]
expected.val.sp <- tr.mat[, current.state] %*% mus
cat('Current State:', current.state, '\nExpected Val:', expected.val.sp, '\n')


nas.fit <- find.best(nas, c(7))
plot.model(nas, nas.fit, 5*250)
summary(nas.fit)  # 7
post.probs <- posterior(nas.fit)
current.state <- post.probs$state[length(post.probs$state)]
tr.mat <- nas.fit@trDens[1, , ]
n.states <- nrow(tr.mat)
mus <- summary(nas.fit)[1:n.states]
expected.val.nas <- tr.mat[, current.state] %*% mus
cat('Current State:', current.state, '\nExpected Val:', expected.val.nas, '\n')


wil.fit <- find.best(wil, c(8))
plot.model(wil, wil.fit, 5*250)
summary(wil.fit)  # 8
post.probs <- posterior(wil.fit)
current.state <- post.probs$state[length(post.probs$state)]
tr.mat <- wil.fit@trDens[1, , ]
n.states <- nrow(tr.mat)
mus <- summary(wil.fit)[1:n.states]
expected.val.wil <- tr.mat[, current.state] %*% mus
cat('Current State:', current.state, '\nExpected Val:', expected.val.wil, '\n')


mean(c(expected.val.sp, expected.val.nas, expected.val.wil))