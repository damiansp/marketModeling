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


run.model <- function(df, n.states=5, show.last.n=250) {
  hmm.mod <- depmix(returns ~ 1, family=gaussian(), nstates=n.states, data=df)
  mod.fit <- fit(hmm.mod)
  post.probs <- posterior(mod.fit)
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
  mod.fit
}


sp.fit <- run.model(sp, 6, 500)
attr(sp.fit, 'response')

nas.fit <- run.model(nas, 6, 500)
attr(nas.fit, 'response')

wil.fit <- run.model(wil, 6, 500)
attr(wil.fit, 'response')

