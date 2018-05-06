#---------#---------#---------#---------#---------#---------#---------#---------
dat <- read.csv('~/Desktop/buyStats.csv')
dat$mCap_ <- as.numeric(gsub(',', '', dat$mCap_))
dat$freeCashFlow_ <- as.numeric(gsub(',', '', dat$freeCashFlow_))
dat.original <- dat

full <- dat[complete.cases(dat), ]
names(full)
start.col <- which(names(full) == 'MFRisk')
full <- full[, start.col:ncol(full)]
#full <- full[, -which(names(full) %in% c('industry', 'sector'))]


for (predictor in names(full)) {
  if (predictor != 'industry' & predictor != 'sector') {
  	cat('\n\nStarting', predictor, '\n')
  	min.val <- min(dat[, predictor], na.rm=T)  	
  	max.val <- max(dat[, predictor], na.rm=T)
    pred.na <- which(is.na(dat[, predictor]))
    n <- pred.na[length(pred.na)]
    for (i in pred.na) {
      cat('On row ', i, ' (last = ', n, ')\r', sep='')
      data.cols <- which(!is.na(dat[i, ]))
      data.cols <- data.cols[data.cols > 9]
      col.names <- names(dat)[data.cols]
      col.names <- c(predictor, col.names)
      col.names <- col.names[-which(col.names %in% c('industry', 'sector'))]
      if (length(col.names)) {
      	mod.string <- sprintf('lm(%s ~ ., data=full[, col.names])', predictor)
        mod <- eval(parse(text=mod.string))
        mod <- step(mod, trace=0)
        prediction <- predict(mod, newdata=dat[i, ])
        if (prediction < min.val) { prediction <- min.val }
        else if (prediction > max.val) { prediction <- max.val }
        dat[i, predictor] <- prediction
      } else {
      	dat[i, predictor] <- median(dat[, predictor], na.rm=T)
      }  
    }
  }
}

write.csv(dat, '~/Desktop/buyStatsFilled.csv', row.names=F)