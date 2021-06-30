hist.file <- '/Users/dsp/Desktop/marketStudies/data/myHistoric.RData'
df <- data.frame(
  SPActual=SPActual, FidIn=FidIn, FidVal=FidVal, f1kIn=f1kIn, f1kVal=f1kVal, 
  OHIn=OHIn, OHVal=OHVal)

save(df, file=hist.file)

load(hist.file)
head(df)