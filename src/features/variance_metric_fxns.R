#### Scaled Quantile Range ####
quantileRange <- function(x, low = .1, high = .9) {
  (quantile(x, low) - quantile(x, high)) / mean(x)
}

#### Coefficient Of Variation ####
coeffVar <- function(x) sd(x)/mean(x)

#### Slope Metrics ####
# experimental!
slopeMetrics <- function(x,  rtn = NULL, radians = TRUE) {
  
  diffs <- c(NA, diff(x)) # get first difference
  corrDiffs <- diffs/mean(x)
  ## peak counting
  # test for change in sign of slope and add all negatives
  npks <- sum(diffs * lag(diffs) < 0, na.rm = TRUE)
  ppks <- npks / (length(x) - 2) # proportion of total possible peaks
  
  # slopes
  slpMean <- mean(diffs, na.rm=TRUE)
  corrSlpMean <- mean(corrDiffs, na.rm=TRUE)
  
  ## change in slopes
  # average of absolute difference in slopes
  absSlpDiff <- mean(diff(diffs), na.rm=TRUE)
  corrAbsSlpDiff <- mean(diff(corrDiffs), na.rm=TRUE)
  # average of percent change in slope 
  pctSlpDiff <- mean(abs(diff(diffs)/lead(diffs)[-length(diffs)]), na.rm=TRUE)
  corrPctSlpDiff <- mean(abs(diff(corrDiffs)/lead(diffs)[-length(diffs)]), na.rm=TRUE)
  
  # ## autocorrelation in slopes
  # slpAcor <- acf(diffs, plot=FALSE, lag.max=1,
  #                      type="correlation", na.action=na.omit)$acf[2]
  # corrSlpAcor <- acf(corrDiffs, plot=FALSE, lag.max=1,
  #                      type="correlation", na.action=na.omit)$acf[2] 
  
  ## slope cv
  slpCv <- sd(corrDiffs, na.rm=TRUE)
  
  ## change in angle
  # average absolute change in angle
  angDiffMean <- mean(abs(diff(atan(diffs))), na.rm=TRUE) * ifelse(radians, 1, 180/pi)
  corrAngDiffMean <- mean(abs(diff(atan(corrDiffs))), na.rm=TRUE) * ifelse(radians, 1, 180/pi)
  # proportion of total possible change in angle
  angDiffPro <- sum(abs(diff(atan(diffs))), na.rm=TRUE) / (pi * (length(x) - 1))
  corrAngDiffPro <- sum(abs(diff(atan(corrDiffs))), na.rm=TRUE) / (pi * (length(x) - 1))
  
  allmetrics <-  list(npks = npks, ppks = ppks, 
                      absSlpDiff = absSlpDiff,  corrAbsSlpDiff = corrAbsSlpDiff,
                      pctSlpDiff = pctSlpDiff, corrPctSlpDiff = corrPctSlpDiff, 
                      angDiffMean = angDiffMean, corrAngDiffMean = corrAngDiffMean, 
                      angDiffPro = angDiffPro, corrAngDiffPro = corrAngDiffPro, 
                      slpMean = slpMean, corrSlpMean = corrSlpMean, 
                     # slpAcor = slpAcor, corrSlpAcor = corrSlpAcor, 
                      slpCv = slpCv)
  
  return(allmetrics[[rtn]])
  
}
