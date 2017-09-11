calculateLogLikelihood <- function(termCountsTarget, termCountsComparison, minSignificance = 6.63) {  
  
  uniqueTerms <- setdiff(names(termCountsTarget), names(termCountsComparison))
  
  zeroCounts <- rep(0, length(uniqueTerms))
  names(zeroCounts) <- uniqueTerms
  termCountsComparison <- c(termCountsComparison, zeroCounts)
  
  termsToCompare <- intersect(names(termCountsTarget), names(termCountsComparison))
  
  a <- termCountsTarget[termsToCompare]
  b <- termCountsComparison[termsToCompare]
  c <- sum(termCountsTarget)
  d <- sum(termCountsComparison)
  Expected1 = c * (a+b) / (c+d)
  Expected2 = d * (a+b) / (c+d)
  t1 <- a * log((a/Expected1) + (a == 0))
  t2 <- b * log((b/Expected2) + (b == 0))
  logLikelihood <- 2 * (t1 + t2)
  
  # compare relative frequencies to indicate over/underuse
  relA <- a / c
  relB <- b / d
  # underused terms are multiplied by -1
  logLikelihood[relA < relB] <- logLikelihood[relA < relB] * -1
  
  logLikelihood[logLikelihood < minSignificance] <- 0
  
  return(logLikelihood)
}