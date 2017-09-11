
calculateCoocStatistics <- function(coocTerm, binDTM, measure = "DICE") {
  
  # Ensure Matrix (SparseM} or matrix {base} format
  require(Matrix)
  require(slam)
  if (is.simple_triplet_matrix(binDTM)) {
    binDTM <- sparseMatrix(i=binDTM$i, j=binDTM$j, x=binDTM$v, dims=c(binDTM$nrow, binDTM$ncol), dimnames = dimnames(binDTM))
  }
  
  # Ensure binary DTM
  if (any(binDTM > 1)) {
    binDTM[binDTM > 1] <- 1
  }
  
  # calculate cooccurrence counts
  coocCounts <- t(binDTM) %*% binDTM
  
  # retrieve numbers for statistic calculation
  k <- nrow(binDTM)
  ki <- sum(binDTM[, coocTerm])
  kj <- colSums(binDTM)
  names(kj) <- colnames(binDTM)
  kij <- coocCounts[coocTerm, ]
  
  # calculate statistics
  switch(measure, 
         DICE = {
           dicesig <- 2 * kij / (ki + kj)
           dicesig <- dicesig[order(dicesig, decreasing=TRUE)]
           sig <- dicesig
         },
         LOGLIK = {
           logsig <- 2 * ((k * log(k)) - (ki * log(ki)) - (kj * log(kj)) + (kij * log(kij)) 
                          + (k - ki - kj + kij) * log(k - ki - kj + kij) 
                          + (ki - kij) * log(ki - kij) + (kj - kij) * log(kj - kij) 
                          - (k - ki) * log(k - ki) - (k - kj) * log(k - kj))
           logsig <- logsig[order(logsig, decreasing=T)]
           sig <- logsig    
         },
         MI = {
           mutualInformationSig <- log(k * kij / (ki * kj))
           mutualInformationSig <- mutualInformationSig[order(mutualInformationSig, decreasing = TRUE)]
           sig <- mutualInformationSig    
         },
         {
           sig <- sort(kij, decreasing = TRUE)
         }
        )
  sig <- sig[-match(coocTerm, names(sig))]
  return(sig)
}