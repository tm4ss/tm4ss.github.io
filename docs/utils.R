convertSlamToMatrix <- function(DTM) {
  if (slam::is.simple_triplet_matrix(DTM)) {
    DTM <- Matrix::sparseMatrix(i=DTM$i, j=DTM$j, x=DTM$v, dims=c(DTM$nrow, DTM$ncol), dimnames = dimnames(DTM))
  }
  return(DTM)
}


convertMatrixToSparseM <- function(X) {
  X.csc <- new("matrix.csc", ra = X@x,
               ja = X@i + 1L,
               ia = X@p + 1L,
               dimension = X@Dim)
  return(as.matrix.csr(X.csc))
}


convertSlamToSparseM <- function(DTM) {
  return(convertMatrixToSparseM(convertSlamToMatrix(DTM)))
}


# MICRO AVERAGE: TP,FP,FN over all category decisions first, then F1
# MACRO AVERAGE: F1 over each individual categories first, then average
F.measure <- function(inPred,inLabels, positiveClassName = NULL) {
  
  # PREPARE DATA
  allpred <- as.vector(inPred)
  alllabels <- as.vector(inLabels)
  classes <- sort(unique(c(allpred,alllabels)))
  
  if (length(classes) == 2) {
    # SINGLE CLASS EVALUATION
    
    if (is.null(positiveClassName)) {
      # assume positiveClassName is classes != 0
      positiveClassName <- classes[which(classes != 0)]
    }
    pred <- ifelse(allpred == positiveClassName, 1, 0)
    labels <- ifelse(alllabels == positiveClassName, 1, 0)
    
    if (length(pred)!=length(labels)) stop("F.measure: lengths of true and predicted labels do not match.");
    neg.labels <- which(labels == 0);  
    pos.labels <- which(labels == 1);
    npos <- length(pos.labels);   
    
    TP <- sum(pred[pos.labels] == 1);
    FP <- sum(pred[neg.labels] == 1);
    FN <- sum(pred[pos.labels] == 0);
    TN <- sum(pred[neg.labels] == 0);
    acc <- (TP+TN)/length(labels);  
    if ((TP+FP) == 0) {
      precision <- 0
    } else { 
      precision <- TP/(TP+FP)
    }
    if ((TP+FN) == 0) {
      recall <- 0
    } else {
      recall <- TP/(TP+FN)
    }    
    if ((TN+FP) == 0) {
      specificity <- 0
    } else {
      specificity <- TN/(TN+FP)
    }    
    if ((precision+recall) == 0) {
      F <- 0
    } else { 
      F = 2 *(precision*recall) / (precision+recall); 
    }    
    results <- c(precision,recall,specificity,F,acc, npos);
    names(results) <- c("P", "R", "S", "F", "A", "Pos.");  
    fList <- results
    
  } else {
    # MULTI CLASS EVALUATION
    
    # MICRO
    TPmic <- 0
    FPmic <- 0
    FNmic <- 0  
    
    # MACRO
    results <- matrix(0, nrow=length(classes),ncol=6) 
    rownames(results) <- classes
    colnames(results) <- c("P", "R", "S", "F", "A", "Pos.");  
    
    # ITERATE POVER CLASSES
    for (classe in classes) {
      
      pred <- ifelse(allpred == classe, 1, 0)
      labels <- ifelse(alllabels == classe, 1, 0)
      
      if (length(pred)!=length(labels)) stop("F.measure: lengths of true and predicted labels do not match.");
      neg.labels <- which(labels == 0);  
      pos.labels <- which(labels == 1);
      npos <- length(pos.labels);   
      
      TP <- sum(pred[pos.labels] == 1);
      FP <- sum(pred[neg.labels] == 1);
      FN <- sum(pred[pos.labels] == 0);
      TN <- sum(pred[neg.labels] == 0);
      #print(c(TP,FP,FN,TN))
      acc <- (TP+TN)/length(labels);
      
      TPmic <- TPmic + TP
      FPmic <- FPmic + FP
      FNmic <- FNmic + FN
      
      if ((TP+FP) == 0) {
        precision <- 0
      } else { 
        precision <- TP/(TP+FP)
      }
      if ((TP+FN) == 0) {
        recall <- 0
      } else {
        recall <- TP/(TP+FN)
      }    
      if ((TN+FP) == 0) {
        specificity <- 0
      } else {
        specificity <- TN/(TN+FP)
      }    
      if ((precision+recall) == 0) {
        F <- 0
      } else { 
        F = 2 *(precision*recall) / (precision+recall); 
      }    
      res <- c(precision,recall,specificity,F,acc, npos);
      results[classe,] <- res
    }
    
    
    # MICRO
    if ((TPmic+FPmic) == 0) {
      precisionMicro <- 0
    } else { 
      precisionMicro <- TPmic/(TPmic+FPmic)
    }
    if ((TPmic+FNmic) == 0) {
      recallMicro <- 0
    } else {
      recallMicro <- TPmic/(TPmic+FNmic)
    }
    
    #print(recallMicro)
    #print(precisionMicro)
    
    fMicro <- 2 *(precisionMicro*recallMicro) / (precisionMicro+recallMicro); 
    resultMicro <- c(precisionMicro, recallMicro, fMicro )
    names(resultMicro) <- c("P", "R", "F");
    
    fList <- list(macro = results, micro = resultMicro)
    
  }
  return (fList);
}


get_k_fold_logical_indexes <- function(j, k, n) {
  if (j > k) stop("Cannot select fold larger than nFolds")
  fold_lidx <- rep(FALSE, k)
  fold_lidx[j] <- TRUE
  fold_lidx <- rep(fold_lidx, length.out = n)
  return(fold_lidx)
}


k_fold_cross_validation <- function(labeledDTM, classesOfDocuments, k = 10, cost = 10, ...) {
  evaluationMeasures <- NULL
  for (j in 1:k) {
    currentFold <- get_k_fold_logical_indexes(j, k, nrow(labeledDTM))
    
    trainingSet <- labeledDTM[!currentFold, ]
    trainingLabels <- classesOfDocuments[!currentFold]
    
    model <- LiblineaR(trainingSet, trainingLabels, cost = cost, ...)
    
    testSet <- labeledDTM[currentFold, ]
    testLabels <- classesOfDocuments[currentFold]
    predictedLabels <- predict(model, testSet)$predictions
    
    # collect k evaluation results
    kthEvaluation <- F.measure(predictedLabels, testLabels, positiveClassName = "FOREIGN")
    evaluationMeasures <- rbind(evaluationMeasures, kthEvaluation)
  }
  return(colMeans(evaluationMeasures))
}



optimize_C <- function(trainingDTM, trainingLabels, plot_graph = F) {
  cParameterValues <- c(0.003, 0.01, 0.03, 0.1, 0.3, 1, 3 , 10, 30, 100)
  fValues <- NULL
  for (cParameter in cParameterValues) {
    print(paste0("C = ", cParameter))
    evalMeasures <- k_fold_cross_validation(trainingDTM, trainingLabels, cost = cParameter)
    fValues <- c(fValues, evalMeasures["F"])
  }
  if (plot_graph) {
    plot(fValues, type="o", col="green", xaxt="n")
    axis(1,at=1:length(cParameterValues),labels=cParameterValues)
  }
  bestC <- cParameterValues[which.max(fValues)]
  print(paste0("Best C value: ", bestC, ", F1 = ", max(fValues)))
  return(bestC)
}

