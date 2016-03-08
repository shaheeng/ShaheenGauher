#Run the function below

Evaluate = function(actual=NULL, predicted=NULL, cm=NULL){
  if(is.null(cm)) {
    actual = actual[!is.na(actual)]
    predicted = predicted[!is.na(predicted)]
    cm = as.matrix(table(Actual=actual, Predicted=predicted))
  }
  n = sum(cm) #number of instances
  nc = nrow(cm) #number of classes
  tptn = diag(cm)
  rowsums = apply(cm,1, sum)
  colsums = apply(cm, 2, sum)
  
  #accuracy
  accuracy = sum(tptn) / n
  
  #per class
  recall = tptn / rowsums
  precision = tptn / colsums
  f1 = 2 * precision * recall / (precision + recall)
  
  #macro
  macro_precision = mean(precision)
  macro_recall = mean(recall)
  macro_f1 = mean(f1)
  
  #1-vs-all matrix
  one_v_all = sapply(1:nc, function(i) {v = c(cm[i,i], rowsums[i]-cm[i,i], colsums[i]-cm[i,i], n-rowsums[i]-colsums[i]+cm[i,i]); return(matrix(v , nrow = 2, byrow = T))}, simplify = F)
  s = matrix(0, nrow=2,ncol=2)
  for(i in 1:nc){s=s+one_v_all[[i]]}
  
  #avg accuracy
  avg_accuracy = sum(diag(s))/sum(s)
  
  #micro
  micro_averaged = (diag(s)/apply(s,1, sum))[1];
  
  #majority class
  majorityClass = which(rowsums==max(rowsums))
  mc_cm = matrix(0, nrow = nc, ncol = nc)
  mc_cm[ ,majorityClass] = rowsums
  mcAccuracy = rowsums[majorityClass] / n
  mcRecall = rep(0,nc); mcRecall[majorityClass] = 1
  mcPrecision = rep(0,nc); mcPrecision[majorityClass] = mcAccuracy
  mcF1 = rep(0,nc); mcF1[majorityClass] = 2*mcPrecision[majorityClass]*mcRecall[majorityClass] / (mcPrecision[majorityClass] + mcRecall[majorityClass])
  
  #random accuracy
  rnd_Accuracy = sapply(one_v_all, function(x) apply(x,1,sum)[1]*apply(x,2,sum)[1]) 
  rnd_Accuracy = sum(rnd_Accuracy) / n^2
  
  #kappa
  kappa = (accuracy - rnd_Accuracy) / (1 - rnd_Accuracy)
  
  #fraction of positive examples
  p = rowsums / n  
  
  #random guess
  rg_accuracy = 0.5
  rg_precision = p
  rg_recall = 0.5
  rg_f1 = p /(p+0.5)
  
  #rnd weighted
  rgw_accurcy = 2*p^2 - 2*p + 1
  rgw_precision = p
  rgw_recall = p
  rgw_F1 = p
  
  classNames = names(tptn)
  if(is.null(classNames)) classNames = paste("C",(1:nc),sep="")
  
  return(list(
    ConfusionMatrix = cm,
    Metrics = data.frame(
      Class = classNames,
      Accuracy = accuracy,
      Precision = precision,
      Recall = recall,
      F1 = f1,
      MacroAvgPrecision = macro_precision,
      MacroAvgRecall = macro_recall,
      MacroAvgF1 = macro_f1,
      AvgAccuracy = avg_accuracy,
      MicroAvgPrecision = micro_averaged,
      MicroAvgRecall = micro_averaged,
      MicroAvgF1 = micro_averaged,
      MajorityClassAccuracy = mcAccuracy,
      MajorityClassPrecision = mcPrecision,
      MajorityClassRecall = mcRecall,
      MajorityClassF1 = mcF1,
      RandomAccuracy = rnd_Accuracy,
      Kappa = kappa,
      RandomGuessAccuracy = rg_accuracy,
      RandomGuessPrecision = rg_precision,
      RandomGuessRecall = rg_recall,
      RandomGuessF1 = rg_f1,
      RandomGuessWeightedAccurcy = rgw_accurcy,
      RandomGuessWeightedPrecision = rgw_precision,
      RandomGuessWeightedRecall= rgw_recall,
      RandomGuessWeightedF1 = rgw_F1)))
}


##
#e.g.1  pg 11 ##################
cm_df = data.frame(col1=c(602,10051,),col2=c(2355,1024273))
cm_matrix = data.matrix(cm_df,rownames.force=NA)
cm_matrix
class(cm_matrix)

results = Evaluate(cm=cm_matrix)
results$Metrics

#e.g.2 pg 12 ##################
cm_df = data.frame(col1=c(139,850),col2=c(346,615228))
cm_matrix = data.matrix(cm_df,rownames.force=NA)
cm_matrix
class(cm_matrix)

results = Evaluate(cm=cm_matrix)
results$Metrics



#e.g.3  pg 13 ##################
cm_df = data.frame(col1=c(3120,15208),col2=c(6837,1296873))
cm_df=T(cm_df)
cm_matrix = data.matrix(cm_df,rownames.force=NA)
cm_matrix
class(cm_matrix)

results = Evaluate(cm=cm_matrix)
results$Metrics

#e.g.4  pg 13 ##################
cm_df = data.frame(col1=c(4034,11314),col2=c(5923,1300767))
cm_matrix = data.matrix(cm_df,rownames.force=NA)
cm_matrix
class(cm_matrix)

results = Evaluate(cm=cm_matrix)
results$Metrics
