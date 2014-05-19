setwd('~/Desktop//Courses/Warwick//Data Mining/Exercise Week 8/')

data = read.csv('AI2013_papers.csv')

#we do not consider the data considering to the categories Synopsis and Subjective
#d <- data[data$type != 'Synopsis',]
#d <- d[d$type != 'Subjective',]
d <- data
d <- d[,-1]
row.names(d) <- NULL

myNormalise=function(data, col){
  tmp = sqrt(sum(data[,col]^2))
  for(i in 1:nrow(data)){
    data[i,col] = data[i,col]/tmp
  }
  return(data)
}

for(attribute in 1:length(names(d))){
  if(is.numeric(d[,attribute])) 
  {
    d <- myNormalise(d, attribute)
  }
}

d<-d[sample(nrow(d)),]
m <- nrow(d)
classes <- c('Case Study', 'Correspondence', 'Essay', 'Opinion', 'Perspective', 'Research', 'Review', 'Viewpoint')

d2 = d
# add a new column that assigns each row a number from 1 to 10, cutting the data up equally
d2$fold = cut(1:nrow(d2), breaks=10, labels=F)
row.names(d2) <- NULL

#get Precision, Recall, F-measure, Accuracy, Rmacro, Rmicro, Pmacro and Pmicro for a fold for a classifier
getPRF = function(table){
  tab.precision;tab.recall;fmeasure;acc <- c()
  Rmacro;Pmacro;TPc <- 0
  Rmicro <- 0
  Pmicro <- 0
  for(i in 1:ncol(table)){
    TP <- table[i,i]
    FN <- sum(table[-i,i])
    FP <- sum(table[i,-i])
    TN <- sum(table[-i,-i])
    precision <- TP/(TP+FP)
    recall <- TP/(TP+FN)
    f <- 2*precision*recall/(precision+recall)
    accuracy <- (TP + TN)/(TP + TN + FP + FN)
    tab.precision <- append(tab.precision, precision, after=length(tab.precision))
    tab.recall <- append(tab.recall, recall, after=length(tab.recall))
    fmeasure <- append(fmeasure, f, after=length(fmeasure))
    acc <- append(acc, accuracy, after=length(acc))
    Rmacro <- Rmacro + recall
    Pmacro <- Pmacro + precision
    TPc <- TPc + TP
    Rmicro <- TP + FN
    Pmicro <- TP + FP
  }
  tab.precision <- as.data.frame(tab.precision, row.names=classes)
  tab.recall <- as.data.frame(tab.recall, row.names=classes)
  fmeasure <- as.data.frame(fmeasure, row.names=classes)
  acc <- as.data.frame(acc, row.names=classes)
  res <- tab.precision
  res <- cbind(res, tab.recall)
  res <- cbind(res, fmeasure)
  res <- cbind(res, acc)
  Rmacro <- Rmacro/ncol(table)
  Pmacro <- Pmacro/ncol(table)
  Rmicro <- TPc / Rmicro
  Pmicro <- TPc / Pmicro
  return(data.frame(tab.precision, tab.recall, fmeasure, acc, Rmacro, Pmacro, Rmicro, Pmicro, row.names=classes))
}

doCV=function(data, kfold, algo){
  #lNaive;lSVM;lRF=list()
  lNaive=list()
  for (i in 1:kfold) {
    #fitNaive = naiveBayes(type ~., data=data[data$fold != i,-13])
    #fitSVM <- svm(type ~., data = data[data$fold != i,-13])
    #fitRandomForest <- randomForest(type ~., data = data[data$fold != i,-13], importance=TRUE)
    
    #predictionsNaive = predict(fitNaive, data[data$fold == i, -c(12,13)])
    #predictionsSVM = predict(fitSVM, data[data$fold == i, -c(12,13)])
    #predictionsRF = predict(fitRandomForest, data[data$fold == i, -c(12,13)])
    
    dfNaive = as.data.frame(getPRF(algo(i, data)))
    dfNaive[is.na(dfNaive)] <- 0
    lNaive[[i]] = dfNaive
  }
  return (lNaive)
}
l=doCV(d2, 10, func_NB)

func_NB=function(fold, data){
  fitNaive = naiveBayes(type ~., data=data[data$fold != fold,-13])
  predictionsNaive = predict(fitNaive, data[data$fold == fold, -c(12,13)])
  return(table(predictionsNaive, d2[d2$fold==fold,]$type))
}

dfSVM = as.data.frame(getPRF(predictionsSVM, data, i))
dfSVM[is.na(dfSVM)] <- 0
lSVM[[i]] = dfSVM
dfRF = as.data.frame(getPRF(predictionsRF, data, i))
dfRF[is.na(dfSVM)] <- 0
lRF[[i]] = dfRF


  if(i == 1){
    precisionNaive <- as.data.frame(dfNaive[,1], row.names=classes)
    recallNaive <- as.data.frame(dfNaive[,2], row.names=classes)
    fmeasureNaive <- as.data.frame(dfNaive[,3], row.names=classes)
    accNaive <- as.data.frame(dfNaive[,4], row.names=classes)
    precisionSVM <- as.data.frame(dfSVM[,1], row.names=classes)
    recallSVM <- as.data.frame(dfSVM[,2], row.names=classes)
    fmeasureSVM <- as.data.frame(dfSVM[,3], row.names=classes)
    accSVM <- as.data.frame(dfSVM[,4], row.names=classes)
    precisionRF <- as.data.frame(dfRF[,1], row.names=classes)
    recallRF <- as.data.frame(dfRF[,2], row.names=classes)
    fmeasureRF <- as.data.frame(dfRF[,3], row.names=classes)
    accRF <- as.data.frame(dfRF[,4], row.names=classes)
    PmicroNaive <- as.data.frame(dfNaive[1,8], row.names=classes)
    RmicroNaive <- as.data.frame(dfNaive[1,7], row.names=classes)
    PmacroNaive <- as.data.frame(dfNaive[1,6], row.names=classes)
    RmacroNaive <- as.data.frame(dfNaive[1,5], row.names=classes)
    PmicroSVM <- as.data.frame(getPRF(predictionsSVM,d2,i)[1,8], row.names=classes)
    RmicroSVM <- as.data.frame(getPRF(predictionsSVM,d2,i)[1,7], row.names=classes)
    PmacroSVM <- as.data.frame(getPRF(predictionsSVM,d2,i)[1,6], row.names=classes)
    RmacroSVM <- as.data.frame(getPRF(predictionsSVM,d2,i)[1,5], row.names=classes)
    PmicroRF <- as.data.frame(getPRF(predictionsRF,d2,i)[1,8], row.names=classes)
    RmicroRF <- as.data.frame(getPRF(predictionsRF,d2,i)[1,7], row.names=classes)
    PmacroRF <- as.data.frame(getPRF(predictionsRF,d2,i)[1,6], row.names=classes)
    RmacroRF <- as.data.frame(getPRF(predictionsRF,d2,i)[1,5], row.names=classes)
  }
  else{
    precisionNaive[,i] <- as.data.frame(getPRF(predictionsNaive, d2, i)[,1])
    recallNaive[,i] <- as.data.frame(getPRF(predictionsNaive, d2, i)[,2])
    fmeasureNaive[,i] <- as.data.frame(getPRF(predictionsNaive, d2, i)[,3])
    accNaive[,i] <- as.data.frame(getPRF(predictionsNaive, d2, i)[,4])
    precisionSVM[,i] <- as.data.frame(getPRF(predictionsSVM, d2, i)[,1])
    recallSVM[,i] <- as.data.frame(getPRF(predictionsSVM, d2, i)[,2])
    fmeasureSVM[,i] <- as.data.frame(getPRF(predictionsSVM, d2, i)[,3])
    accSVM[,i] <- as.data.frame(getPRF(predictionsSVM, d2, i)[,4])
    precisionRF[,i] <- as.data.frame(getPRF(predictionsSVM, d2, i)[,1])
    recallRF[,i] <- as.data.frame(getPRF(predictionsSVM, d2, i)[,2])
    fmeasureRF[,i] <- as.data.frame(getPRF(predictionsSVM, d2, i)[,3])
    accRF[,i] <- as.data.frame(getPRF(predictionsRF, d2, i)[,4])
    PmicroNaive[,i] <- as.data.frame(getPRF(predictionsNaive,d2,i)[1,8], row.names=classes, na.action=0)
    RmicroNaive[,i] <- as.data.frame(getPRF(predictionsNaive,d2,i)[1,7], row.names=classes, na.action=0)
    PmacroNaive[,i] <- as.data.frame(getPRF(predictionsNaive,d2,i)[1,6], row.names=classes, na.action=0)
    RmacroNaive[,i] <- as.data.frame(getPRF(predictionsNaive,d2,i)[1,5], row.names=classes, na.action=0)
    PmicroSVM[,i] <- as.data.frame(getPRF(predictionsSVM,d2,i)[1,8], row.names=classes, na.action=0)
    RmicroSVM[,i] <- as.data.frame(getPRF(predictionsSVM,d2,i)[1,7], row.names=classes, na.action=0)
    PmacroSVM[,i] <- as.data.frame(getPRF(predictionsSVM,d2,i)[1,6], row.names=classes, na.action=0)
    RmacroSVM[,i] <- as.data.frame(getPRF(predictionsSVM,d2,i)[1,5], row.names=classes, na.action=0)
    PmicroRF[,i] <- as.data.frame(getPRF(predictionsRF,d2,i)[1,8], row.names=classes, na.action=0)
    RmicroRF[,i] <- as.data.frame(getPRF(predictionsRF,d2,i)[1,7], row.names=classes, na.action=0)
    PmacroRF[,i] <- as.data.frame(getPRF(predictionsRF,d2,i)[1,6], row.names=classes, na.action=0)
    RmacroRF[,i] <- as.data.frame(getPRF(predictionsRF,d2,i)[1,5], row.names=classes, na.action=0)
  }
}
columns <- c('Fold 1', 'Fold 2', 'Fold 3', 'Fold 4', 'Fold 5', 'Fold 6', 'Fold 7', 'Fold 8', 'Fold 9', 'Fold 10')
colnames(precisionNaive) <- columns
colnames(recallNaive) <- columns
colnames(fmeasureNaive) <- columns
colnames(precisionSVM) <- columns
colnames(recallSVM) <- columns
colnames(fmeasureSVM) <- columns
colnames(precisionRF) <- columns
colnames(recallRF) <- columns
colnames(fmeasureRF) <- columns
colnames(accRF) <- columns
colnames(accSVM) <- columns
colnames(accNaive) <- columns

#get the average accuracy for each fold and each classifier
getAverage=function(accNaive, accSVM, accRF, PmicroNaive, PmicroSVM, PmicroRF, RmicroNaive, RmicroSVM, RmicroRF, RmacroNaive, RmacroSVM, RmacroRF, PmacroNaive, PmacroSVM, PmacroRF){
  tmp <- sapply(accNaive, mean)
  res <- as.data.frame(tmp)
  names <- row.names(res)
  res <- rbind(res, mean(tmp))
  tmp <- sapply(accSVM, mean)
  res1 <- as.data.frame(tmp)
  res1 <- rbind(res1, mean(tmp))
  res <- cbind(res, res1)
  tmp <- sapply(accRF, mean)
  res2 <- as.data.frame(tmp)
  res2 <- rbind(res2, mean(tmp))
  res <- cbind(res, res2)
  v <- c(mean(RmacroNaive[1,]), mean(RmacroSVM[1,]), mean(RmacroRF[1,])
  res <- rbind(res, as.data.frame(v))
  v <- c(mean(PmacroNaive[1,]), mean(PmacroSVM[1,]), mean(PmacroRF[1,])
  res <- rbind(res, as.data.frame(v))
  v <- c(mean(RmicroNaive[1,]), mean(RmicroSVM[1,]), mean(RmicroRF[1,])
  res <- rbind(res, as.data.frame(v))
  v <- c(mean(PmicroNaive[1,]), mean(PmicroSVM[1,]), mean(PmicroRF[1,])
  res <- rbind(res, as.data.frame(v))
  names <- append(names, values=c("Average", "Rmacro", "Pmacro", "Rmicro", "Pmicro"), after=11)
  row.names(res) <- names
  colnames(res) <- c("Naive Bayes", "SVM", "Random Forest")
  return(res)
}
getAverage(accNaive, accSVM, accRF, PmicroNaive, PmicroSVM, PmicroRF, RmicroNaive, RmicroSVM, RmicroRF, RmacroNaive, RmacroSVM, RmacroRF, PmacroNaive, PmacroSVM, PmacroRF)