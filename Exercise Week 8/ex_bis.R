setwd('~/Desktop//Courses/Warwick//Data Mining/Exercise Week 8/')
require(e1071)
require(randomForest)

data = read.csv('AI2013_papers.csv')

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

classes <- c('Case Study', 'Correspondence', 'Essay', 'Opinion', 'Perspective', 'Research', 'Review', 'Viewpoint')

#function that implements the three algorithms to be compared
func_NB=function(fold, data){
  fitNaive = naiveBayes(type ~., data=data[data$fold != fold,-13])
  predictionsNaive = predict(fitNaive, data[data$fold == fold, -c(12,13)])
  return(table(predictionsNaive, data[data$fold==fold,]$type))
}
func_SVM=function(fold, data){
  fitSVM <- svm(type ~., data = data[data$fold != fold,-13])
  predictionsSVM = predict(fitSVM, data[data$fold == fold, -c(12,13)])
  return(table(predictionsSVM, data[data$fold==fold,]$type))
}
func_RF=function(fold, data){
  fitRandomForest <- randomForest(type ~., data = data[data$fold != fold,-13], importance=TRUE)
  predictionsRF = predict(fitRandomForest, data[data$fold == fold, -c(12,13)])
  return(table(predictionsRF, data[data$fold==fold,]$type))
}

#get Precision, Recall, F-measure, Accuracy, Rmacro, Rmicro, Pmacro and Pmicro for a fold for a classifier
getPRF = function(table){
  tab.precision <- c()
  tab.recall <- c()
  fmeasure <- c()
  acc <- c()
  Rmacro <- 0
  Rmicro <- 0
  Pmicro <- 0
  Pmacro <- 0
  TPc <- 0
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
  Rmacro <- Rmacro/ncol(table)
  Pmacro <- Pmacro/ncol(table)
  Rmicro <- TPc / Rmicro
  Pmicro <- TPc / Pmicro
  return(data.frame(tab.precision, tab.recall, fmeasure, acc, Rmacro, Pmacro, Rmicro, Pmicro, row.names=classes))
}

#function that implements the cross-validation over a specific algorithm
doCV = function(data, kfold, algo){
  l=list()
  for (i in 1:kfold) {
    df = as.data.frame(getPRF(algo(i, data)))
    df[is.na(df)] <- 0
    l[[i]] = df
  }
  return (l)
}
d2<-d[sample(nrow(d)),]
d2$fold = cut(1:nrow(d2), breaks=10, labels=F)
row.names(d2) <- NULL

lNaive = doCV(d2, 10, func_NB)
lNaive

lSVM = doCV(d2, 10, func_SVM)
lSVM

lRF = doCV(d2, 10, func_RF)
lRF

#get the average accuracy for each fold and each classifier
getAverage = function(l){
  folds <- c('Fold 1', 'Fold 2', 'Fold 3', 'Fold 4', 'Fold 5', 'Fold 6', 'Fold 7', 'Fold 8', 'Fold 9', 'Fold 10', 'Average')
  x <- numeric(11)
  df <- data.frame(x,x,x,x,x,x,x,x, row.names=folds)
  colnames(df) <- c('Precision', 'Recall', 'Fmeasure', 'Accuracy', 'Rmacro', 'Pmacro', 'Rmicro', 'Pmicro')
  #browse by column
  for(j in 1:dim(l[[1]])[1]){
    #browse by folds
    for(i in 1:length(l)){
      df[i,j] <- sapply(l[[i]][j], mean)
    }
  }
  df[11,] <- apply(df[-11,], 2, mean)
  return(df)
}

dfNaive <- getAverage(lNaive)
dfNaive

dfSVM <- getAverage(lSVM)
dfSVM

dfRF <- getAverage(lRF)
dfRF

getTvalue = function(dfNaive, dfSVM, dfRF){
  folds <- c('Fold 1', 'Fold 2', 'Fold 3', 'Fold 4', 'Fold 5', 'Fold 6', 'Fold 7', 'Fold 8', 'Fold 9', 'Fold 10', 'Average', 'Stdev', 't-value')
  cols <- c('NB-SVM', 'NB-RF', 'SVM-RF')
  x <- numeric(13)
  df <- data.frame(x,x,x, row.names=folds)
  #extract the average and standard deviation of each of the three classfiers to calculate the t-test
  for(i in 1:nrow(dfNaive)){
    df[i,1] <- dfNaive[i,4] - dfSVM[i,4]
    df[i,2] <- dfNaive[i,4] - dfRF[i,4]
    df[i,3] <- dfSVM[i,4] - dfRF[i,4]
  }
  df[11,] <- apply(df[-c(11,12,13),], 2, mean)
  df[12,] <- apply(df[-c(11,12,13),], 2, sd)
  for(i in 1:ncol(df)){
    df[13,i] <- abs(df[11,i]/(df[12,i]/sqrt(10)))
  }
  colnames(df) <- cols
  return (df)
}
dTvalue <- getTvalue(dfNaive, dfSVM, dfRF)
dTvalue