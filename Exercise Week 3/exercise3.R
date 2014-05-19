#Question 1
par(mfrow=c(2,2))
hist(iris$Sepal.Length)
hist (iris$Sepal.Width)
hist (iris$Petal.Length)
hist (iris$Petal.Width)

par(mfrow=c(1,4))
boxplot(iris$Sepal.Length, xlab="Sepal.Length")
boxplot(iris$Sepal.Width, xlab="Sepal.Width")
boxplot(iris$Petal.Length, xlab="Petal.Length")
boxplot(iris$Petal.Width, xlab="Petal.Width")

#order: setosa, versicolor and virginica
pairs(iris[1:4], main = "Anderson's Iris Data -- 3 species", pch=21, bg = c("red", "green3", "blue")[unclass(iris$Species)])

#Question 2
irisMissing = read.csv("irismissing.csv")
sum(is.na(irisMissing)) 

#Question 3
irisDrop <- na.omit(irisMissing)

#Question 4
replaceMissingValuesByMean=function(x){
  for(attribute in 1:length(names(x))){
    if(is.numeric(x[,attribute])) 
    {
      z <- mean(x[,attribute], na.rm = TRUE)
      x[is.na(x[,attribute]), attribute] <- z
    }
  }
  return(x)
}
replaceMissingValuesByMedian=function(x){
  for(attribute in 1:length(names(x))){
    if(is.numeric(x[,attribute])) 
    {
      z <- median(x[,attribute], na.rm = TRUE)
      x[is.na(x[,attribute]), attribute] <- z
    }
  }
  return(x)
}
getMode=function(x){
  z <- table(x)
  as.numeric(names(z)[z == max(z)]) 
}
replaceMissingValuesByMode=function(x){
  for(attribute in 1:length(names(x))){
    if(is.numeric(x[,attribute])) 
    {
      z <- getMode(x[,attribute])
      x[is.na(x[,attribute]), attribute] <- z
    }
  }
  return(x)
}

#Question 5
foo=function(x, FUNC){
  d <- FUNC(x)
  return(d)
}

#Question 6
irisDropMean = foo(irisMissing, replaceMissingValuesByMean)
irisDropMedian = foo(irisMissing, replaceMissingValuesByMedian)
irisDropMode = foo(irisMissing, replaceMissingValuesByMode)

par(mfrow=c(2,2))
hist(irisDropMean$Sepal.Width)
hist(irisDropMedian$Sepal.Width)
hist(irisDropMode$Sepal.Width)

par(mfrow=c(1,3))
boxplot(irisDropMean$Sepal.Width, xlab="Sepal.Width")
boxplot(irisDropMedian$Sepal.Width, xlab="Sepal.Width")
boxplot(irisDropMode$Sepal.Width, xlab="Sepal.Width")