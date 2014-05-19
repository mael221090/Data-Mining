HairEyeColor

HairEyeColorDF <- as.data.frame(HairEyeColor)

HEC <- HairEyeColorDF[rep(row.names(HairEyeColorDF), HairEyeColorDF$Freq), 1:3]
HEC
m <- naiveBayes(Sex ~ ., data = HEC)
m

NB=function(df, class){
  DF <- table(df[,class])
  res <- list()
  for(attribute in names(df)){
    if(attribute != class)
    {
      x <- table(df[,class], df[,attribute])
      res[[attribute]] <- prop.table(x, 1)
    }
  }
  return (list(prop.table(DF), res))
}

irisMissing <- read.csv('~/Desktop/Courses/Warwick/Data mining/Exercise 3/irismissing.csv')

disc=function(dataset, N){
  for(attribute in 1:length(names(dataset))){
    if(attribute == 2)
    {
      width <- (max(dataset[,attribute]) - min(dataset[,attribute]))/N
      dataset[,attribute] <- cut(dataset[,attribute], breaks = seq(min(dataset[,attribute]), max(dataset[,attribute]), by = width), include.lowest=TRUE, na.action=na.omit)
    }
  }
  return (dataset)
}
irisMissing$X <- NULL
irisMissing$X.1 <- NULL
i <- irisMissing
i2 <- disc(i2, 3)
i2<-i
i2 <- i2[!complete.cases(i2),]
i2 <- na.omit(i)
irisFinal <- rbind(i, i2)

model <- klaR::NaiveBayes(Sepal.Width ~ ., data = i2)
iMissing <- i[!complete.cases(i),]
l <- predict(model, iMissing)

rows = as.numeric(rownames(irisMissing[!complete.cases(irisMissing),]))
df = as.data.frame(l$class)
colnames(df)[1] <- "class"
r = as.numeric(rownames(i2))
df2 <- rbind(df, i2$Sepal.Width)

for(i in 1:length(rows))
{
  v <- as.vector(df[i, 'class'])
  irisMissing[rows[i],2] <- v[1]
}
for(i in 1:length(r))
{
  irisMissing[r[i],2] <- i2[i, 2]
}

l$class[2]

irisFinal <- rbind(iMissing, i)
modelFinal <- klaR::NaiveBayes(Species ~ ., data = irisFinal)
