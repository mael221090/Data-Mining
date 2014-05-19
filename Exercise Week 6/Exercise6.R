setwd('~/Desktop//Courses/Warwick//Data Mining/Exercise 6/')

perceptron=function(D, eta, class){
  r <- data.frame()
  b <- 0.1
  converged <- FALSE
  w <- rep(1,(ncol(D) - 1))
  while(!converged){
    converged <- TRUE 
    for(i in 1:nrow(D)){
      a <- sum(w*D[i,]) + b
      # output y != input class(y)
      if(class[i]*a <= 0){
        w <- w + class[i]*eta*D[i,]
        r <- rbind(r, w)
        converged <- FALSE
      }
    }
  }
  return (r)
}
for(i in 1:20){
  w <- perceptron(train, 5, train$Species)
}

iris
d <- iris[iris$Species == c('setosa', 'versicolor'), -(2:3)]
row.names(d) <- NULL
d$Species <- as.numeric(d$Species == "setosa")
d$Species[d$Species == 0] <- -1
training <- sample(1:nrow(d), nrow(d), replace=FALSE)
train <- subset(d[training,])
row.names(train) <- NULL


for(i in 1:20){
  w <- perceptron(d[,-3], 0.1, d$Species)
}

res <- data.frame()
bla <- c(0.1,0.5,1,2,5,8)
for(i in bla){
  w <- perceptron(train[,-3], i, train$Species)
  res <- rbind(res, w)
}
row.names(res) <- NULL

a <- data.frame()
for (i in 1: nrow(res)) {
  w1 <- res[i,1]
  w2 <- res[i,2]
  b<-  0.1
  k <- res[i,4]
  plot(train[,-3], main=paste("step", i), pch=ifelse(train$Species > 0, 1, 2))
  a <- rbind(a,-b/w2)
}

x <- c(2.2, 3, 1.1, 7.9, 4.1, 3.7, 3.4, 2.0, 5.5, 7.0, 4.3, 6.6, 12)
y <- c(6.5, 5.1, 2.9, 9.4, 7.1, 8.9, 8, 11.3, 9.0, 9.9, 6.6, 8.9, 15)
class<-c(1,1,1,1,1,1,1,1,1,1,1,1,1)
m<-data.frame()
m<-cbind(x,y,class)
x2<-c(1,2,3,4.2,4.5,5.0, 5.7, 6.2,6.6,7.0)
y2<-c(0.1,0.9,1.1,2.5,3.0,3.1,3.6,4.5,5.7,6.6)
c2<-c(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1)
for(i in 1:length(x2)){
  m <- rbind(m, x2[i])
  m[i + 13,2] <- y2[i]
  m[i + 13,3] <- c2[i]
}
plot(m[,-3], main='Linearly separable dataset', pch=ifelse(m[,3] > 0, 1, 2))
abline(1,1)
legend('bottomright', legend=c('1', '-1'), pch=c(1,2))
w <- perceptron(m[,-3], 0.1, m[,3])
plot(m[,-3], main='Perceptron classifier', pch=ifelse(m[,3] > 0, 1, 2))
for(i in 1:nrow(w)){
  w1 <- w[i,1]
  w2 <- w[i,2]
  b <- 0.1
  if(i != nrow(w))
    abline(-b/w2, -w1/w2,col='green')
  else
    abline(-b/w2, -w1/w2, col='red')
}
legend('bottomright', legend=c('1', '-1'), pch=c(1,2))


w <- perceptron(d[,-3], 5, d$Species)

for(i in 1:20){
  w <- perceptron(train[-3], 1, train$Species)
}

