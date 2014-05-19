loan = read.csv('../Exercise 4//Loan.csv')
disc=function(dataset, N){
  for(attribute in 1:length(names(dataset))){
    if(is.numeric(dataset[,attribute]))
    {
      width <- (max(dataset[,attribute]) - min(dataset[,attribute]))/N
      dataset[,attribute] <- cut(dataset[,attribute], breaks = seq(min(dataset[,attribute]), max(dataset[,attribute]), by = width), include.lowest=TRUE)
    }
  }
  return (dataset)
}
loanDisc=disc(loan, 4)
loanDisc

disc=function(dataset, N){
  intervals=c(1:N)
  for(attribute in 1:length(names(dataset))){
    if(is.numeric(dataset[,attribute]))
    {
      dataset <- dataset[order(dataset[,attribute]),]
      width <- (max(dataset[,attribute]) - min(dataset[,attribute]))/N
      tmp = dataset[1, attribute]
      for(i in 1:nrow(dataset)){
        if(dataset[i, attribute] == tmp)
          dataset[i, attribute] <- intervals[1]
        else
          dataset[i, attribute] <- intervals[ceiling((dataset[i, attribute]-tmp)/width)]
      }
    }
  }
  return(dataset)
}

disc(loan, 2)
intervals = c(1:5)
intervals[ceiling((loan[4, 1]-loan[1, 1])/8.2)]

entropy=function(x1, x2, n){
  res = -(x1/n)*log2(x1/n)-(x2/n)*log2(x2/n)
  return(res)
}
x1 = 3
x1 <- as.numeric(x1)
x2 = 3
x2 <- as.numeric(x2)
n = 6
n <- as.numeric(n)
entropy(x1, x2, n)

loanChi <- chiM(loan, 0.05)
play <- read.csv('play.csv')

getVal=function(o, e){
  tmp = e
  if(e < 0.5)
    tmp = 0.5
  res=(o-e)*(o-e)/tmp
  return (res)
}

getChi2_3=function(x1, x2, x3, y1, y2, y3){
  x = c(x1, x2, x3)
  y = c(y1, y2, y3)
  tc = c(1:length(x))
  t1 = 0
  t2 = 0
  for(i in 1:length(x)){
    t1 = t1 + x[i]
    t2 = t2 + y[i]
    tc[i] = x[i] + y[i]
  }
  t = c(t1, t2)
  total = t1 + t2
  ex = c(1:length(x))
  ey = c(1:length(y))
  for(i in 1:length(x)){
    ex[i] = t[1] * tc[i] / total
    ey[i] = t[2] * tc[i] / total
  }
  res = 0.0
  as.double(res)
  for(i in 1:length(x)){
    res = res + getVal(x[i], ex[i])
    res = res + getVal(y[i], ey[i])
  }
  return (res)
}

getChi2=function(x1, x2, y1, y2){
  x = c(x1, x2)
  y = c(y1, y2)
  tc = c(1:length(x))
  t1 = 0
  t2 = 0
  for(i in 1:length(x)){
    t1 = t1 + x[i]
    t2 = t2 + y[i]
    tc[i] = x[i] + y[i]
  }
  t = c(t1, t2)
  total = t1 + t2
  ex = c(1:length(x))
  ey = c(1:length(y))
  for(i in 1:length(x)){
    ex[i] = t[1] * tc[i] / total
    ey[i] = t[2] * tc[i] / total
  }
  res = 0.0
  as.double(res)
  for(i in 1:length(x)){
    res = res + getVal(x[i], ex[i])
    res = res + getVal(y[i], ey[i])
  }
  return (res)
}