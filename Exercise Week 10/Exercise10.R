setwd('~/Desktop//Courses/Warwick//Data Mining/Exercise 10/')

setwd('~/Desktop//Courses/Warwick//Sensor Networks And Mobile Data Communications/Lab 4/')
x <- c(0, 300)
y <- c(0,0)
x1 <- c(150,136.597,117,108.762,103.533,85.8215,105.817,115.241,115.584,95.8562)
y1 <- c(0,-14.8849,-18.8365,-37.0613,-56.3656,-65.6554,-65.224,-47.5835,-27.5864,-30.8748)
png("part2.png")
plot(x1,y1, pch=19,xlim=c(0,300),ylim=c(0,-70), col='red', xlab="X-coordinate", ylab="Y-coordinate", main="Nodes' Locations")
points(x[1],y[1], col="green", pch=19)
points(x[2],y[2], col="blue", pch=19)
for(i in 1:length(x1)){
  arrows(x1[i],y1[i],x1[i+1],y1[i+1], col='red')
}
draw.circle(0, 0, 200, col = NA, border="black")
draw.circle(300, 0, 200, col = NA, border="black")
legend('topright', c('Node 0', 'Node 2', 'Node 1') , 
       lty=1, col=c('green', 'blue', 'red'), bty='n', cex=.75)
dev.off()