
egret_flow_smoothing<-function(x,y,window=20,edgeAdjust=T){
  y=log(y)
baseYear <- trunc(x[1])
numYears <- length(x)
xVec <- seq(1, numYears)
xy <- data.frame(x, y, xVec)
xy <- na.omit(xy)
goodYears <- length(xy$x)
x <- xy$x
x1 <- x[1]
xn <- x[goodYears]
zVec<-x
for (i in 1:goodYears) {
  xi <- x[i]
  distToEdge <- min((xi - x1), (xn - xi))
  close <- (distToEdge < window)
  thisWindow <- if (edgeAdjust & close) {
    (2 * window) - distToEdge
  }
  else {
    window
  }
  w <- triCube(x - xi, thisWindow)
  mod <- lm(xy$y ~ x, weights = w)
  new <- data.frame(x = x[i])
  z <- exp(predict(mod, new))
  iYear <- xy$xVec[i]
  zVec[i]<-z
}
zVec
}
