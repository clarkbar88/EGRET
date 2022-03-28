##plotDiffContours_WY

plotDiffContours_WY<-function (eList, year0, year1, 
                            qBottom=NA, qTop=NA, maxDiff=NA, 
                            whatSurface = 3, tcl=0.03,
                            qUnit = 2, span = 60, pval = 0.05, printTitle = TRUE, plotPercent = FALSE,
                            vert1 = NA, vert2 = NA, horiz = NA, flowDuration = TRUE, yTicks=NA,tick.lwd=1,
                            lwd=2,cex.main=0.95,cex.axis=1,customPar=FALSE,usgsStyle = FALSE,
                            color.palette=colorRampPalette(c("blue","white","red")),...) {
  
  localINFO <- getInfo(eList)
  localDaily <- getDaily(eList)
  localsurfaces <- getSurfaces(eList)
  
  if (is.numeric(qUnit)) {
    qUnit <- qConst[shortCode = qUnit][[1]]
  } else if (is.character(qUnit)) {
    qUnit <- qConst[qUnit][[1]]
  }
  
  if(!customPar){
    par(mgp=c(2.5,0.5,0))
  }
  
  surfaceName <- c("log of Concentration", "Standard Error of log(C)", 
                   "Concentration")
  j <- 3
  j <- if (whatSurface == 1) 1 else j
  j <- if (whatSurface == 2) 2 else j
  surf <- localsurfaces
  
  bottomLogQ <- localINFO$bottomLogQ
  stepLogQ <- localINFO$stepLogQ
  nVectorLogQ <- localINFO$nVectorLogQ
  bottomYear <- localINFO$bottomYear
  stepYear <- localINFO$stepYear
  nVectorYear <- localINFO$nVectorYear
  start0<-round((year0-bottomYear+.833)*16)-15
  end0<-start0+16
  start1<-round((year1-bottomYear+.833)*16)-15
  end1<-start1+16
  if (plotPercent) {
    diff<-(surf[,start1:end1,j] - surf[,start0:end0,j])*100/surf[,start0:end0,j]
  } else {
    diff<-surf[,start1:end1,j] - surf[,start0:end0,j]
  }
  difft<-t(diff)
  if(!is.na(maxDiff)){
    if(length(maxDiff) == 1){
      surfaceSpan <- c(-maxDiff,maxDiff)
    } else {
      surfaceSpan <- range(maxDiff)
    }
  } else {
    surfaceSpan <- quantile(difft, c(0.05,0.95))
  }
  
  contourLevels <- pretty(surfaceSpan, n = 15)
  x <- seq(0,1,stepYear)
  y <- ((1:nVectorLogQ) * stepLogQ) + (bottomLogQ - stepLogQ)
  yLQ <- y
  qFactor <- qUnit@qUnitFactor
  y <- exp(y) * qFactor
  numX <- length(x)
  numY <- length(y)
  
  qBottomT <- ifelse(is.na(qBottom), quantile(localDaily$Q, probs = 0.05)*qFactor, qBottom)
  
  qTopT <- ifelse(is.na(qTop), quantile(localDaily$Q, probs = 0.95)*qFactor, qTop)
  
  if(any(is.na(yTicks))){
    
    if(is.na(qBottom)){
      qBottom <- max(0.9*y[1],qBottomT)
    }
    if(is.na(qTop)){
      qTop <- min(1.1*y[numY],qTopT)
    }
    yTicks <- logPretty3(qBottom,qTop)
  }
  
  xTicks <- c(0,0.0848,0.1642,0.249,0.331,0.416,0.498,0.583,0.668,0.750,0.835,0.917,1)
  #xLabels <- c("Jan1","Feb1","Mar1","Apr1","May1","Jun1","Jul1","Aug1","Sep1","Oct1","Nov1","Dec1","Jan1")
  xLabels <- c("Oct1","Nov1","Dec1","Jan1","Feb1","Mar1","Apr1","May1","Jun1","Jul1","Aug1","Sep1","Oct1")
  nxTicks<-length(xTicks)
  
  nYTicks <- length(yTicks)
  numDays <- length(localDaily$Day)
  freq <- rep(0, nVectorLogQ)
  plotTitle <- if (printTitle) 
    paste(localINFO$shortName, " ", localINFO$paramShortName, 
          "\nEstimated", surfaceName[j], "change from",year0,"to",year1)
  else ""
  
  if(flowDuration) {
    durSurf <- rep(0, 17 * nVectorLogQ)
    dim(durSurf) <- c(17, nVectorLogQ)
    #centerDays <- seq(1, 388, 22.9)
    centerDays <- seq(1+273, 388+273, 22.9)
    centerDays <- floor(centerDays)
    for (ix in 1:17) {
      startDay <- centerDays[ix] - span
      endDay <- centerDays[ix] + span
      goodDays <- seq(startDay, endDay, 1)
      goodDays <- ifelse(goodDays > 0, goodDays, goodDays + 
                           365)
      goodDays <- ifelse(goodDays < 366, goodDays, goodDays - 
                           365)
      numDays <- length(localDaily$Day)
      isGood <- localDaily$Day %in% goodDays 
      
      spanDaily <- data.frame(localDaily, isGood)
      spanDaily <- subset(spanDaily, isGood)
      n <- length(spanDaily$Day)
      LogQ <- spanDaily$LogQ
      for (jQ in 1:nVectorLogQ) {
        ind <- ifelse(LogQ < yLQ[jQ], 1, 0)
        freq[jQ] <- sum(ind)/n
      }
      
      durSurf[ix, ] <- freq
    }
    plevels <- c(pval, 1 - pval)
    pct1 <- format(plevels[1] * 100, digits = 2)
    pct2 <- format(plevels[2] * 100, digits = 2)
    
    firstLine <- paste(localINFO$shortName,"  ",localINFO$paramShortName,sep="")
    secondLine <- if (plotPercent){
      paste("\nEstimated", surfaceName[j], "percent change from",year0,"to",year1)
    } else {
      paste("\nEstimated", surfaceName[j], "change from",year0,"to",year1)
    }
    thirdLine <- paste("\nBlack lines are", pct1, "and", pct2, "flow percentiles")
    plotTitle <- paste(firstLine,secondLine,thirdLine)
    
  }
  vectorNone <- c(year0, log(yTicks[1], 10) - 1, year1, 
                  log(yTicks[1], 10) - 1)
  v1 <- if (is.na(vert1)) 
    vectorNone
  else c(vert1, log(yTicks[1], 10), vert1, log(yTicks[nYTicks], 10))
  v2 <- if (is.na(vert2)) 
    vectorNone
  else c(vert2, log(yTicks[1], 10), vert2, log(yTicks[nYTicks], 10))
  h1 <- if (is.na(horiz)) 
    vectorNone
  else c(year0, log(horiz, 10), year1, log(horiz, 10))
  
  deltaY <- (log(yTicks[length(yTicks)],10)-log(yTicks[1],10))/25
  deltaX <- (1)/25
  
  yLab <- ifelse(usgsStyle,qUnit@unitUSGS,qUnit@qUnitExpress)
  filled.contour(x, log(y, 10), difft, levels = contourLevels, 
                 xlim = c(0,1), ylim = c(log(yTicks[1], 
                                             10), log(yTicks[nYTicks], 10)), #main = plotTitle, 
                 xaxs = "i", yaxs = "i",
                 plot.axes = {
                   axis(1, tcl = 0, at = xTicks, labels = xLabels, cex.axis=0.9*cex.axis)
                   axis(2, tcl = 0, las = 1, at = log(yTicks, 10), 
                        labels = yTicks, cex.axis=cex.axis)
                   axis(3, tcl = 0, at = xTicks, labels =FALSE)
                   axis(4, tcl = 0, at = log(yTicks, 10), labels=FALSE)
                   if(flowDuration) contour(x, log(y, 10), durSurf, add = TRUE, drawlabels = FALSE, 
                                            levels = plevels,lwd=lwd)
                   segments(v1[1], v1[2], v1[3], v1[4])
                   segments(v2[1], v2[2], v2[3], v2[4])
                   segments(h1[1], h1[2], h1[3], h1[4])
                   
                   segments(xTicks, rep(log(yTicks[1],10),length(xTicks)), xTicks, rep(grconvertY(grconvertY(par("usr")[3],from="user",to="inches")+tcl,from="inches",to="user"),length(xTicks)), lwd = tick.lwd)
                   segments(xTicks, rep(log(yTicks[nYTicks],10),length(xTicks)), xTicks, rep(grconvertY(grconvertY(par("usr")[4],from="user",to="inches")-tcl,from="inches",to="user"),length(xTicks)), lwd = tick.lwd)
                   segments(rep(0,length(yTicks)), log(yTicks,10), rep(grconvertX(grconvertX(par("usr")[1],from="user",to="inches")+tcl,from="inches",to="user"),length(yTicks)),log(yTicks,10), lwd = tick.lwd)
                   segments(rep(grconvertX(grconvertX(par("usr")[2],from="user",to="inches")-tcl,from="inches",to="user"),length(yTicks)), log(yTicks,10), rep(1,length(yTicks)),log(yTicks,10), lwd = tick.lwd)
                   
                 },
                 plot.title = {
                   if(printTitle) {
                     title(main = plotTitle, ylab = yLab,cex.main = cex.main)
                   } else {
                     title(main = NULL, ylab = yLab)
                   }
                 }, 
                 color.palette=color.palette,...)
  
}
