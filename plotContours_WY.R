plotContours_WY<-function(eList, dateStart, dateEnd, qBottom=NA, qTop=NA, whatSurface = 3, 
                       qUnit = 2, contourLevels = NA, span = 60, pval = 0.05,
                       printTitle = TRUE, vert1 = NA, vert2 = NA, horiz = NA, tcl=0.03,
                       flowDuration = TRUE, customPar=FALSE, yTicks=NA,tick.lwd=1,usgsStyle = FALSE,
                       lwd=2,cex.main=1,cex.axis=1,color.palette=colorRampPalette(c("white","gray","blue","red")),...) {
  
  localINFO <- getInfo(eList)
  localDaily <- getDaily(eList)
  localsurfaces <- getSurfaces(eList)
  
  nVectorYear <- localINFO$nVectorYear
  bottomLogQ <- localINFO$bottomLogQ
  stepLogQ <- localINFO$stepLogQ
  nVectorLogQ <- localINFO$nVectorLogQ
  
  if (is.numeric(qUnit)){
    qUnit <- qConst[shortCode=qUnit][[1]]
  } else if (is.character(qUnit)){
    qUnit <- qConst[qUnit][[1]]
  }
  
  if(!customPar){
    par(mgp=c(2.5,0.5,0))
  }
  surfaceName<-c("log of Concentration","Standard Error of log(C)","Concentration")
  j<-3
  j<-if(whatSurface==1) 1 else j
  j<-if(whatSurface==2) 2 else j
  surf<-localsurfaces
  surfaceMin<-min(surf[,,j])
  surfaceMax<-max(surf[,,j])
  surfaceSpan<-c(surfaceMin,surfaceMax)
  contourLevels<-if(is.na(contourLevels[1])) pretty(surfaceSpan,n=5) else contourLevels
  # computing the indexing of the surface, the whole thing, not just the part being plotted
  if(all(c("Year","LogQ") %in% names(attributes(localsurfaces)))){
    x <- attr(localsurfaces, "Year")
    y <- attr(localsurfaces, "LogQ")
  } else {
    x <- seq(localINFO$bottomYear, by=localINFO$stepYear, length.out=localINFO$nVectorYear)
    y <- ((1:nVectorLogQ)*stepLogQ) + (bottomLogQ - stepLogQ)
  }
  
  yLQ<-y
  qFactor<-qUnit@qUnitFactor
  y<-exp(y)*qFactor
  numX<-length(x)
  numY<-length(y)
  
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
  
  if(lubridate::decimal_date(dateEnd)-lubridate::decimal_date(dateStart) >= 4){
    xSpan<-c(dateStart,dateEnd)
    xTicks<-pretty(xSpan,n=5)
    xlabels <- xTicks
  } else {
    xlabels <- c(as.Date(dateStart), as.Date(dateEnd))
    xlabels <- pretty(xlabels,n=5)
    xTicksDates <- as.POSIXlt(xlabels)
    years <- xTicksDates$year + 1900 
    day <- xTicksDates$yday
    xTicks <- years + day/365
    xlabels <- format(xlabels, "%Y-%b")
  }
  
  nYTicks<-length(yTicks)
  surfj<-surf[,,j]
  surft<-t(surfj)
  # the next section does the flow duration information, using the whole period of record in Daily, not just the graph period
  plotTitle<-if(printTitle) paste(localINFO$shortName," ",localINFO$paramShortName,"\nEstimated",surfaceName[j],"Surface in Color") else NULL
  if(flowDuration) {
    numDays<-length(localDaily$Day)
    freq<-rep(0,nVectorLogQ)
    durSurf<-rep(0,length(x)*length(y))
    dim(durSurf)<-c(length(x),length(y))
    centerDays<-seq(1,365,22.9)
    centerDays<-floor(centerDays)
    for (ix in 1:16) {
      startDay<-centerDays[ix]-span
      endDay<-centerDays[ix]+span
      goodDays<-seq(startDay,endDay,1)
      goodDays<-ifelse(goodDays>0,goodDays,goodDays+365)
      goodDays<-ifelse(goodDays<366,goodDays,goodDays-365)
      numDays<-length(localDaily$Day)
      isGood <- localDaily$Day %in% goodDays 
      
      spanDaily<-data.frame(localDaily,isGood)
      spanDaily<-subset(spanDaily,isGood)
      n<-length(spanDaily$Day)
      LogQ<-spanDaily$LogQ
      for(jQ in 1:length(y)) {
        ind<-ifelse(LogQ < yLQ[jQ],1,0)
        freq[jQ]<-sum(ind)/n
      }
      xInd<-seq(ix,numX,16)
      numXind<-length(xInd)
      for(ii in 1:numXind) {
        iX<-xInd[ii]
        durSurf[iX,]<-freq
      }
    }
    plevels<-c(pval,1-pval)
    pct1<-format(plevels[1]*100,digits=2)
    pct2<-format(plevels[2]*100,digits=2)
    plotTitle<- plotTitle<-if(printTitle)paste(localINFO$shortName,"  ",localINFO$paramShortName,"\nEstimated",surfaceName[j],"Surface in Color\nBlack lines are",pct1,"and",pct2,"flow percentiles")
  }
  # setting up for the possible 3 straight lines to go on the graph
  # if the lines aren't being plotted they are just located outside the plot area
  vectorNone<-c(lubridate::decimal_date(dateStart),log(yTicks[1],10)-1,lubridate::decimal_date(dateEnd),log(yTicks[1],10)-1)
  v1<-if(is.na(vert1)) vectorNone else c(vert1,log(yTicks[1],10),vert1,log(yTicks[nYTicks],10))
  v2<-if(is.na(vert2)) vectorNone else c(vert2,log(yTicks[1],10),vert2,log(yTicks[nYTicks],10))
  h1<-if(is.na(horiz)) vectorNone else c(lubridate::decimal_date(dateStart),log(horiz,10),lubridate::decimal_date(dateEnd),log(horiz,10))
  
  deltaY <- (log(yTicks[length(yTicks)],10)-log(yTicks[1],10))/25
  deltaX <- (lubridate::decimal_date(dateEnd)-lubridate::decimal_date(dateStart))/25
  
  yLab<-ifelse(usgsStyle,qUnit@unitUSGS,qUnit@qUnitExpress)
  
  logY <- log(y,10)
  filled.contour(x,log(y,10),surft,
                 levels=contourLevels,
                 xlim=lubridate::decimal_date(c(dateStart,dateEnd)),
                 ylim=c(log(yTicks[1],10),log(yTicks[nYTicks],10)),
                 xaxs="i",yaxs="i",
                 color.palette=color.palette, # ...,
                 plot.axes={

                   width <- grconvertX(par("usr")[2],from="user",to="inches") - grconvertX(par("usr")[1],from="user",to="inches")
                   height <- grconvertY(par("usr")[4],from="user",to="inches") - grconvertY(par("usr")[3],from="user",to="inches")

                   axis(1,tcl=0,at=xTicks,labels=xlabels,cex.axis=cex.axis)
                   axis(2,tcl=0,las=1,at=log(yTicks,10),labels=yTicks,cex.axis=cex.axis)
                   axis(3, tcl = 0, at = xTicks, labels =FALSE,cex.axis=cex.axis)
                   axis(4, tcl = 0, at = log(yTicks, 10), labels=FALSE,cex.axis=cex.axis)
                   if(flowDuration) contour(x,log(y,10),durSurf,add=TRUE,drawlabels=FALSE,levels=plevels,lwd=lwd)
                   segments(v1[1],v1[2],v1[3],v1[4])
                   segments(v2[1],v2[2],v2[3],v2[4])
                   segments(h1[1],h1[2],h1[3],h1[4])

                   segments(xTicks, rep(log(yTicks[1],10),length(xTicks)), xTicks, rep(grconvertY(grconvertY(par("usr")[3],from="user",to="inches")+tcl,from="inches",to="user"),length(xTicks)), lwd = tick.lwd)
                   segments(xTicks, rep(log(yTicks[nYTicks],10),length(xTicks)), xTicks, rep(grconvertY(grconvertY(par("usr")[4],from="user",to="inches")-tcl,from="inches",to="user"),length(xTicks)), lwd = tick.lwd)
                   segments(rep(lubridate::decimal_date(dateStart),length(yTicks)), log(yTicks,10), rep(grconvertX(grconvertX(par("usr")[1],from="user",to="inches")+tcl,from="inches",to="user"),length(yTicks)),log(yTicks,10), lwd = tick.lwd)
                   segments(rep(grconvertX(grconvertX(par("usr")[2],from="user",to="inches")-tcl,from="inches",to="user"),length(yTicks)), log(yTicks,10), rep(lubridate::decimal_date(dateEnd),length(yTicks)),log(yTicks,10), lwd = tick.lwd)
                 },
                 plot.title = {
                   if(printTitle) {
                     title(main = plotTitle, ylab = yLab,cex.main = cex.main)
                   } else {
                     title(main = NULL, ylab = yLab)
                   }
                 }
  )

  

# row.names(surft)<-x
# colnames(surft)<-log(y,10)
# surft
# tibble(DecYear=as.numeric(rep(row.names(testz),each=ncol(testz))),
#        Q=10^as.numeric(rep(colnames(testz),nrow(testz))),
#        Conc=as.vector(t(testz))) %>%
#   filter(DecYear>2020) %>%
#   ggplot()+
#   geom_raster(aes(x=DecYear,y=Q,fill=Conc),interpolate =T)+
#   geom_contour(aes(x=DecYear,y=Q,z=Conc),color='black')+
#   scale_y_log10()+
#   scale_fill_gradient2(low='white',high='red')+
#   geom_line(data=eList$Daily %>% filter(DecYear>=2020),aes(x=DecYear,y=Q*35.1324))
}
