plot = function(x,...){
  UseMethod("plot")
}

plot.QCCTSShewhartSgp<- function(Data,PhaseIIData,ChartType)
{

  if(!is.Partition.SgpSummary(Data))
    stop("'Data' must be a 'Partition.SgpSummary' object")

  if(!is.SgpSummary(PhaseIIData))
    stop("'Data' must be a 'SgpSummary' object")

  ## Shewhart Xbar-chart

  if(ChartType=="Xbar"){

    par(mar = c(9, 5, 3, 5))
    par(las=1)

    PhaseIIData$SgpSummary$Decision=FALSE

    #Data=PartitonedData

    plot(c(Data$SgpSummary$SgpMean,PhaseIIData$SgpSummary$SgpMean),col=ifelse(c(Data$SgpSummary$Decision,PhaseIIData$SgpSummary$Decision)==TRUE, "red", "black"),mgp = c(6, 1, 0),ylab="s",type = "o",pch=16,xlab="Date(Subgroup)",ylim=c(min(c(Data$SgpSummary$SgpMean,PhaseIIData$SgpSummary$SgpMean))-0.01,max(c(Data$SgpSummary$SgpMean,PhaseIIData$SgpSummary$SgpMean))+0.01), xaxt = "n")
    abline(v=length(Data$SgpSummary$SgpDate))


    uclPhaseI <- ((Data$LookUpTab$b4Tab[match(Data$SgpSummary$SgpSize,Data$LookUpTab$LengthFreq)])*(Data$LookUpTab$c4Tab[match(Data$SgpSummary$SgpSize,Data$LookUpTab$LengthFreq)])* Data$PhaseIIParam$Sbar)
    uclPhaseII <- ((PhaseIIData$LookUpTab$b4Tab[match(PhaseIIData$SgpSummary$SgpSize,PhaseIIData$LookUpTab$LengthFreq)])*(PhaseIIData$LookUpTab$c4Tab[match(PhaseIIData$SgpSummary$SgpSize,PhaseIIData$LookUpTab$LengthFreq)])*Data$PhaseIIParam$Sbar)

    ucl=c(uclPhaseI,uclPhaseII)
    par(new=TRUE)
    plot(ucl, col = "red",pch="-",type="s",ylim=c(min(c(Data$SgpSummary$AjSgpStd,PhaseIIData$SgpSummary$AjSgpStd))-0.01,max(c(Data$SgpSummary$AjSgpStd,PhaseIIData$SgpSummary$AjSgpStd))+0.01), xlab="",ylab="s",xaxt = "n",yaxt="n")

    mtext("UCL",  EAST <-4, at= mean(ucl), line=0.25, cex=0.8, las=1)

    abline(h = Data$PhaseIIParam$Xbar, col = "darkgreen",lty=1)

    mtext("Center Line",  EAST <-4, at= Data$PhaseIIParam$Sbar, line=0.25, cex=0.8, las=1)

      lclPhaseI <- ((Data$LookUpTab$b3Tab[match(Data$SgpSummary$SgpSize,Data$LookUpTab$LengthFreq)])*(Data$LookUpTab$c4Tab[match(Data$SgpSummary$SgpSize,Data$LookUpTab$LengthFreq)])* Data$PhaseIIParam$Sbar)
      lclPhaseII <- ((PhaseIIData$LookUpTab$b3Tab[match(PhaseIIData$SgpSummary$SgpSize,PhaseIIData$LookUpTab$LengthFreq)])*(PhaseIIData$LookUpTab$c4Tab[match(PhaseIIData$SgpSummary$SgpSize,PhaseIIData$LookUpTab$LengthFreq)])*Data$PhaseIIParam$Sbar)

      lcl <- c(lclPhaseI,lclPhaseII)

      par(new=TRUE)
      plot(lcl, col = "red",pch="-",type="s",ylim=c(min(c(Data$SgpSummary$AjSgpStd,PhaseIIData$SgpSummary$AjSgpStd))-0.01,max(c(Data$SgpSummary$AjSgpStd,PhaseIIData$SgpSummary$AjSgpStd))+0.01), xlab="",ylab="s",xaxt = "n",yaxt="n")

      mtext("LCL",  EAST <-4, at= mean(lcl), line=0.25, cex=0.8, las=1)


    TotalDates=c(Data$SgpSummary$SgpDate,PhaseIIData$SgpSummary$SgpDate)
    axis(1, at=seq(1,length(TotalDates),4), labels=TotalDates[seq(1,length(TotalDates),4)],las=2)

    text(15, max(c(Data$SgpSummary$AjSgpStd,PhaseIIData$SgpSummary$AjSgpStd)), "Phase I")

    text(length(Data$SgpSummary$SgpDate)+6, max(c(Data$SgpSummary$AjSgpStd,PhaseIIData$SgpSummary$AjSgpStd)), "Phase II")

  }


  ##  Shewhart S-chart




  if(ChartType=="S"){

    par(mar = c(9, 5, 3, 5))
    par(las=1)

    PhaseIIData$SgpSummary$Decision=FALSE

    plot(c(Data$SgpSummary$AjSgpStd,PhaseIIData$SgpSummary$AjSgpStd),col=ifelse(c(Data$SgpSummary$Decision,PhaseIIData$SgpSummary$Decision)==TRUE, "red", "black"),mgp = c(6, 1, 0),ylab="s",type = "o",pch=16,xlab="Date(Subgroup)",ylim=c(min(c(Data$SgpSummary$AjSgpStd,PhaseIIData$SgpSummary$AjSgpStd))-0.01,max(c(Data$SgpSummary$AjSgpStd,PhaseIIData$SgpSummary$AjSgpStd))+0.01), xaxt = "n")
    abline(v=length(Data$SgpSummary$SgpDate))


    uclPhaseI <- ((Data$LookUpTab$b4Tab[match(Data$SgpSummary$SgpSize,Data$LookUpTab$LengthFreq)])*(Data$LookUpTab$c4Tab[match(Data$SgpSummary$SgpSize,Data$LookUpTab$LengthFreq)])* Data$PhaseIIParam$Sbar)
    uclPhaseII <- ((PhaseIIData$LookUpTab$b4Tab[match(PhaseIIData$SgpSummary$SgpSize,PhaseIIData$LookUpTab$LengthFreq)])*(PhaseIIData$LookUpTab$c4Tab[match(PhaseIIData$SgpSummary$SgpSize,PhaseIIData$LookUpTab$LengthFreq)])*Data$PhaseIIParam$Sbar)

    ucl=c(uclPhaseI,uclPhaseII)
    par(new=TRUE)
    plot(ucl, col = "red",pch="-",type="s",ylim=c(min(c(Data$SgpSummary$AjSgpStd,PhaseIIData$SgpSummary$AjSgpStd))-0.01,max(c(Data$SgpSummary$AjSgpStd,PhaseIIData$SgpSummary$AjSgpStd))+0.01), xlab="",ylab="s",xaxt = "n",yaxt="n")

    mtext("UCL",  EAST <-4, at= mean(ucl), line=0.25, cex=0.8, las=1)

    abline(h = Data$PhaseIIParam$Sbar, col = "darkgreen",lty=1)

    mtext("Center Line",  EAST <-4, at= Data$PhaseIIParam$Sbar, line=0.25, cex=0.8, las=1)

    if(max(Data$SgpSummary$SgpSize)>5){

    lclPhaseI <- ((Data$LookUpTab$b3Tab[match(Data$SgpSummary$SgpSize,Data$LookUpTab$LengthFreq)])*(Data$LookUpTab$c4Tab[match(Data$SgpSummary$SgpSize,Data$LookUpTab$LengthFreq)])* Data$PhaseIIParam$Sbar)
    lclPhaseII <- ((PhaseIIData$LookUpTab$b3Tab[match(PhaseIIData$SgpSummary$SgpSize,PhaseIIData$LookUpTab$LengthFreq)])*(PhaseIIData$LookUpTab$c4Tab[match(PhaseIIData$SgpSummary$SgpSize,PhaseIIData$LookUpTab$LengthFreq)])*Data$PhaseIIParam$Sbar)

    lcl <- c(lclPhaseI,lclPhaseII)

     par(new=TRUE)
     plot(lcl, col = "red",pch="-",type="s",ylim=c(min(c(Data$SgpSummary$AjSgpStd,PhaseIIData$SgpSummary$AjSgpStd))-0.01,max(c(Data$SgpSummary$AjSgpStd,PhaseIIData$SgpSummary$AjSgpStd))+0.01), xlab="",ylab="s",xaxt = "n",yaxt="n")

      mtext("LCL",  EAST <-4, at= mean(lcl), line=0.25, cex=0.8, las=1)
    }

    TotalDates=c(Data$SgpSummary$SgpDate,PhaseIIData$SgpSummary$SgpDate)
    axis(1, at=seq(1,length(TotalDates),4), labels=TotalDates[seq(1,length(TotalDates),4)],las=2)

    text(15, max(c(Data$SgpSummary$AjSgpStd,PhaseIIData$SgpSummary$AjSgpStd)), "Phase I")

    text(length(Data$SgpSummary$SgpDate)+6, max(c(Data$SgpSummary$AjSgpStd,PhaseIIData$SgpSummary$AjSgpStd)), "Phase II")

 }
  invisible()
}


