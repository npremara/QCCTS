#' Subgroup partition into common and special causes based on the Shewhart S chart rules
#'
#' ' Function separates subgroups into common and special cause periods.
#'
#' @param Data subgroup summary (object type "SgpSummary")
#' @param ChartType Chart type for partitioning rule: if "Xbar"- Shewhart X-bar chart (mean), "R"- Shewhart control chart for range, "S"- Shewhart S chart
#' @return Subgroups in common and special caused periods
#' @author Nadeeka Premarathna
#' @references Nadeeka Premarathna,  A. Jonathan R. Godfrey and K. Govindaraju. "Decomposition of stock market trade-offs using Shewhart methodology." International Journal of Quality & Reliability Management 33, no. 9 (2016): 1311-1331.
#' @examples
#' SubgroupCriteria="weeks"
#' data(AAPL)
#' require(zoo)
#' StockPrice=window(AAPL[,"Close"],start=as.Date("2012-01-02"),end=as.Date("2013-12-31"))
#' Subgroups=Subgrouping(StockPrice,SubgroupCriteria,CountSgps=1)
#' DataSum=SubgroupSummary(Subgroups,MaxSgpSize=5)
#' B=PartitioningSgps(DataSum,ChartType="S")
#' @export
#' @seealso \code{\link{Subgrouping}}, \code{\link{SubgroupSummary}}
PartitioningSgps<-function(Data, ChartType = c("Xbar", "R", "S"))
{
  if(!is.SgpSummary(Data))
    stop("'Data' must be a 'SgpSummary' object")

  Data$SgpSummary<-na.omit(Data$SgpSummary)

  # # xbar-chart
  #  Data$SgpSummary$Decision<-(Data$SgpSummary$AjSgpStd>((Data$LookUpTab$b4Tab[match(Data$SgpSummary$SgpSize,Data$LookUpTab$LengthFreq)])*(Data$LookUpTab$c4Tab[match(Data$SgpSummary$SgpSize,Data$LookUpTab$LengthFreq)])*mean(Data$SgpSummary$AjSgpStd,na.rm=T,trim=0.025)))
  #  p=0.0027
  #  PoissonUpperLimit=p*length(Data$SgpSummary$SgpSize)+3*sqrt(p*length(Data$SgpSummary$SgpSize))
  #  FalseCountDiff=FirstFalseCount=sum(Data$SgpSummary$Decision, na.rm=TRUE)
  #
  #  while (FalseCountDiff> PoissonUpperLimit){
  #    Data$SgpSummary[Data$SgpSummary$Decision==FALSE,][,"Decision"]<-(( Data$SgpSummary[Data$SgpSummary$Decision==FALSE,]$AjSgpStd>((Data$LookUpTab$b4Tab[match(Data$SgpSummary[Data$SgpSummary$Decision==FALSE,]$SgpSize,Data$LookUpTab$LengthFreq)])*(Data$LookUpTab$c4Tab[match(Data$SgpSummary[Data$SgpSummary$Decision==FALSE,]$SgpSize,Data$LookUpTab$LengthFreq)])* mean(Data$SgpSummary[Data$SgpSummary$Decision==FALSE,]$AjSgpStd,na.rm=T,trim=0.025))))
  #    FalseCount=sum(Data$SgpSummary$Decision, na.rm=TRUE)
  #    FalseCountDiff=FalseCount-FirstFalseCount
  #    FirstFalseCount=FalseCount
  #  }
  # # R-chart
  #  Data$SgpSummary$Decision<-(Data$SgpSummary$AjSgpStd>((Data$LookUpTab$b4Tab[match(Data$SgpSummary$SgpSize,Data$LookUpTab$LengthFreq)])*(Data$LookUpTab$c4Tab[match(Data$SgpSummary$SgpSize,Data$LookUpTab$LengthFreq)])*mean(Data$SgpSummary$AjSgpStd,na.rm=T,trim=0.025)))
  #  p=0.0027
  #  PoissonUpperLimit=p*length(Data$SgpSummary$SgpSize)+3*sqrt(p*length(Data$SgpSummary$SgpSize))
  #  FalseCountDiff=FirstFalseCount=sum(Data$SgpSummary$Decision, na.rm=TRUE)
  #
  #  while (FalseCountDiff> PoissonUpperLimit){
  #    Data$SgpSummary[Data$SgpSummary$Decision==FALSE,][,"Decision"]<-(( Data$SgpSummary[Data$SgpSummary$Decision==FALSE,]$AjSgpStd>((Data$LookUpTab$b4Tab[match(Data$SgpSummary[Data$SgpSummary$Decision==FALSE,]$SgpSize,Data$LookUpTab$LengthFreq)])*(Data$LookUpTab$c4Tab[match(Data$SgpSummary[Data$SgpSummary$Decision==FALSE,]$SgpSize,Data$LookUpTab$LengthFreq)])* mean(Data$SgpSummary[Data$SgpSummary$Decision==FALSE,]$AjSgpStd,na.rm=T,trim=0.025))))
  #    FalseCount=sum(Data$SgpSummary$Decision, na.rm=TRUE)
  #    FalseCountDiff=FalseCount-FirstFalseCount
  #    FirstFalseCount=FalseCount
  #  }

  # S-chart
  if (ChartType=="S"){
  Data$SgpSummary$Decision<-(Data$SgpSummary$AjSgpStd>((Data$LookUpTab$b4Tab[match(Data$SgpSummary$SgpSize,Data$LookUpTab$LengthFreq)])*(Data$LookUpTab$c4Tab[match(Data$SgpSummary$SgpSize,Data$LookUpTab$LengthFreq)])*mean(Data$SgpSummary$AjSgpStd,na.rm=T,trim=0.025))| Data$SgpSummary$AjSgpStd<((Data$LookUpTab$b3Tab[match(Data$SgpSummary$SgpSize,Data$LookUpTab$LengthFreq)])*(Data$LookUpTab$c4Tab[match(Data$SgpSummary$SgpSize,Data$LookUpTab$LengthFreq)])*mean(Data$SgpSummary$AjSgpStd,na.rm=T,trim=0.025)))
  p=0.0027
  PoissonUpperLimit=p*length(Data$SgpSummary$SgpSize)+3*sqrt(p*length(Data$SgpSummary$SgpSize))
  FalseCountDiff=FirstFalseCount=sum(Data$SgpSummary$Decision, na.rm=TRUE)
  while (FalseCountDiff> PoissonUpperLimit){
    Data$SgpSummary[Data$SgpSummary$Decision==FALSE,][,"Decision"]<-(( Data$SgpSummary[Data$SgpSummary$Decision==FALSE,]$AjSgpStd>((Data$LookUpTab$b4Tab[match(Data$SgpSummary[Data$SgpSummary$Decision==FALSE,]$SgpSize,Data$LookUpTab$LengthFreq)])*(Data$LookUpTab$c4Tab[match(Data$SgpSummary[Data$SgpSummary$Decision==FALSE,]$SgpSize,Data$LookUpTab$LengthFreq)])* mean(Data$SgpSummary[Data$SgpSummary$Decision==FALSE,]$AjSgpStd,na.rm=T,trim=0.025))| Data$SgpSummary[Data$SgpSummary$Decision==FALSE,]$AjSgpStd<((Data$LookUpTab$b3Tab[match(Data$SgpSummary[Data$SgpSummary$Decision==FALSE,]$SgpSize,Data$LookUpTab$LengthFreq)])*(Data$LookUpTab$c4Tab[match(Data$SgpSummary[Data$SgpSummary$Decision==FALSE,]$SgpSize,Data$LookUpTab$LengthFreq)])* mean(Data$SgpSummary[Data$SgpSummary$Decision==FALSE,]$AjSgpStd,na.rm=T,trim=0.025))))
    FalseCount=sum(Data$SgpSummary$Decision, na.rm=TRUE)
    FalseCountDiff=FalseCount-FirstFalseCount
    FirstFalseCount=FalseCount
  }

  #CommonCause= Data$SgpSummary[Data$SgpSummary$Decision==FALSE,]
  #SpecialCause=Data$SgpSummary[Data$SgpSummary$Decision==TRUE,]

  SbarC= mean(Data$SgpSummary[Data$SgpSummary$Decision==FALSE,]$AjSgpStd,na.rm=T,trim=0.025)
  XbarC= mean(Data$SgpSummary[Data$SgpSummary$Decision==FALSE,]$SgpMean,na.rm=T)
  }

  PhaseIIParam=data.frame(Xbar=XbarC,Sbar=SbarC)


  Partition=list(SgpSummary=Data$SgpSummary,LookUpTab=Data$LookUpTab,PhaseIIParam=PhaseIIParam)


  class(Partition)="Partition.SgpSummary"



  return(Partition)
}





