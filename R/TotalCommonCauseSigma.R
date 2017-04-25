#' Standard deviation in total and common cause periods
#'
#' Function calculates the standard deviation in total and common cause periods
#'
#' @param Data subgroup summary (object type "SgpHist")
#' @return Standard deviation in total and common cause periods
#' @author Nadeeka Premarathna
#' @references K. Govindaraju and A. Jonathan R. Godfrey. " Analysis of stock market volatility using Shewhart methodology." Total Quality Managment & Business Excellence 22 no. 4 (2011): 425-432.
#' @examples
#' SubgroupCriteria="weeks"
#' data(AAPL)
#' require(zoo)
#' StockPrice=window(AAPL[,"Close"],start=as.Date("2012-01-02"),end=as.Date("2014-12-31"))
#' Subgroups=Subgrouping(StockPrice,SubgroupCriteria,CountSgps=1)
#' DataSum=SubgroupSummary(Subgroups,MaxSgpSize=5)
#' # partitioning data into common and special cause periods
#' PartitonedData=PartitioningSgps(DataSum,ChartType="S")
#' TCCSigmaVal=TotalandCommonCauseSigma(PartitonedData)
#' @export
TotalandCommonCauseSigma<-function(Data)
{
  if(!is.Partition.SgpSummary(Data))
    stop("'Data' must be a 'Partition.SgpSummary' object")

  SbarC= mean(Data$SgpSummary[Data$SgpSummary$Decision==FALSE,]$AjSgpStd,na.rm=T,trim=0.025)
  SbarT= mean(Data$SgpSummary$AjSgpStd,na.rm=T,trim=0.025)
  Sbar=list(SbarT=SbarT,SbarC=SbarC)
  return(Sbar)
}


