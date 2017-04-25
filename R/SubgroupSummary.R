#' Subgroup summary
#'
#' Function gives subgroup size, mean, standard deviation, adjusted standard deviation, skewness, kurtosis and a table of control chart constants for subgroups in the given series.
#'
#' @param Subgroups a series of subgroups
#' @param MaxSgpSize possible maximum size of a subgroup
#' @return  SgpSummary
#' @author Nadeeka Premarathna
#' @seealso \code{\link{Subgrouping}}
#' @examples
#' # Subgroup size
#' SubgroupCriteria="weeks"
#' #  load data
#' data(AAPL)
#' # divide into subgroups
#' StockPrice=window(AAPL[,"Close"],start=as.Date("2012-01-02"),end=as.Date("2014-12-31"))
#' Subgroups=Subgrouping(StockPrice,SubgroupCriteria,CountSgps=1)
#' # obtain the subgroup summary
#' SubgroupSummary(Subgroups,MaxSgpSize=5)
#' @export
#' @seealso \code{\link{Subgrouping}}

SubgroupSummary<-function(Subgroups, MaxSgpSize)
{

  #Create lookup table of control chart constants
  SgpSize<-sapply(Subgroups, length)

  SgpDateComplete=sapply(Subgroups,zoo::index)

  SgpDate=zoo::as.Date(unlist(lapply(SgpDateComplete, `[[`, 1)))

  # LengthFreq=as.data.frame.table(table(SgpSize))
  # LengthFreq=as.numeric(as.vector(LengthFreq[,"SgpSize"]))
  LengthFreq=2:MaxSgpSize
  c4Tab=IQCC::c4(LengthFreq)
  b3Tab=QCCTS.b3(LengthFreq)
  b4Tab=QCCTS.b4(LengthFreq)
  LookUpTab=list(LengthFreq=LengthFreq,c4Tab=c4Tab,b3Tab=b3Tab,b4Tab=b4Tab)

  #Subgroup summaries
  SgpMean<-sapply(Subgroups,mean)
  SgpStd<-sapply(Subgroups,sd)
  AjSgpStd<-(sapply(Subgroups,sd)/ LookUpTab$c4Tab[match(SgpSize,LookUpTab$LengthFreq)])
  SgpSkew<-sapply(Subgroups,moments::skewness)
  SgpKurt<-sapply(Subgroups,moments::kurtosis)

  RangeCal<-function(x) return(diff(range(x)))

  SgpRange<-sapply(Subgroups,RangeCal)


  SgpIndex<-1:length(SgpSize)

  SgpSummary=data.frame(SgpIndex,SgpSize=SgpSize,SgpMean=SgpMean,SgpStd=SgpStd,AjSgpStd=AjSgpStd,SgpSkew=SgpSkew,SgpKurt=SgpKurt,SgpRange=SgpRange,SgpDate=SgpDate)
  results=list(SgpSummary=SgpSummary,LookUpTab=LookUpTab)
  # create a new class of object "SgpSummary"
  class(results)="SgpSummary"

  return(results)
}




QCCTS.b3 <- function(n)
{
  #Control chart constant b3
  b3=1-(3/IQCC::c4(n))*(sqrt(1-IQCC::c4(n)^2))
  return(b3)
}

QCCTS.b4 <- function(n)
{
  #control chart constant b4
  b4=1+(3/IQCC::c4(n))*(sqrt(1-IQCC::c4(n)^2))
  return(b4)
}


