#' Divide into subgroups
#'
#' Function divides the data series into subgroups according to the given criteria.
#'
#' @param Data a series of time series data (data type: zoo or xts)
#' @param SubgroupCriteria  length of the subgroups  based on time, eg. "weeks", "months",  if using daliy data
#' @param CountSgps subgroups sizes further increase using CountSgps=2,3,4 eg. SubgroupCriteria="weeks", CountSgps=2 gives subgroups size of two weeks
#' @return Subgropus
#' @author Nadeeka Premarathna
#' @examples
#' SubgroupCriteria="weeks"
#' data(AAPL)
#' require(zoo)
#' StockPrice=window(AAPL[,"Close"],start=as.Date("2012-01-02"),end=as.Date("2014-12-31"))
#' Subgroups=Subgrouping(StockPrice,SubgroupCriteria,CountSgps=1)
#' @export
#' @seealso \code{\link{PartitioningSgps}}

Subgrouping<-function(Data,SubgroupCriteria,CountSgps)
{

  if (missing(Data))
    stop("'data' argument is not specified")
  if(!zoo::is.zoo(Data) && !xts::is.xts(Data))
    stop("'Data' must be a zoo or xts object")
  if(missing(SubgroupCriteria))
    stop("'SubgroupCriteria' argument is not specified")

  Returns<-diff((log(Data)))
  Returns.xts<-xts::as.xts(Returns)
  Subgroups<-split(Returns.xts, f=SubgroupCriteria, k=CountSgps)
  return(Subgroups)

}
