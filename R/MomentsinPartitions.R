#' The first four moments (mean, standard deviation, skewness, kurtosis) in total, common and special cause periods.
#'
#' This function gives the first four moments in total, common and special cause periods. Partitioning of common and special cause periods is based on the Shewhart S chart.
#'
#' @param PartitonedData  common and special cause partitions
#' @param Subgroups a list of subgroups used for partitioning
#' @return Mean, standard deviation, skewness and kurtosis  in total, common and special caused periods
#' @author Nadeeka Premarathna
#' @references Nadeeka Premarathna,  A. Jonathan R. Godfrey and K. Govindaraju. "Decomposition of stock market trade-offs using Shewhart methodology." International Journal of Quality & Reliability Management 33, no. 9 (2016): 1311-1331.
#' @examples
#' # subgroup size
#' SubgroupCriteria="weeks"
#' # data loading
#' data(AAPL)
#' #subgrouping
#' require(zoo)
#' StockPrice=window(AAPL[,"Close"],start=as.Date("2012-01-02"),end=as.Date("2014-12-31"))
#' Subgroups=Subgrouping(StockPrice,SubgroupCriteria,CountSgps=1)
#' # obtain subgroup summary
#' DataSum=SubgroupSummary(Subgroups,MaxSgpSize=5)
#' # partitioning data into common and special cause periods
#' PartitonedData=PartitioningSgps(DataSum,ChartType="S")
#' # calculate the first four moments
#' MomentsInPartitions(PartitonedData$SgpSummary,Subgroups)
#'
#' @export

MomentsInPartitions<-function(PartitonedData,Subgroups)
{

CC=PartitonedData[PartitonedData$Decision==FALSE,]
SC=PartitonedData[PartitonedData$Decision==TRUE,]



SC.Sd.dev=sd(unlist(Subgroups[SC$SgpIndex]))
SC.Mean=mean(unlist(Subgroups[SC$SgpIndex]))
SC.Skew=moments::skewness(unlist(Subgroups[SC$SgpIndex]))
SC.Kurt=moments::kurtosis(unlist(Subgroups[SC$SgpIndex]))


CC.Sd.dev=sd(unlist(Subgroups[CC$SgpIndex]))
CC.Mean=mean(unlist(Subgroups[CC$SgpIndex]))
CC.Skew=moments::skewness(unlist(Subgroups[CC$SgpIndex]))
CC.Kurt=moments::kurtosis(unlist(Subgroups[CC$SgpIndex]))

TT.Sd.dev=sd(c(unlist(Subgroups[CC$SgpIndex]),unlist(Subgroups[SC$SgpIndex])))
TT.Mean=mean(c(unlist(Subgroups[CC$SgpIndex]),unlist(Subgroups[SC$SgpIndex])))
TT.Skew=moments::skewness(c(unlist(Subgroups[CC$SgpIndex]),unlist(Subgroups[SC$SgpIndex])))
TT.Kurt=moments::kurtosis(c(unlist(Subgroups[CC$SgpIndex]),unlist(Subgroups[SC$SgpIndex])))

Means=c(TT.Mean,CC.Mean,SC.Mean)
Std=c(TT.Sd.dev,CC.Sd.dev,SC.Sd.dev)
Skews=c(TT.Skew,CC.Skew,SC.Skew)
Kurts=c(TT.Kurt,CC.Kurt,SC.Kurt)
x=data.frame(Means,Std,Skews,Kurts)
colnames(x)<-c("Mean","Std.dev","Skewness","Kurtosis")
rownames(x)<-c("Total","Common","Special")
return(x)
}
