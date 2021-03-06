% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TotalCommonCauseSigma.R
\name{TotalandCommonCauseSigma}
\alias{TotalandCommonCauseSigma}
\title{Standard deviation in total and common cause periods}
\usage{
TotalandCommonCauseSigma(Data)
}
\arguments{
\item{Data}{subgroup summary (object type "SgpHist")}
}
\value{
Standard deviation in total and common cause periods
}
\description{
Function calculates the standard deviation in total and common cause periods
}
\examples{
SubgroupCriteria="weeks"
data(AAPL)
require(zoo)
StockPrice=window(AAPL[,"Close"],start=as.Date("2012-01-02"),end=as.Date("2014-12-31"))
Subgroups=Subgrouping(StockPrice,SubgroupCriteria,CountSgps=1)
DataSum=SubgroupSummary(Subgroups,MaxSgpSize=5)
# partitioning data into common and special cause periods
PartitonedData=PartitioningSgps(DataSum,ChartType="S")
TCCSigmaVal=TotalandCommonCauseSigma(PartitonedData)
}
\references{
K. Govindaraju and A. Jonathan R. Godfrey. " Analysis of stock market volatility using Shewhart methodology." Total Quality Managment & Business Excellence 22 no. 4 (2011): 425-432.
}
\author{
Nadeeka Premarathna
}
