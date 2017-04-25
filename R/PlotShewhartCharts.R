#' Plotting the Shewhart charts for Phase I and Phase II data
#'
#' The function plots the Shewhart charts for subgroups, in Phase I and Phase II.
#'
#' @param Data Phase I partitioned  subgroup summary (object type "Partition.SgpSummary")
#' @param PhaseIIData Phase II subgroup summary (object type "SgpSummary")
#' @param ChartType Chart type "Xbar"- Shewhart X-bar chart, "R"- Shewhart control chart for range, "S"- Shewhart S chart
#' @param plot logical. If TRUE the corresponding Shewhart chart is plotted.
#' @return plot Shewhart charts
#' @author Nadeeka Premarathna
#' @references Nadeeka Premarathna,  A. Jonathan R. Godfrey and K. Govindaraju. "Decomposition of stock market trade-offs using Shewhart methodology." International Journal of Quality & Reliability Management 33, no. 9 (2016): 1311-1331.
#' @examples
#' require(zoo)
#' SubgroupCriteria="weeks"
#' # data loading
#' data(AAPL)
#' #subgrouping Phase I data
#' StockPrice=window(AAPL[,"Close"],start=as.Date("2012-01-02"),end=as.Date("2013-12-31"))
#' Subgroups=Subgrouping(StockPrice,SubgroupCriteria,CountSgps=1)
#' # obtain subgroup summary of Phase I data
#' DataSum=SubgroupSummary(Subgroups,MaxSgpSize=5)
#' # partitioning data into common and special cause periods (Phase I data)
#' PartitonedData=PartitioningSgps(DataSum,ChartType="S")
#'
#' # Phase II data
#' StockPrice=window(AAPL[,"Close"],start=as.Date("2014-01-06"),end=as.Date("2014-05-30"))
#' SubgroupsPhaseII=Subgrouping(StockPrice,SubgroupCriteria,CountSgps=1)
#' PhaseIIData=SubgroupSummary(SubgroupsPhaseII,MaxSgpSize=5)
#' B=ShewhartSgpCharts(PartitonedData, PhaseIIData,ChartType="S",plot=TRUE)
#' @export
ShewhartSgpCharts<- function(Data, PhaseIIData,ChartType,plot=TRUE){

if(plot) plot.QCCTSShewhartSgp(Data, PhaseIIData, ChartType)

return(Data)
}
