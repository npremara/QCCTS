QCCTS.TrimMean<- function(x)
{
  x <- sort(zoo::coredata(x))
  a=ceiling(length(x)/4)
  return((x[a]+2*median(x)+ x[length(x)-a+1])/4)
}
