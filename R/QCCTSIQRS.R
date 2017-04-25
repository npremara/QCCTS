QCCTS.IQRS<-function(x)
{
  x <- sort(zoo::coredata(x))
  a=ceiling(length(x)/4)
  return(x[length(x)-a+1]-x[a])
}
