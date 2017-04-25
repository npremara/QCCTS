QCCTS.IQR10<-function(x)
{
  x<-sort(x)
  v=ceiling(length(x)/10):(length(x)-ceiling(length(x)/10)+1)
  return(sum(x[v])/(length(x)-2*(ceiling(length(x)/10)-1)))
}
