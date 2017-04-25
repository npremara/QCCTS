QCCTS.IQR10Me<-function(x)
{
  x<-sort(x)
  v=ceiling((length(x)/10)+1):(length(x)-ceiling(length(x)/10))
  return(sum(x[v])/(length(x)-2*(ceiling(length(x)/10))))
}
