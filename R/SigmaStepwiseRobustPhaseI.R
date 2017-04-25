#' Estimating \eqn{\sigma} using a stepwise robust chart procedure
#'
#' Estimation procedure performs individual observations and subgroup screening.  An initial estimate for \eqn{\sigma} is obtained from trimmed means of interquartile ranges. Subgroups are first screened.  Then, the resulting \eqn{\sigma} from the remaining subgroups is used to eliminate individual outliers in the remaining subgroups.
#' @param Subgroups a list subgroups, function handles the unequal subgroup sizes.
#' @return  estimates of \eqn{\sigma} from Phase I data
#' @author Nadeeka Premarathna
#' @references Nazir, Hafiz Z., Marit Schoonhoven, Muhammad Riaz, and Ronald JMM Does. "Quality Quandaries: How to Set Up a Robust Shewhart Control Chart for Dispersion?." Quality Engineering 26, no. 1 (2014): 130-136.
#' @examples
#' data(MeltIndex)
#' require(xts)
#' Subgroups=split(MeltIndex, f="weeks")
#' Subgroups[20]=NULL
#' sigma=SigmaStepwiseRobustPhaseI(Subgroups)
#' @export


SigmaStepwiseRobustPhaseI<-function(Subgroups)
{
  SgpSize<-sapply(Subgroups, length)
  SubgroupsIQRS=sapply(Subgroups,QCCTS.IQRS)
  IQR10M=QCCTS.IQR10(SubgroupsIQRS)

  LengthFreq=as.data.frame.table(table(SgpSize))
  n=3:10
  dIQR10=c(1.644,2.020,0.951,1.253,1.490,1.683,1.122,1.293)
  dIQR=c(1.692,2.060,0.990,1.284,1.514,1.704,1.144,1.312)
  U1=c(2.923,2.525,3.220,2.688,2.403,2.225,2.474,2.281)
  L1=c(0.042,0.108,0.035,0.093,0.154,0.208,0.146,0.198)
  ConstPhaseI=data.frame(n=n,U1=U1,L1=L1,dIQR10=dIQR10,dIQR=dIQR)
  LengthFreq=as.data.frame.table(table(SgpSize))
  ConstPhaseII=ConstPhaseI[ConstPhaseI$n %in% LengthFreq$SgpSize,]
  ConstPhaseIIval=matrix(NA, nrow=length(SgpSize), ncol=4)
  for(k in 1:length(SgpSize)){
    ConstPhaseIIval[k,]=c(ConstPhaseII$U1[match(SgpSize[k],ConstPhaseII$n)],ConstPhaseII$L1[match(SgpSize[k],ConstPhaseII$n)], ConstPhaseII$dIQR10[match(SgpSize[k],ConstPhaseII$n)],ConstPhaseII$dIQR[match(SgpSize[k],ConstPhaseII$n)])
  }
  colnames(ConstPhaseIIval) <- c("U1","L1","dIQR10","dIQR")
  # control limits for subgroups
  UCL1=(ConstPhaseIIval[,"U1"])*(IQR10M/ConstPhaseIIval[,"dIQR10"])
  LCL1=(ConstPhaseIIval[,"L1"])*(IQR10M/ConstPhaseIIval[,"dIQR10"])

  # screen subgroups
  IQRdIQR=SubgroupsIQRS/ConstPhaseIIval[,"dIQR"]
  SubgroupsIQRSN=SubgroupsIQRS[-which(IQRdIQR> UCL1 | IQRdIQR< LCL1)]
  ConstPhaseIIvalN=ConstPhaseIIval[-which(IQRdIQR> UCL1 | IQRdIQR< LCL1),]
  Subgroups[which(IQRdIQR> UCL1 | IQRdIQR< LCL1)] <- NULL

  # control limits for individual observations
  UCLInd=3*mean(SubgroupsIQRSN)/ConstPhaseIIvalN[,"dIQR"]
  LCLInd=-3*mean(SubgroupsIQRSN)/ConstPhaseIIvalN[,"dIQR"]

  # calculating the residuals of individual variables
  SubgroupsMeanTrim=sapply(Subgroups,QCCTS.TrimMean)
  res=Subgroups
  for(k in 1:length(SubgroupsMeanTrim)){
    res[[k]]=res[[k]]-SubgroupsMeanTrim[k]
  }


  SgpSize<-sapply(res, length)
  UCLIndn=rep(UCLInd,SgpSize)
  LCLIndn=rep(LCLInd,SgpSize)
  Lres=unlist(res)
  # remove individual data

  for(k in 1:length(Subgroups)){
    Subgroups[[k]][which(res[[k]][,1]> UCLInd[k] | res[[k]][,1]< LCLInd[k]),1]<-NA
    Subgroups[[k]]=na.omit(Subgroups[[k]])

  }

  SubgroupsSize=sapply(Subgroups,length)

  LengthFreq=as.data.frame.table(table(SubgroupsSize))
  LengthFreq=as.numeric(as.vector(LengthFreq[,"SubgroupsSize"]))
  c4Tab=IQCC::c4(LengthFreq[LengthFreq>1])
  LookUpTab=list(LengthFreq=LengthFreq[LengthFreq>1],c4Tab=c4Tab)


  unval=matrix(NA, nrow=length(SubgroupsSize), ncol=1)
  for(k in 1:length(SubgroupsSize)){
    unval[k]=LookUpTab$c4Tab[match(SubgroupsSize[k],LookUpTab$LengthFreq)]
  }
  SubgroupsSd=sapply(Subgroups,sd)
  # obtain the adjusted standard deviation
  AjSgpStd<-SubgroupsSd/unval

  sigma=mean(AjSgpStd)
  return(sigmaPhaseI=sigma)
}





