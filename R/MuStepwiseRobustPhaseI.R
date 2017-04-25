#' Unbiased robust estimator for \eqn{\mu} based on a stepwise robust chart procedure
#'
#' Estimation procedure combines the use of individual observations and subgroup screening. An initial estimate for \eqn{\mu} is based on the trimmed means of the tri means and screen the subgroups. Then, the resulting estimator for \eqn{\mu} from the remaining subgroups is sued to  screen the individual outliers in the remaining subgroups.
#' @param Subgroups subgroups of the data series, function handles the unequal subgroup sizes.
#' @param sigma estimate for \eqn{\sigma}
#'
#' @return  estimates of \eqn{\mu} from Phase I data
#' @author Nadeeka Premarathna
#' @references Nazir, Hafiz Z., Marit Schoonhoven, Muhammad Riaz, and Ronald JMM Does. "Quality Quandaries: A Stepwise Approach for Setting Up a Robust Shewhart Location Control Chart." Quality Engineering 26, no. 2 (2014): 246-252.
#' @examples
#' data(MeltIndex)
#' require(xts)
#' Subgroups=split(MeltIndex, f="weeks")
#' Subgroups[20]=NULL
#' sigma=SigmaStepwiseRobustPhaseI(Subgroups)
#' Subgroups=split(MeltIndex, f="weeks")
#' Subgroups[20]=NULL
#' MuStepwiseRobustPhaseI(Subgroups,sigma)

#' @export


MuStepwiseRobustPhaseI<-function(Subgroups,sigma){


  SgpSize<-sapply(Subgroups, length)
  SubgroupsMeanTrim=sapply(Subgroups,QCCTS.TrimMean)
  IQR10M=QCCTS.IQR10Me(SubgroupsMeanTrim)

  UCL1=IQR10M+3*sigma/sqrt(SgpSize)
  LCL1=IQR10M-3*sigma/sqrt(SgpSize)

  Subgroups[which(SubgroupsMeanTrim> UCL1 | SubgroupsMeanTrim< LCL1)] <- NULL

  SubgroupsMeanTrim=sapply(Subgroups,QCCTS.TrimMean)

  TMnew=mean(SubgroupsMeanTrim)

  UCLInd=TMnew+3*sigma
  LCLInd=TMnew-3*sigma


  for(k in 1:length(Subgroups)){
    Subgroups[[k]][which(Subgroups[[k]][,1]> UCLInd | Subgroups[[k]][,1]< LCLInd),1]<-NA
    Subgroups[[k]]=na.omit(Subgroups[[k]])

  }

  NewMeanSubgroup=sapply(Subgroups,mean)

  MeanFPhaseII=mean(NewMeanSubgroup)
  return(MuPhaseI=MeanFPhaseII)
}

