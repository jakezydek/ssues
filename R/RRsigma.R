#' @title RRsigma
#' @description Calculates the unbiased response ratio and variance for the effect of a treatment in a controlled trial with two groups using the sigma method (Lajeunesse 2015).
#' @param 2 vectors
#' @references Lajeunesse, M J.  2015.  Bias and correction for the log response ratio in ecological meta-analysis.  Ecology, 96(8): 2056-2063  

RRsigma<-function(x,y){
  treatment<-x
  control<-y
  Mt<-mean(treatment)
  Mc<-mean(control)
  SDt<-sd(treatment)
  SDc<-sd(control)
  Nt<-length(treatment)
  Nc<-length(control)
  RR<-ln(Mt/Mc)
  varRR<-((SDt^2)/(Nt*(Xt^2)))+((SDc^2)/(Nc*(Xc^2)))
  RRsigma<-.5*log(((Xt^2)+((Nt^-1)*SDt^2))/((Xc^2)+((Nc^-1)*SDc^2)))
  varRRsigma<-(2*varRR)-(ln((1+varRR)+(((SDt^2)*(SDc^2))/(Nt*Nc*(Mt^2)*(Mc^2)))))
  return(RRsigma)
  return(varRRsigma)
}
