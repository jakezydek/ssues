#' @title RRdelta
#' @description Calculates the unbiased response ratio and variance for the effect a treatment in a controlled trial with two groups using the delta method (Lajeunesse 2015).
#' @param 2 vectors
#' @references Lajeunesse, M J.  2015.  Bias and correction for the log response ratio in ecological meta-analysis.  Ecology, 96(8): 2056-2063  

RRdelta<-function(x,y){
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
  RRdelta<-RR+(.5*((SDt^2)/(Nt*(Xt^2)))-((SDc^2)/(Nc*(Xc^2))))
  varRRdelta<-varRR+(.5*((SDt^4)/((Nt^2)*(Xt^4)))+((SDc^4)/((Nc^2)*(Xc^4))))
  return(RRdelta)
  return(varRRdelta)
}
