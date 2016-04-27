#' @title RRdelta
#' @description Calculates the unbiased response ratio and associated variance between two groups of a controlled trial using the delta method (Lajeunesse 2015).
#' @param 2 vectors
#' @references Lajeunesse, M J.  2015.  Bias and correction for the log response ratio in ecological meta-analysis.  Ecology, 96(8): 2056-2063  
#' @examples RRdelta(sleep[sleep$group==1,1],sleep[sleep$group==2,1])


RRdelta<-function(x,y){
  treatment<-x
  control<-y
  Xt<-mean(treatment)
  Xc<-mean(control)
  SDt<-sd(treatment)
  SDc<-sd(control)
  Nt<-length(treatment)
  Nc<-length(control)
  RR<-log(Xt/Xc)
  var.RR<-((SDt^2)/(Nt*(Xt^2)))+((SDc^2)/(Nc*(Xc^2)))
  RR.delta<-RR+(.5*((SDt^2)/(Nt*(Xt^2)))-((SDc^2)/(Nc*(Xc^2))))
  varRR.delta<-var.RR+(.5*((SDt^4)/((Nt^2)*(Xt^4)))+((SDc^4)/((Nc^2)*(Xc^4))))
  RRs<-list(RR.delta,varRR.delta)
  return(RRs)
}
