#' @title RRsigma
#' @description Calculates the unbiased response ratio and associated variance between two groups of a controlled trial, using the sigma method (Lajeunesse 2015).
#' @param 2 vectors
#' @references Lajeunesse, M J.  2015.  Bias and correction for the log response ratio in ecological meta-analysis.  Ecology, 96(8): 2056-2063  
#' @examples RRsigma(sleep[sleep$group==1,1],sleep[sleep$group==2,1])


RRsigma<-function(x,y){
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
  RR.sigma<-.5*log(((Xt^2)+((Nt^-1)*SDt^2))/((Xc^2)+((Nc^-1)*SDc^2)))
  varRR.sigma<-(2*var.RR)-(log((1+var.RR)+(((SDt^2)*(SDc^2))/(Nt*Nc*(Xt^2)*(Xc^2)))))
  RRssig<-list(RR.sigma,varRR.sigma)
  return(RRssig)
}
