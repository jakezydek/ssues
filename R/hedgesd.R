#' @title Hedges' d
#' @param 2 vectors
#' @description Calculates Hedges' d effect size for studies with small sample sizes.  The two vectors represent observations of treatment and control groups in a controlled trial.
#' @references Hedges, L and I Olkin, 1985. Statistical Methods for Meta-Analysis. Academic Press, Orlando Fl. pp. 81, 104
#' @examples Hedgesd(sleep[sleep$group==1,1],sleep[sleep$group==2,1])

Hedgesd<-function(x,y) {
  treatment<-x
  control<-y
  s.corrected.treatment<-(length(treatment)-1)*sd(treatment) 
  s.corrected.control<-(length(control)-1)*sd(control)
  sample.size.corrected<-length(treatment)+length(control)-2
  s.star<-sqrt((s.corrected.treatment+s.corrected.control)/sample.size.corrected)
  g<-(mean(treatment)-mean(control))/s.star
  a<-length(treatment)+length(control)-2
  j.a<-gamma(a/2)/(sqrt(a/2)*gamma((a-1)/2))
  d<-j.a*g
  return(d)
}