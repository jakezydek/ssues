#' @title G(r): small sample correlation coefficient
#' @param 2 numeric vectors
#' @description Calculates an unbiased correlation coefficient for studies with small sample sizes, from Olkin and Pratt 1958
#' @references Olkin, I. and J. W. Pratt, 1958, Unbiased estimation of certain correlation coefficients, The Annals of Mathematical Statistics, 29(1): 201-211.


Gofr<-function(x,y){
  r<-cor(x,y)
  r2<-r^2
  n<-length(x)+length(y)
  integrand<-function(z){
    ((z^-.5)*((1-z)^(((n-2)/2)-1))/((1-z*(1-r2))^.5))
  }
  s<-integrate(integrand,0,1)
  gam1<-gamma((n-1)/2)
  gam2<-gamma(.5)
  gam3<-gamma((n-2)/2)
  gam4<-gam1/(gam2*gam3)
  G.r<-r*gam4*s[[1]]
  return(G.r)
}