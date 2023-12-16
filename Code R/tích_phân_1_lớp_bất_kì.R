MC<- function(fun,a,b,N)
{
  x<- runif(N, min=a,max=b)
  Averfun <- sum(fun(x))/N
  Integ <- (b-a)*Averfun
  Averfun2 <- sum(fun(x)^2)/N
  Sig <- sqrt((Averfun2-Averfun^2)/(N-1))
  print(c(Integ,Sig))
  
  #print(sd((b-a)*fun(x)/sqrt(N)),digits=18)
  
  return(Integ)
}
f <- function(x)
{
  return(exp(-x^2))
}
e <- function(y)
{
  return(exp(-1/y^2)/y^2)
}
  