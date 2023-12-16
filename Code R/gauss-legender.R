GaussLegendreInt <- function(fun,a,b){
  h <- (b-a)
  coe <- 1/sqrt(3)
  #x = a + (b-a)/2*(y+1) = (b+a)/2+(b-a)/2*y
  p1 <- (b+a)/2 + (b-a)/2*(-coe)
  p2 <- (b+a)/2 + (b-a)/2*(coe)
  Integ <- h/2*(fun(p1) + fun(p2))
  
  return(Integ)
}

MultGausslegendreInt <- function(fun,a,b,tol)
{
  N <- 1
  Integ1 <- GaussLegendreInt(fun,a,b)
  print(Integ1)
  Err <- 2*tol
  while(Err >tol)
  {
    N <- N+1
    h <- (b-a)/N
    Integ2 <- 0
    for(j in 1:N)
    {
      aj <- a+ (j-1)*h
      bj <- a+ j*h
      Integ2 <- Integ2 + GaussLegendreInt(fun,aj,bj)
    }
    Err <- abs(Integ2-Integ1)
    Integ1 <- Integ2
    print(N)
  }
  return(Integ2)
}