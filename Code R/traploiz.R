#TrapeInt <- function(fun, a, b)
#{
#  return((b-a)/2*(fun(a)+fun(b)))
#}

MultTrapeInt <- function(fun, a, b, tol)
{
  N <- 1
  h <- b - a
  Integ1 <- h/2*(fun(a)+fun(b))
  
  N <- 2*N
  h <- (b-a)/2
  xj <- (a + h)
  Integ2 <- Integ1/2 + h*fun(xj)
  Err <- abs(Integ2 - Integ1)
  
  while (Err > tol)
  {
    N <- 2*N
    h <- h/2
    Integ1 <- Integ2
    Integ2 <- Integ1/2
    for (j in seq(1, N-1, by=2)){
      xj <- a + j*h
      Integ2 <- Integ2 + h*fun(xj)
     
    }
    Err <- abs(Integ2 - Integ1)
    print(N)
  }
  return(Integ2)
}
