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
MCInt3D <- function(fun,a,b,c,N)
{
  x <- runif(N, min=a[1],max=a[2])
  y <- runif(N, min=b[1],max=b[2])
  z <- runif(N, min = c[1],max=c[2])
  
  Averfun <- sum(fun(x,y,z))/N
  vol <- (a[2]-a[1])*(b[2] - b[1]) * (c[2]-c[1])
  Integ <- vol*Averfun
  
  Averfun2 <- sum(fun(x,y,z)^2)/N
  Sig <- sqrt((Averfun2 - Averfun^2)/(N-1))
  print(c(Integ,Sig))
  
  return(Integ)
}

f <- function(x,y,z)
{
  return(exp(-x^2-y^2-z^2))
}

MCInt_nD <- function(fun,bounds,N)
{
  n <-dim(bounds)[1]
  x <- data.frame(matrix(NA, nrow=n,ncol=N))
  
  for(j in 1:n){
    x[j,] <- runif(N,min=bounds[j,1],max=bounds[j,2])
  }
  Averfun <- 0
  for(k in 1:N){
    Averfun <- Averfun + fun(x[,k])
  }
  Averfun <- Averfun/N
  vol <- 1
  for(d in 1:n)
  {
    vol <- vol*bounds[d,2] - bounds[d,1]
  }
  Integ <- vol*Averfun
  return(Integ)
}
f_nD <- function(x)
{
  return(exp(-sum(x*2)))
}
