LargCoef <- function(xi,x,j)
{
  N <- length(xi)
  Lj <- 1;
  for(k in 1:N)
  {
    if(j !=k)
    {
      Lj <- Lj*(x-xi[k])/(xi[j]-xi[k])
    }
  }
  return(Lj)
}

LargInter <- function(xi, Fi,x)
{
  N <- length(xi)
  P <-0;
  for(j in 1:N)
  {
    P <- P+ LargCoef(xi,x,j)*Fi[j]
  }
  return(P)
}

f <- function(t,y,dy)
{
  return(-9.8)
}
t0 <- 0
tN <- 5
z0 <- 100
dz0 <- 0
out <- MyEuler3_2ndOrder(f,t0,tN,z0,dz0)
plot(out$x, out$y, main="z(t)")
lines(out$x,100-9.8*out$x^2/2)

plot(out$x, out$dy,main="dz(t)")
lines(out$x, -9.8*out$x)

ti <- out$x[9:11]
zi <- out$y[9:11]
FreeFall <- function(t)
{
  return(LargInter(ti,zi,t))
}
t_ground <- myBisect(FreeFall,4,5)
