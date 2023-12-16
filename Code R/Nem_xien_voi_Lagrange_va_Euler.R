g <- 9.8
alpha <- 45*pi/180
v0 <- 10
f <- function(t,y,dy)
{
  return(-g)
}
t0 <- 0
tN <- 6
z0 <- 100
dz0 <- v0*sin(alpha)
out<- MyEuler3_2ndOrder(f,t0,tN,z0,dz0)
plot(out$x,out$y,main="z(t)")
lines(out$x, z0+dz0*out$x-g*out$x^2/2)

plot(out$x,out$dy,main="dz(t)")
lines(out$x,dz0 - g*out$x)

yL <- function(alpha)
{
  a<- alpha*pi/180
  dz0 <- v0*sin(a)
  out<- MyEuler3_2ndOrder(f,t0,tN,z0,dz0)
  FreeFall <- function(t)
  {
    return(LargInter(out$x[9:11],out$y[9:11],t))
  }
  t_g <- myBisect(FreeFall,4,6)
  yL <- dz0 * cos(a) *t_g
  return(yL)
}
ai <- 30:60
yi <- yL(ai)
plot(ai,yi,type='o')

dyL <- function(alpha)
{
  da <- 1e-6
  dyL <- (yL(alpha+da)-yL(alpha-da))/(2*da)
  return(dyL)
}
alpha_max <- myBisect(dyL,40,60)
print(alpha_max*180/pi)
#tính góc alpha với độ ma sát 0.01 sao cho quãng đường đi được xa nhất 
