g <- 9.8
alpha <- 45/180*pi
v0 <- 10
z0 <- .Machine$double.eps
f <- function(t, y, dy){return(-g)}
t0 <- 0
tN <- 6
dz0 <- v0*sin(alpha)
out <- MyEuler3_2ndOrder(f, t0, tN, z0, dz0)
plot(out$x,out$y, main = "z(t)")
lines(out$x, z0 + dz0*out$x -g*out$x^2/2)

plot(out$x,out$dy, main = "dz(t)")
lines(out$x, dz0 - g*out$x)

ti <- out$x
zi <- out$y
FreeFall <- function(t){return(LargInter(ti, zi, t))}
t_ground <- myBisect(FreeFall,0,2)

yL <- function(alpha){
  dz0 <- v0*sin(alpha)
  out <- MyEuler3_2ndOrder(f, t0, tN, z0, dz0)
  FreeFall <- function(t){return(LargInter(out$x, out$y, t))}
  t_g <- myBisect(FreeFall,0,6,tol=1e-15)
  yL <- v0*cos(alpha)*t_g
  return(yL)
}

ai <- seq(0,pi/2, length.out = 50)
yi <- c()
for (k in 1:length(ai))
{
  yi[k] <- yL(ai[k])
}
plot(ai/pi*180,yi, type = 'o')

dyL <- function(alpha){
  da <- 1e-6
  dyL <- (yL(alpha+da)-yL(alpha-da))/(2*da)
  return(dyL)
}

alpha_max <- myBisect(dyL,0.6,0.8,tol = 1e-15)
print(alpha_max/pi*180)
