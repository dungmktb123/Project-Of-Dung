g <- 9.8
alpha <- 45/180*pi
v0 <- 10
z0 <- 100
k <- 0.1 # hệ số ma sát
f <- function(t, y, dy){return(-g - k*dy)} # hàm tính lực ma sát
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
t_ground <- myBisect(FreeFall,1,6)

yL <- function(alpha){
  dz0 <- v0*sin(alpha)
  out <- MyEuler3_2ndOrder(f, t0, tN, z0, dz0)
  FreeFall <- function(t){return(LargInter(out$x, out$y, t))}
  t_g <- myBisect(FreeFall,1,6)
  yL <- v0*cos(alpha)*t_g - k*t_g^2/2 # tính khoảng cách bay xa nhất khi có ma sát
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

alpha_max <- myBisect(dyL,0,1,tol = 1e-9)
print(alpha_max/pi*180) # góc ném tối ưu khi có ma sát
