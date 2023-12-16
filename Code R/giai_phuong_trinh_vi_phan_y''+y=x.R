MyEuler3_2ndOrder <- function(fun,x0,xN,y0,dy0,dx = (xN-x0)/10)
{
  xi <- c()
  yi <- c()
  dyi <- c()
  i <- 1
  xi[1] <- x0
  yi[1] <- y0
  dyi[1] <- dy0
  while(xi[i] < xN)
  {
    xi[i+1] <- xi[i] + dx
    dyi[i+1] <- dyi[1] + fun(xi[i],yi[i],dyi[i])*dx
    yi[i+1] <- yi[i] + (dyi[i] + dyi[i+1])/2*dx
    dyi[i+1] <- dyi[i] + (fun(xi[i],yi[i],dyi[i])+fun(xi[i+1],yi[i+1],dyi[i+1]))/2*dx
    yi[i+1] <- yi[i] + (dyi[i] + dyi[i+1])/2*dx
    i <- i+1
  }
  return(data.frame(x=xi,y=yi,dy=dyi))
}
fun <- function(x,y,dy) 
{
  return(x-y)
}
x0 <- 0
xN <- 4*pi
y0 <- 1
dy0 <- 2
out<- MyEuler3_2ndOrder(fun,x0,xN,y0,dy0,dx=0.2)
plot(out$x,out$y)
lines(out$x,sin(out$x)+cos(out$x)+out$x)
plot(out$x,out$dy)
lines(out$x, cos(out$x)-sin(out$x)+1)
