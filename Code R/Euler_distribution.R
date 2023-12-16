MyEuler <- function(f,x0,xN,y0,dx=(xN-x0)/10)
{
  xi <- c()
  yi <- c()
  i <- 1
  yi[1] <- y0
  xi[1] <- x0
  while(xi[i] < xN)
  {
    xi[i+1] <- xi[i] + dx
    yi[i+1] <- yi[i] + f(xi[i],yi[i])*dx
    i <- i+1
  }
  return(data.frame(x=xi,y=yi))
}

fun <- function(x,y) 
{
  return(x^2)
}
x0 <- 0
xN <- 5
y0 <- 1
out<- MyEuler(fun,x0,xN,y0)
plot(out$x,out$y)
lines(out$x,out$x^3/3+1)

MyEulerEnd <- function(f,x0,xN,y0,dx=(xN-x0)/10)
{
  xi <- c()
  yi <- c()
  i <- 1
  yi[1] <- y0
  xi[1] <- x0
  while(xi[i] < xN)
  {
    xi[i+1] <- xi[i] + dx
    yi[i+1] <- yi[i] + f(xi[i],yi[i])*dx
    yi[i+1] <- yi[i] + f(xi[i+1],yi[i+1])*dx # sử dụng đạo hàm tại điểm cuối
    i <- i+1
  }
  return(data.frame(x=xi,y=yi))
}

fun <- function(x,y) 
{
  return(x^2)
}
x0 <- 0
xN <- 5
y0 <- 1
out<- MyEulerEnd(fun,x0,xN,y0)
plot(out$x,out$y)
lines(out$x,out$x^3/3+1)

MyEulerTwoPoint <- function(f,x0,xN,y0,dx=(xN-x0)/10)
{
  xi <- c()
  yi <- c()
  i <- 1
  yi[1] <- y0
  xi[1] <- x0
  while(xi[i] < xN)
  {
    xi[i+1] <- xi[i] + dx
    yi[i+1] <- yi[i] + f(xi[i],yi[i])*dx
    yi[i+1] <- yi[i] + f(xi[i+1],yi[i+1])*dx
    yi[i+1] <- yi[i] + f((xi[i]+xi[i+1])/2,(yi[i]+yi[i+1])/2)*dx # sử dụng đạo hàm tại điểm cuối
    i <- i+1
  }
  return(data.frame(x=xi,y=yi))
}

fun <- function(x,y) 
{
  return(x^2)
}
x0 <- 0
xN <- 5
y0 <- 1
out<- MyEulerTwoPoint(fun,x0,xN,y0)
plot(out$x,out$y)
lines(out$x,out$x^3/3+1)
data.frame(out, Sol = exp(out$x^2/2), Err = exp(out$x^2/2) - out$y, ErrRel = (exp(out$x^2/2) - out$y)/exp(out$x^2/2))


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
MyEulerImpr <- function(f, x0, xN, y0, dx = (xN-x0)/10)
{
  xi <- c()
  yi <- c()
  i <- 1
  xi[1] <- x0
  yi[1] <- y0
  while (xi[i] < xN){
    xi[i+1] <- xi[i] + dx
    f_start <- f(xi[i], yi[i])
    yi[i+1] <- yi[i] + f_start*dx
    f_end <- f(xi[i+1], yi[i+1])
    yi[i+1] <- yi[i] + (f_start + f_end)/2*dx
    f_err <- 1
    # lặp lại bước tính đạo hàm tại điểm cuối nhiều lần
    # cho đến khi sai số giữa đạo hàm tại điểm cuối mới và cũ < giá trị nào đó
    # i là số lần lặp đến khi hội tụ
    while (f_err > 1e-9){
      f_end_2 <- f(xi[i+1], yi[i+1])
      yi[i+1] <- yi[i] + (f_start + f_end_2)/2*dx
      f_err <- abs(f_end_2 - f_end)
      f_end <- f_end_2
      print(c(i, f_err))
    }
    i <- i+1
  }
  return(data.frame(x=xi, y=yi))
}
fun <- function(x,y) 
{
  return(x^2)
}
x0 <- 0
xN <- 5
y0 <- 1
out<- MyEulerTwoPoint(fun,x0,xN,y0)
plot(out$x,out$y)
lines(out$x,out$x^3/3+1)
data.frame(out, Sol = exp(out$x^2/2), Err = exp(out$x^2/2) - out$y, ErrRel = (exp(out$x^2/2) - out$y)/exp(out$x^2/2))
