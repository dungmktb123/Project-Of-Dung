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

LargInter_3P <- function(xi, Fi,x)
{
  N = length(xi)
  n = max(which(x>=(xi-1e-9)))
  if(n <= N-2){
    return(LargInter(xi[n:(n+2)],Fi[n:(n+2),x]))
  }
  else
  {
    return(LargInter(xi[(N-2):N], Fi[(N-2):N],x))
  }
  
  return(LargInter(Xi[n:(n+2)]),Fi[n:(n+2)],x)
}
xx <- seq(1,5,length.out=100)
for(k in 1:length(xx))
{
  yy[k] <- LargInter_3P(xi,yi,xx[k])
}
lines(xx,yy)
#xi <- 4:7
#Fi <- c(-0.0660433280, -0.3275791376, -0.276683581, -0.0046828235)
#plot(xi,Fi, col='red')
#x <- seq(min(xi), max(xi), by=0.1)
#y <- LargInter(xi,Fi,x)
#lines(x,y)
MultiLargInter_3p <- function(xi,Fi,x)
{
  N <- length(xi)
  il <- max(which(x >=(xi - .Machine$double.eps)))
  ir <- min(which(x <= (xi - .Machine$double.eps)))
  if(il==1)
  {
    return(LargInter(xi[1:3],Fi[1:3],x))
  }
  else if (ir==N)
  {
    return(LargInter(xi[(N-2):N],Fi[(N-2):N],x))
  }
  else{
    if(abs(x-il) >abs(x-ir)) {center <-il} else {center <-ir}
    return(LargInter(xi[(center-1):(center+1)], Fi[(center-1):(center+1)],x))
  }
}