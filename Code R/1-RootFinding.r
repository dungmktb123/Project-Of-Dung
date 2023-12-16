# 1. Bisection Method
# 1.1. Absolute error (= abs(b-a))
myBisect1 <- function(f, a, b, tol=1e6 , m = 100) #tol: acceptable error
{
  iter <- 0
  f.a <- f(a)
  f.b <- f(b)
  while(abs(b-a) > tol) {
    iter <- iter + 1
    if (iter > m) {
      warning("(Bisect) No solution found after", m, "loops")
      break
    }
    xmid <- (a + b)/2 #c: mid point
    ymid <- f(xmid)
    if (f.a*ymid > 0) {
      a <- xmid
      f.a <- ymid
    }
    else {
      b <- xmid
      f.b <- ymid
    }
  }
  
  root <- (a + b)/2
  sentence <- paste("iter = ", iter)
  print(sentence)
  return(root)
}
# 1.2. Relative error (= abs(b-a))
myBisect2 <- function(f, a, b, tol = 1e6, m = 100) #tol: acceptable error
{
  iter <- 0
  f.a <- f(a)
  f.b <- f(b)
  while(abs(b-a)/max(abs(c(a,b))) > tol) {
    iter <- iter + 1
    if (iter > m) {
      warning("(Bisect) No solution found after", m, "loops")
      break
    }
    c = (a + b)/2 #c: mid point
    f.c <- f(c)
    if (f.a*f(c) > 0) {
      a <- c
      f.a <- f.c
    }
    else {
      b <- c
      f.b <- f.c
    }
  }
  root <- (a + b)/2
  print(iter)
  return(root)
}
#1.3. Test
myfun <- function(x){return(cos(x)-x)}
myBisect1(myfun, 0, 1, 1e-12)
myBisect2(myfun, 0, 1, 1e-12)
#1.4. Draw Plot
xi <- seq(0,1,by=0.1)
yi <- myfun(xi)
plot(xi, yi, type="l") #type="l" - line plot

#2. Newton-Raphson Method
#2.1. C1
myNewton1 <- function(f, fp, x, tol, m) # fp: derivative function
{
  iter <- 0
  for(i in 1:m) {
    iter <- iter + 1
    i = x - (f(x)/fp(x))
    x = i
  }
  if(iter > m)
    stop("(Newton) No solution found after", m, "loops")
  print(iter)
  return(x)
}
#2.2. C2
myNewton2 <- function(f, fp, x, tol, m) # fp: derivative function
{
  iter <- 0
  err <- 10*tol
  while(err > tol) {
    iter <- iter + 1
    xnew <- x - f(x)/fp(x)
    if (iter>m)
      stop("(Newton) No solution found after", m, "loops")
    err <- abs(xnew - x)
    x <- xnew
  }
  print(iter)
  return(x)
}
#2.3. Test
myfund <- function(x){return(-sin(x)-1)}
myNewton1(myfun, myfund, 1, 1e-6, 10)
myNewton2(myfun, myfund, 1, 1e-6, 10)

#3. Secant Method
mySecant <- function(f, x0, x1, tol, m=100)
{
  iter <- 0
  while(abs(x1-x0) > tol){
    iter <- iter + 1
    f.x0 <- f(x0)
    f.x1 <- f(x1)
    xi <- x1 - (f.x0 / ((f.x1 - f.x0)/(x1 - x0)))
    x0 <- x1
    x1 <- xi
    if (iter>m)
      stop("(Secant) No solution found after", m, "loops")
  }
  print(iter)
  return(xi)
}
mySecant(myfun, 0, 1, 1e-12)

mySecant2 <- function(f, a,b, tol, m=100)
{
  iter <- 0
  f.a <- f(a)
  f.b <- f(b)
  while(abs(b-a) > tol) {
    iter <- iter + 1
    if (iter > m) {
      warning("(Bisect) No solution found after", m, "loops")
      break
    }
    xmid <- (a*f.b - b*f.a)/(f.b-f.a) #c: mid point
    ymid <- f(xmid)
    if (f.a*ymid > 0) {
      a <- xmid
      f.a <- ymid
    }
    else {
      b <- xmid
      f.b <- ymid
    }
  }
  
  root <- (a*f.b - b*f.a)/(f.b-f.a)
  sentence <- paste("iter = ", iter)
  print(sentence)
  return(root)
}

#3.1. Test
mySecant(myfun, 0, 1, 1e-12)
mySecant2(myfun, 0, 1, 1e-12)

#4. Homework
#4.1. Find the root: f=sin(x)-x/2 in [pi/2, pi]
myfun2 <- function(x){return (sin(x)-x/2)}
myfund2 <- function(x){return (cos(x)-1/2)}
myBisect1(myfun2, pi/2, pi, 1e-12)
myNewton2(myfun2, myfund2, 3*pi/4, 1e-12, 100)
mySecant(myfun2, pi/2, pi, 1e-12)
#4.2. Write a function combining 3 methods
myCombine <- function(...)
{
  args <- list(...)
  if (length(args) == 4) {
    bisect_result = myBisect1(args[[1]], args[[2]], args[[3]], args[[4]])
    secant_result = mySecant(args[[1]], args[[2]], args[[3]], args[[4]])
    print(bisect_result)
    print(secant_result)
  } 
  else if (method == 5) {
    return(myNewton2(args[[1]], args[[2]], args[[3]], args[[4]], args[[5]]))
  }
  else {
    stop("Invalid method")
  }
}
result_bisection <- myCombine(myfun, 0, 1, 1e-6)
print(result_bisection)
result_secant <- myCombine(myfun, initial_guess, 'secant')

myfun3 <- function(x){return (x*x*x - 3*x*x +2)}
mySecant(myfun3, -2, 0, 1e-6)
myBisect1(myfun3, -2, 0, 1e-6)
x <- seq(0,1,by=0.1)
y <- myfun3(xi)
plot(xi, yi) #type="l" - line plot

J1i <- c(-0.0660433280,-0.3275791376,-0.2766838581,-0.0046828235)
J2i <- c(0.3641281459,0.0465651163,-0.2428732100,-0.3014172201)
dJ1i <- c()
for (i in 1:length(xi)){
  dJ1i[i] = ( J0i[i]-J2i[i] )/2
}