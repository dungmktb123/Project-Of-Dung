myBisect <- function(f , a, b, tol = 0.001, m = 100)
{
  iter <- 0
  f.a <- f(a)
  f.b <- f(b)
  while (abs(b - a) > tol) {
    iter <- iter + 1
    if (iter > m) {
      warning("iterations maximum excerded")
      break
    }
    xmid <- (a + b)/2
    ymid <- f(xmid)
    if (f.a * ymid > 0) {
      a <- xmid
      f.a <- ymid
    }
    else {
      b <- xmid
      f.b <- ymid
    }
  }
  root <- (a + b)/2
  (b - iter)/iter -> iter
  #sua tu thuyet doi thanh tuyet doi
  print(iter)
  #tinh toc do hoi tu cua ham

  return(root)
}