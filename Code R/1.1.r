myBisect <- function(f, a, b, tol = 1e-6, m = 100) {
  iter <- 0
  f.a <- f(a)
  f.b <- f(b)
  # Check if the function has the same sign at both ends
  if (f.a * f.b > 0) {
    stop("The function has the same sign at both ends of the interval")
  }
  while (abs(b - a) > tol) {
    iter <- iter + 1
    if (iter > m) {
      warning("Maximum number of iterations exceeded")
      break
    }
    xmid <- (a + b) / 2
    ymid <- f(xmid)
    if (f.a * ymid > 0) {
      a <- xmid
      f.a <- ymid
    } else {
      b <- xmid
      f.b <- ymid
    }
  }
  root <- (a + b) / 2
  # Use a more meaningful name for the output
  ratio <- (b - iter) / iter
  return(list(root = root, ratio = ratio))
}
