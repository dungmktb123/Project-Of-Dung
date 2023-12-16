a = 0
b = 1
err = 1e-1
f <- function(x){
  return(sin(x))
}
sinx <- function(f,a,b,err){
  h = (b-a)
  n = 1
  int_old = 0
  while(TRUE){
    h = (b-a)/n
    int = f(a)/2 + f(b)/2
    
    for(i in 1:(n-1)){
      x = a +i*h
      int = int + f(x)
    }
    int = h*int
    if(abs(int - int_old) < err){
      break
    }
    
    int_old = int
    n = 2*n
  }
print(n)
print(int)
}
sinx(f,a,b,err)

