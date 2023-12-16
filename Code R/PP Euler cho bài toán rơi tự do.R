MyEulerSystem <- function(f, g, x0, xN, y0, z0, dx = (xN - x0) / 10) {
  xi <- c()
  yi <- c()
  zi <- c()
  i <- 1
  
  xi[1] <- x0
  yi[1] <- y0
  zi[1] <- z0
  
  while (xi[i] < xN) {
    xi[i + 1] <- xi[i] + dx
    yi[i + 1] <- yi[i] + f(xi[i], yi[i], zi[i]) * dx
    zi[i + 1] <- zi[i] + g(xi[i], yi[i], zi[i]) * dx
    zi[i + 1] <- zi[i] + ((g(xi[i + 1], yi[i+1],zi[i+1])+g(xi[i],yi[i],zi[i]))/2) * dx
    yi[i + 1] <- yi[i] + ((f(xi[i + 1], yi[i+1],zi[i+1])+f(xi[i],yi[i],zi[i]))/2) * dx
    i <- i + 1
  }
  
  return(data.frame(x = xi, y = yi, z = zi))
}

# Định nghĩa hàm f(x, y, z) = x * y * z
f <- function(x, y, z) {
  return(z)
}

# Định nghĩa hàm g(x, y, z) = x * y * z
g <- function(x, y, z) {
  return(-9.8)
}

# Gọi hàm MyEulerSystem với điều kiện ban đầu y(0) = 1, z(0) = 1 và vùng x từ 0 đến 1
result <- MyEulerSystem(f, g, x0 = 0, xN = 2, y0 = 100, z0 = 0)
print(result)

x <- seq(0, 2, length.out = 100)
y <- seq(0, 1, length.out = 100)
z <- seq(0, 1, length.out = 100)
y_origin <- (-9.8*x*x*0.5+100)
z_origin <- -9.8*x   

plot(x, y_origin, type = "l", col = "red", xlab = "x", ylab = "y", main = "So sánh với MyEulerSystem")
#plot(x, z_origin, type = "l", col = "red", xlab = "x", ylab = "z", main = "So sánh với MyEulerSystem")

# Vẽ đồ thị của MyEulerSystem cho y và z
points(result$x, result$y, col = "blue")
points(result$x, result$z, col = "green")
legend("topright", legend = c("Hàm gốc y", "Hàm gốc z", "MyEulerSystem y", "MyEulerSystem z"), 
       col = c("red", "red", "blue", "green"), lty = 1)