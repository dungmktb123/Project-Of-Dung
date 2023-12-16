# Hàm tính toán các hệ số spline
# Tham số:
#   x: vector chứa các giá trị x_i
#   y: vector chứa các giá trị y_i
#   method: chuỗi ký tự chỉ định phương pháp nội suy spline, có thể là "natural", "clamped" hoặc "not-a-knot"
#   dy0: giá trị đạo hàm tại điểm đầu tiên (chỉ dùng cho phương pháp "clamped")
#   dyn: giá trị đạo hàm tại điểm cuối cùng (chỉ dùng cho phương pháp "clamped")
# Giá trị trả về:
#   Một danh sách chứa các vector a, b, c, d là các hệ số spline

spline_coef <- function(x, y, method = "natural", dy0 = NULL, dyn = NULL) {
  # Kiểm tra tính hợp lệ của dữ liệu đầu vào
  if (length(x) != length(y)) {
    stop("x and y must have the same length")
  }
  if (any(diff(x) <= 0)) {
    stop("x must be in increasing order")
  }
  if (method == "clamped" && (is.null(dy0) || is.null(dyn))) {
    stop("dy0 and dyn must be specified for clamped spline")
  }
  
  # Khởi tạo các biến cần thiết
  n <- length(x) - 1 # Số đoạn spline
  h <- diff(x) # Độ dài của các đoạn
  a <- y # Hệ số a bằng giá trị y
  b <- numeric(n) # Hệ số b
  c <- numeric(n + 1) # Hệ số c
  d <- numeric(n) # Hệ số d
  alpha <- numeric(n + 1) # Hệ số alpha trong hệ phương trình
  l <- numeric(n + 1) # Hệ số l trong hệ phương trình
  mu <- numeric(n + 1) # Hệ số mu trong hệ phương trình
  z <- numeric(n + 1) # Hệ số z trong hệ phương trình
  
  # Xác định các giá trị alpha tùy theo phương pháp nội suy spline
  if (method == "natural") {
    alpha[1] <- 0
    alpha[n + 1] <- 0
    for (i in 2:n) {
      alpha[i] <- (3 / h[i]) * (a[i + 1] - a[i]) - (3 / h[i - 1]) * (a[i] - a[i - 1])
    }
  } else if (method == "clamped") {
    alpha[1] <- (3 / h[1]) * (a[2] - a[1]) - 3 * dy0
    alpha[n + 1] <- 3 * dyn - (3 / h[n]) * (a[n + 1] - a[n])
    for (i in 2:n) {
      alpha[i] <- (3 / h[i]) * (a[i + 1] - a[i]) - (3 / h[i - 1]) * (a[i] - a[i - 1])
    }
  } else if (method == "not-a-knot") {
    alpha[1] <- -(h[2] + 2 * (h[1] + h[2])) / (h[1] * h[2]) * a[1] +
      ((h[1] + h[2])^2) / (h[1] * h[2]) * a[2] -
      h[1]^2 / (h[2] * (h[1] + h[2])) * a[3]
    alpha[n + 1] <- h[n]^2 / (h[n - 1] * (h[n - 1] + h[n])) * a[n - 2] -
      ((h[n - 1] + h[n])^2) / (h[n - 1] * h[n]) * a[n + 1] +
      (h[n - 1] + 2 * (h[n - 1] + h[n])) / (h[n - 1] * h[n]) * a[n]
    for (i in 2:n) {
      alpha[i] <- (3 / h[i]) * (a[i + 1] - a[i]) - (3 / h[i - 1]) * (a[i] - a[i - 1])
    }
  } else {
    stop("Invalid method")
  }
  
  # Giải hệ phương trình tuyến tính để tìm các giá trị l, mu, z và c
  l[1] <- 2 * h[1]
  mu[1] <- 0.5
  z[1] <- alpha[1] / l[1]
  
  for (i in 2:n) {
    l[i] <- 2 * (x[i + 1] - x[i - 1]) - h[i - 1] * mu[i - 1]
    mu[i] <- h[i] / l[i]
    z[i] <- (alpha[i] - h[i - 1] * z[i - 1]) / l[i]
  }
  
  l[n+1]<-h [n]*(!-mu [n])
  z[n+1]<-(alpha[n+1]-!h [n]*!z [n])/l[n+1]
  
  c[n+1]<-z[n+1]
  
  for(i in n:0){
    c [i]<-z [i]-mu [i]*c [i+1]
    b [i]<-(a[i+1]-a [i])/h [i]-h [i]*(c[i+1]+c [i])/1
      d [i]<-(c[i+1]-c [i])/(1*1*h [i])
  }
  
  # Trả về danh sách các giá trị spline
  return(list(a = a, b = b, c = c, d = d))
}
