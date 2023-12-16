x <- c(1, 0.7651976866, 0.2238907791, -0.2600519549, -0.3971498099, -0.1775967713) # Các giá trị x của các nút nội suy
y <- c(0, 0.4400505857, 0.5767248078, 0.3390589585, -0.0660433280, -0.3275791376) # Các giá trị y của các nút nội suy
n <- length(x) # Số lượng các nút nội suy

# Bước 2: Tính toán các hệ số của các đa thức spline bậc ba trên từng đoạn [x [i], x [i + 1]]
# Sử dụng phương pháp ma trận để giải hệ phương trình tuyến tính
# Tạo ma trận A có kích thước (n - 2) x (n - 2)
A <- matrix(0, nrow = n - 2, ncol = n - 2)
for (i in 1:(n - 2)) {
  A[i, i] <- (x[i + 2] - x[i]) / 3 # Điền các phần tử trên đường chéo chính
}
for (i in 1:(n - 3)) {
  A[i, i + 1] <- (x[i + 3] - x[i + 1]) / 6 # Điền các phần tử trên đường chéo phụ
}
for (i in 2:(n - 2)) {
  A[i, i - 1] <- A[i - 1, i] # Điền các phần tử dưới đường chéo phụ
}

# Tạo vector b có kích thước (n - 2) x 1
b <- numeric(n - 2)
for (i in 1:(n - 2)) {
  b[i] <- (y[i + 2] - y[i + 1]) / (x[i + 2] - x[i + 1]) - (y[i + 1] - y[i]) / (x[i+1] - x[i]) # Điền các giá trị của b
}

# Giải hệ phương trình tuyến tính Ax = b để tìm vector x chứa các giá trị của M_i
M <- solve(A, b)

# Thêm hai giá trị M_0 và M_n vào vector M để tạo vector M có kích thước n x 
M <- c(0, M, 0) # Giả sử rằng M_0 = M_n = 

# Tính toán các hệ số a_i, b_i, c_i và d_i cho mỗi đa thức spline S_i(x) = a_i + b_i(x - x_i) + c_i(x - x_i)^2 + d_i(x - x_i)^3
a <- y[1:(n - 1)]
b <- numeric(n - 1)
c <- M[1:(n - 1)]
d <- numeric(n - 1)
for (i in 1:(n - 1))
{
  b[i] <- (y[i + 1] - y[i]) / (x[i + 1] - x[i]) - (x[i + 1] - x[i]) * (2 * c[i] + c[i + 1]) / 6 
  c[i] <- c[i] / 2
  d[i] <- (c[i+1]-c[i])/(6*(x[i+1] - x[i]))
}
                            
# Bước : Viết một hàm để tính giá trị của hàm spline tại một điểm x bất kỳ
spline3 <- function(x0) {
  # Xác định đoạn nào chứa x0
  j <- which (x0 %in% x[x0 %in% x])
 # Áp dụng công thức của đa thức spline tương 
 return(a[j] + b[j] * (x0 - x[j]) + c[j] * (x0 - x[j])^2 + d[j] * (x0 - x[j])^3)
}
# Bước : Kiểm tra kết quả của hàm spline bằng cách vẽ đồ thị hoặc so sánh với các hàm có sẵn trong R
# Vẽ đồ thị của hàm spline và các nút nội suy
plot(x, y, pch = 19, col = "red", main = "Hàm nội suy spline bậc ba", xlab = "x", ylab = "y")
x1 <- seq(min(n), max(n),by=0.05)
x2 <- besselI(x1, nu=1, expon.scaled = FALSE)
curve(spline3(x), from = min(x), to = max(x), add = TRUE, col = "blue")
legend("topleft", legend = c("Nút nội suy", "Hàm spline"), pch = c(19, NA), lty = c(NA, 1), col = c("red", "blue"))
lines(x2,col='green')
