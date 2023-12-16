# Nhập dữ liệu
ti = seq(0,1,by=0.1);
vi = c(-0.10290, 0.37364, 2.43748, 3.93836, 3.31230, 5.49472, 5.43325, 6.39321, 9.06048, 9.36416, 9.52066)
# Minh họa dữ liệu
plot(ti,vi)

# Thiết lập hệ phương trình
V <- matrix(nrow = 2, ncol = 2)
Y <- vector()
V[1,1] <- length(ti)
V[1,2] <- sum(ti)
V[2,1] <- sum(ti)
V[2,2] <- sum(ti*ti)
Y[1] <- sum(vi)
Y[2] <- sum(vi*ti)

# Giải hệ phương trình ở đây
Coe <- solve(V,Y)
tmp1 <- -V[1,1]/V[2,1]*V[2,2] + V[1,2]
tmp2 <- - V[1,1]/V[2,1]*Y[2] + Y[1]
b <- tmp2/tmp1
a <- {Y[1] - b*V[1,2]}/V[1,1] 

# Vẽ đường thẳng
t <- seq(0,1,by=0.01)
v <- Coe[1] + Coe[2]*t
lines(t,v)