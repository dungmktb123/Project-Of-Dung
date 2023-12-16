#youtube estimate khá nhỏ nhưng vẫn tác động lên sales
#newspaper estimate nhỏ và sai số còn lớn hơn estimate => t value rất gần 0
#=> newspaper gần như không tác động
#confint(model) dùng để tìm ra được xác suất xảy ra trường hợp hiếm.
load("marketing.rda")
model  <- lm(sales ~ youtube + facebook+newspaper, data = marketing)
summary(model)

RSS <- sum((marketing$sales - (3.526667 +0.045765*marketing$youtube + 0.188530*marketing$facebook- 0.001037*marketing$newspaper))^2)
TSS <- sum((marketing$sales - mean(marketing$sales))^2)
R <- 1- RSS/TSS
