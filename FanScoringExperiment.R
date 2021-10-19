rm(list = ls())

### 載入套件與資料------
library(tidyverse)
data <- read.csv("C:\\Rproject\\FINISHED\\DOE\\DOEfinal\\FanData.csv", header = T)

### 變數編碼說明-----
# 繳交位置Position (上中下) = (0,1,2)
# 風速Power (中強) = (0,1)
# 紙質Material (非doubleA 80磅, doubleA 80磅) = (0,1)
# y : Target.length/All.length
###

# 變數類別設定
data$Position <- as.factor(data$Position)
data$Power <- as.factor(data$Power)
data$Material <- as.factor(data$Material)

# 交互作用圖-------
# interaction plot
## 交互作用圖結論
# 1. Position 跟 Material 可能有交互作用
# 2. Position 跟 Power 可能有交互作用

# Target.length 為應變數
interaction.plot(data$Position, data$Power, data$Target.length)
interaction.plot(data$Position, data$Material, data$Target.length)
interaction.plot(data$Material, data$Power, data$Target.length)

# 建立模型----------
# model 1:含三項交互作用 
m1 <- lm(y ~ Position * Power * Material, data = data)
summary(m1)
anova(m1)

## QQ plot
coef <- m1$coef[-1]
ord <- order(coef)
length(coef)
plot(qnorm(1:11/12), coef[ord], xlab="normal quantiles", ylab="effects", 
     col = "red", pch = 16)
text(qnorm(1:11/12), coef[ord], names(coef[ord]))
coef[ord]

# model 2:二項交互作用
m2 <- lm(y ~ Position + Power + Material + 
           Position:Power + Material:Position + Material:Power, data = data)
summary(m2)
anova(m2)


# model 3:不含交互作用 
m3 <- lm(y ~ Position + Power + Material, data = data)

g3 <- aov(y ~ Position + Power + Material, data = data)
summary(m3)
anova(m3)

### 模型診斷-------

## 變異數齊一性
car::ncvTest(m3)
plot(m3)

## 應變數調整 : 開根號
m4 <- lm(sqrt(y) ~ Position + Power + Material, data = data)
summary(m4)
anova(m4)
plot(m4)

## 對model 4做模型驗證

## 常態性
shapiro.test(m4$residuals)

## 變異數齊一性
car::ncvTest(m4)

## Durbin-Watson test
## 由於資料非時間序列資料，此項可不用檢定
lmtest::dwtest(m4)

