## 某零售商店顧客終身價值(CLV)之影響因素探討與預測模型建立
library(tidyverse)
## 一、資料處理--------

## 載入原始資料

setwd("C:\\Rproject\\Dun\\StatCon_final")
demo_0122 <- read.csv("C:\\Rproject\\Dun\\data\\demo_0122.csv", header = T)
trans2_B <- read.csv("C:\\Rproject\\Dun\\data\\trans2_B.csv", header = T)
trans2_HK <- read.csv("C:\\Rproject\\Dun\\data\\trans2_HK.csv", header = T)

## 將資料依照時間切分成三段
# 第一段:產生訓練集的自變數
# 第二段:產生訓練集的應變數、驗證集的自變數
# 第三段:產生驗證集的應變數

trans1 <- filter(trans2_B, DAY <= 311 & DAY >= 112)
trans2 <- filter(trans2_B, DAY <= 511 & DAY >= 312)
trans3 <- filter(trans2_B, DAY >= 512)

## 訓練集
## 產生訓練集自變數

train_x <- group_by(trans1, household_key) %>%
  summarise(BASKET_QTY = length(unique(BASKET_ID)),
            STORE_QTY = length(unique(STORE_ID)),
            PRODUCT_QTY = sum(PRODUCT_QTY),
            DAY_first = min(DAY),
            DAY_last = max(DAY),
            TRANS_TIME_first = min(TRANS_TIME),
            TRANS_TIME_last = max(TRANS_TIME),
            
            # 共買過幾種不同產品
            SALES_VALUE = sum(SALES_VALUE_sum),
            RETAIL_DISC = sum(RETAIL_DISC_sum),
            COUPON_DISC = sum(COUPON_DISC_sum),
            COUPON_MATCH_DISC = sum(COUPON_MATCH_DISC_sum))

train_data <- full_join(train_x, demo_0122[,c(2,8:13)], by = "household_key")


## 補值:補0
train_data_2 <- train_data
train_data_2$BASKET_QTY <- ifelse(is.na(train_data_2$BASKET_QTY), 0, train_data_2$BASKET_QTY)
train_data_2$STORE_QTY <- ifelse(is.na(train_data_2$STORE_QTY), 0, train_data_2$STORE_QTY)
train_data_2$PRODUCT_QTY <- ifelse(is.na(train_data_2$PRODUCT_QTY), 0, train_data_2$PRODUCT_QTY)
train_data_2$SALES_VALUE <- ifelse(is.na(train_data_2$SALES_VALUE), 0, train_data_2$SALES_VALUE)
train_data_2$RETAIL_DISC <- ifelse(is.na(train_data_2$RETAIL_DISC), 0, train_data_2$RETAIL_DISC)
train_data_2$COUPON_DISC <- ifelse(is.na(train_data_2$COUPON_DISC), 0, train_data_2$COUPON_DISC)
train_data_2$COUPON_MATCH_DISC <- ifelse(is.na(train_data_2$COUPON_MATCH_DISC), 0, train_data_2$COUPON_MATCH_DISC)

## 補值:補中位數

train_data_2$DAY_first <- ifelse(is.na(train_data_2$DAY_first), median(train_data_2$DAY_first, na.rm = T), train_data_2$DAY_first)
train_data_2$DAY_last <- ifelse(is.na(train_data_2$DAY_last), median(train_data_2$DAY_last, na.rm = T), train_data_2$DAY_last)
train_data_2$TRANS_TIME_first <- ifelse(is.na(train_data_2$TRANS_TIME_first), median(train_data_2$TRANS_TIME_first, na.rm = T), train_data_2$TRANS_TIME_first)
train_data_2$TRANS_TIME_last <- ifelse(is.na(train_data_2$TRANS_TIME_last), median(train_data_2$TRANS_TIME_last, na.rm = T), train_data_2$TRANS_TIME_last)

summary(train_data_2)
## 新增變數
train_data_3 <- mutate(train_data_2,
                       RES_DAY = 311 - DAY_last,
                       DISCOUNT = RETAIL_DISC + COUPON_DISC + COUPON_MATCH_DISC)
summary(train_data_3)

train <- train_data_3

train$TRANS_TIME_last <- as.character(train$TRANS_TIME_last)
train$TRANS_TIME_last_m <- substr(train$TRANS_TIME_last,3,4)
summary(as.numeric(train$TRANS_TIME_last_m))

train$TRANS_TIME_last <- as.numeric(train$TRANS_TIME_last)
table(train$TRANS_TIME_last == train_data_3$TRANS_TIME_last)

train$TRANS_TIME_last_m <- as.numeric(train$TRANS_TIME_last_m)
train <- mutate(train, 
                TRANS_TIME_last_h = (TRANS_TIME_last - TRANS_TIME_last_m)/100)
summary(train$TRANS_TIME_last_h)
train$TRANS_TIME_last_m_all <- train$TRANS_TIME_last_h * 60 + train$TRANS_TIME_last_m


## 產生訓練集應變數

train_y <- group_by(trans2, household_key) %>%
  summarise(y = sum(SALES_VALUE_sum))

train_data_4 <- full_join(train, train_y, by = "household_key")
summary(train_data_4)

write.csv(train_data_4, "train_data.csv", row.names = F)
getwd()


## 驗證期
## 產生驗證期自變數
test_x <- group_by(trans2, household_key) %>%
  summarise(BASKET_QTY = length(unique(BASKET_ID)),
            STORE_QTY = length(unique(STORE_ID)),
            DAY_first = min(DAY),
            DAY_last = max(DAY),
            TRANS_TIME_first = min(TRANS_TIME),
            TRANS_TIME_last = max(TRANS_TIME),
            PRODUCT_QTY = sum(PRODUCT_QTY),
            
            # 共買過幾種不同產品
            SALES_VALUE = sum(SALES_VALUE_sum),
            RETAIL_DISC = sum(RETAIL_DISC_sum),
            COUPON_DISC = sum(COUPON_DISC_sum),
            COUPON_MATCH_DISC = sum(COUPON_MATCH_DISC_sum))

test_data <- left_join(test_x, demo_0122[,c(2,8:13)], by = "household_key")

test_data_2 <- mutate(test_data,
                      RES_DAY = 511 - DAY_last,
                      DISCOUNT = RETAIL_DISC + COUPON_DISC + COUPON_MATCH_DISC)
summary(test_data_2)


test_data_2$TRANS_TIME_last <- as.character(test_data_2$TRANS_TIME_last)
test_data_2$TRANS_TIME_last_m <- substr(test_data_2$TRANS_TIME_last,3,4)
summary(as.numeric(test_data_2$TRANS_TIME_last_m))

test_data_2$TRANS_TIME_last <- as.numeric(test_data_2$TRANS_TIME_last)
table(test_data_2$TRANS_TIME_last == test_data$TRANS_TIME_last)

test_data_2$TRANS_TIME_last_m <- as.numeric(test_data_2$TRANS_TIME_last_m)
test_data_2 <- mutate(test_data_2, 
                      TRANS_TIME_last_h = (TRANS_TIME_last - TRANS_TIME_last_m)/100)
summary(test_data_2$TRANS_TIME_last_h)
test_data_2$TRANS_TIME_last_m_all <- test_data_2$TRANS_TIME_last_h * 60 + test_data_2$TRANS_TIME_last_m


## 產生驗證期應變數
test_y <- group_by(trans3, household_key) %>%
  summarise(y = sum(SALES_VALUE_sum))

test_data_4 <- left_join(test_data_2, test_y, by = "household_key")

summary(test_data_4)
write.csv(test_data_4, "test_data.csv", row.names = F)

## 二、建立模型-------------
rm(list = ls())
# 載入資料
train <- read.csv("C:\\Rproject\\Dun\\StatCon_final\\train_data.csv", header = T)
test <- read.csv("C:\\Rproject\\Dun\\StatCon_final\\test_data.csv", header = T)
summary(train)
names(train)

### 1.線性迴歸模型--------

## Model 1

m1 <- lm(y ~ BASKET_QTY+STORE_QTY+TRANS_TIME_last_m_all+
           SALES_VALUE+AGE+INCOME+HOMEOWNER+HOUSEHOLD_SIZE+
           KIDS+RES_DAY+DISCOUNT, data = train[,-1])
summary(m1)
# R2 : 0.6987
s1 <- summary(m1)
AIC(m1) #12737.3

s1_coef <- as.data.frame(round(s1$coefficients,3))
write.csv(s1_coef, "s1_coef.csv")

vif(m1)

# BOXCOX 轉換-------
library(MASS)
bc <- boxcox(m1)
lambda <- bc$x[which.max(bc$y)]
# lambda = 0.505

## Model 2

m2 <- lm(I(y^lambda) ~ BASKET_QTY+TRANS_TIME_last_m_all+
           SALES_VALUE+AGE+
           RES_DAY+DISCOUNT, data = train[,-1])

summary(m2)
AIC(m2) # 5689.217

s2 <- summary(m2)
s2_coef <- as.data.frame(round(s2$coefficients,3))
write.csv(s2_coef, "s2_coef.csv")

## 線性迴歸模型診斷-----
## 常態檢定
model <- m2

nortest::lillie.test(model$residuals)

hist(model$residuals)
## 變異數齊一性
library(car)
ncvTest(model)

## 殘差圖與QQ圖
plot(model)
hist(model$residuals)

qqPlot(model)

## 離群值/影響點
outlierTest(model)
inf <- influence.measures(model)
inf$is.inf[inf$is.inf == TRUE]

## RandomForest--------
library(randomForest)
cv <- rfcv(train[,-c(1,24)], train[,24], cv.fold = 10)
summary(cv)
cv$error.cv #11最低
train$MARITAL_STATUS_CODE <- as.factor(train$MARITAL_STATUS_CODE)
m_rf <- randomForest(y ~ ., 
                     data = train[,-c(1)], mtry = 11)
m_rf

## 使用與線性迴歸模型 Model 2 相同變數
cv <- rfcv(train[,c(2,3,4,9,13,19,20,23)], train[,24], cv.fold = 10)
summary(cv)
cv$error.cv #8最低

set.seed(623)
m_rf <- randomForest(y ~ BASKET_QTY+STORE_QTY+TRANS_TIME_last_m_all+PRODUCT_QTY+
                       SALES_VALUE+RES_DAY+DISCOUNT+MARITAL_STATUS_CODE, 
                     data = train[,-c(1)], mtry = 8)
m_rf

## 預測模型效果
importance(m_rf)
varImpPlot(m_rf)

pred_rf <- predict(m_rf, train)
hist(pred_rf)

## 預測指標
# 函數
MAD <- function(act, pred){
  sum(abs(act - pred)) / length(act)
}

MSE <- function(act, pred){
  sum((act - pred)^2) / length(act)
}

RMSE <- function(act, pred){
  mse <- sum((act - pred)^2) / length(act)
  return(sqrt(mse))
}
MAPE <- function(act, pred){
  mape <- sum(abs(act - pred)/ act) / length(act)
  return(mape)
}

MAD(train[,"y"], pred_rf)
MSE(train[,"y"], pred_rf)
RMSE(train[,"y"], pred_rf)
MAPE(train[,"y"], pred_rf)

## 使用驗證集進行預測
table(names(train) == names(test))
test$MARITAL_STATUS_CODE <- as.factor(test$MARITAL_STATUS_CODE)
pred_rf_test <- predict(m_rf, test)

MAD(test[,"y"], pred_rf_test)
MSE(test[,"y"], pred_rf_test)
RMSE(test[,"y"], pred_rf_test)
MAPE(test[,"y"], pred_rf_test)

## 調整參數

mtry <- tuneRF(train[,c("BASKET_QTY","STORE_QTY","TRANS_TIME_last_m_all","PRODUCT_QTY",
                        "SALES_VALUE","RES_DAY","DISCOUNT","MARITAL_STATUS_CODE")], train[,24], ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m) #10


## 線性迴歸模型 Model 2 預測效果
pred_lm_tr <- predict(m2, train)
MAD(train[,"y"], pred_lm_tr)
MSE(train[,"y"], pred_lm_tr)
RMSE(train[,"y"], pred_lm_tr)
MAPE(train[,"y"], pred_lm_tr)

pred_lm_test <- predict(m2, test)
MAD(test[,"y"], pred_lm_test)
MSE(test[,"y"], pred_lm_test)
RMSE(test[,"y"], pred_lm_test)
MAPE(test[,"y"], pred_lm_test)