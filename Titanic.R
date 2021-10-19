### 使用Titanic資料(包含船員資料)
library(tidyverse)
setwd("C:\\Rproject\\StatCon\\HW4")

data <- read.csv("titanic.csv", header = T)


## 資料處理-------------
## 新增crew變數
data$crew <- case_when(data$class == "1st" ~ 0,
                       data$class == "2nd" ~ 0,
                       data$class == "3rd" ~ 0,
                       data$class == "deck crew" ~ 1,
                       data$class == "engineering crew" ~ 1,
                       data$class == "restaurant staff" ~ 1,
                       data$class == "victualling crew" ~ 1)

## 遺失值處理
# 補值
summary(data)
data2 <- data
data2$ticketno <- ifelse(is.na(data2$ticketno) & data2$crew == 1, 
                         "crew", data2$ticketno)
sum(is.na(data2$ticketno))
data2$fare <- ifelse(is.na(data2$fare) & data2$crew == 1, 
                     0, data2$fare)

summary(data2$fare)
sum(data2$fare <= 1, na.rm = T)
data2$sibsp <- ifelse(is.na(data2$sibsp) & data2$crew == 1, 
                      0, data2$sibsp)

summary(data2$sibsp)

data2$parch <- ifelse(is.na(data2$parch) & data2$crew == 1, 
                      0, data2$parch)

summary(data2$parch)

data3 <- data2
data3$country <- ifelse(data3$country != "England"& data3$country != "United States", 
                        "other", data3$country)

data3$country[is.na(data3$country)] <- "other"
data3$lnfare <- log(data3$fare)
table(data3$country)
unique(data3$country)


## EDA--------
var_d <- data3[,c("gender", "class", "embarked",
                  "country", "survived", "crew")]
var_c <- data2[,c("age", "fare", "sibsp", "parch")]
var_c$lnfare <- log(var_c$fare)

library(funModeling)
plot_num(var_c)
freq(var_d, input = "crew")
summary(data2)
length(unique(var_d$country))

cross_plot(var_d, target = "survived")
p_value <- c()
for (i in (1:5)) {
  tab <- table(var_d[,i], var_d$survived)
  test <- chisq.test(tab)
  p_value[i] <- test$p.value
}
round(p_value, 5)
chisq.test(var_d$class, var_d$survived)

plotar(data3, input = "lnfare", target = "survived", plot_type = "boxplot")
cross_plot(data3, input = "parch", target = "survived")
var_c2 <- var_c[complete.cases(var_c),]
mat <- round(as.matrix(cor(var_c2[,-5])),4)
write.csv(mat, "cor_mat.csv")

## 檢視三個艙等乘客的票價差異

sub1 <- filter(data3, class == "1st")
sub2 <- filter(data3, class == "2nd")
sub3 <- filter(data3, class == "3rd")
hist(sub1$fare)
hist(sub2$fare)
hist(sub3$fare)

summary(sub1$fare)
summary(sub2$fare)
summary(sub3$fare)

df <- rbind(sub1, sub2, sub3)
summary(df)
plotar(df, input = "lnfare", target = "class", plot_type = "boxplot")

## model-------------
### logistic------------
data4 <- data3
summary(data4)
data4$name <- NULL
data4$ticketno <- NULL
summary(data4)
data5 <- data4[complete.cases(data4),]


summary(data5) # 2179obs
data6 <- data5

## 將類別變數改為數字表示
table(data6$gender, data5$gender)
data6$gender <- ifelse(data6$gender == "female", 0, 1)
data6$gender <- as.factor(data6$gender)

data6$class <- as.numeric(as.factor(data6$class))
data6$class <- as.factor(data6$class)

table(data6$class, data5$class)

table(data6$country, data5$country)
data6$country <- case_when(data6$country == "England" ~ 1,
                           data6$country == "United States" ~ 2,
                           data6$country == "other" ~ 3)
data6$country <- as.factor(data6$country)
table(data6$embarked, data5$embarked)
data6$embarked <- case_when(data6$embarked == "B" ~ 4,
                            data6$embarked == "C" ~ 2,
                            data6$embarked == "Q" ~ 3,
                            data6$embarked == "S" ~ 1)

data6$embarked <- as.factor(data6$embarked)
data6$survived  <- ifelse(data6$survived  == "no", 0, 1)
table(data6$survived, data5$survived)

data6$survived <- as.factor(data6$survived)

data6$crew <- as.factor(data6$crew)

### 切割資料-------
library(caret)
set.seed(3456)
trainIndex <- createDataPartition(data6$survived, p = .8, 
                                  list = FALSE, 
                                  times = 1)

traindata <- data6[trainIndex,]
testdata <- data6[-trainIndex,]

## 使用fare+crew
# AIC: 1744.7
data7 <- traindata[,-c(1,4,12)]
mL1 <- glm(survived ~., data =  data7, 
           family = binomial(link = "logit"))
summary(mL1)
anova(mL1, test = "LRT") 

## 使用class+fare
# AIC: 1641.9
data8 <- traindata[,-c(1,11,12)]
mL2 <- glm(survived ~., data =  data8, 
           family = binomial(link = "logit"))
summary(mL2)
anova(mL2, test = "LRT")

# mL2:不要parch
#AIC: 1639.9
data9 <- traindata[,-c(1,9,11,12)]
mL3 <- glm(survived ~., data =  data9, 
           family = binomial(link = "logit"))
summary(mL3)
anova(mL3, test = "LRT")

## step
data10 <- traindata[,-c(1,11,12)]
mL10 <- glm(survived ~., data =  data10, 
            family = binomial(link = "logit")) # AIC: 1641.9
summary(mL10) 
m_step <- step(mL10, direction = "both")
summ <- summary(m_step) # AIC: 1635.5
summ_co <- as.data.frame(round(summ$coefficients, 4))
summ_co$expB <- exp(summ_co$Estimate)
write.csv(summ_co, "logistic_res.csv")

library(DescTools)
y <- as.numeric(traindata$survived) -1 
yhat <- fitted(m_step)
anova(m_step, test = "LRT")

# HosmerLemeshow Test
pred <- predict(mL2, traindata, type = "response")
HosmerLemeshowTest(yhat, y)

PseudoR2(m_step)

sum(is.na(pred))


## 模型預測效果
library(InformationValue)
predict.prob <- predict(m_step, testdata, type = "response")
opt.cutoff <- optimalCutoff(testdata$survived, predict.prob)
mat <- confusionMatrix(testdata$survived, predict.prob, threshold = opt.cutoff)
round(fun(mat),4)

table(testdata$survived)
plotROC(testdata$survived, predict.prob)

y <- as.numeric(testdata$survived)-1

AUROC(y, predict.prob)

## random forest---------
library(randomForest)
traindata$survived <- as.factor(traindata$survived)
set.seed(2021)
titanic_rf <- randomForest(survived ~ class + gender + age + 
                             sibsp + parch + fare + embarked + country
                           +crew, data = traindata, 
                           nodesize = 6,importance=TRUE, proximity=TRUE)
summary(titanic_rf)
pred_rf <- predict(titanic_rf, testdata, type = "prob")

pred_rf_r <- predict(titanic_rf, testdata, type = "response")
tab <- table(pred_rf_r, testdata$survived)
class(tab)
sum(tab)
tab[1,1]

pred_rf2 <- as.numeric(pred_rf[,2])
y <- as.numeric(testdata$survived)-1
head(y)
AUROC(y, pred_rf2)

head(pred_rf)
print(titanic_rf)


## CV---------
cv <- rfcv(traindata[,-c(1,6,10,12)], traindata[,10], cv.fold = 10)
summary(cv)
cv$error.cv # 4個比較適當，error.cv最小

rf.label <- as.factor(traindata$survived)
set.seed(123)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                       index = cv.10.folds)
cl <- makeCluster(6, type = "SOCK")

registerDoSNOW(cl)

## 比較預測效果指標-----------

fun <- function(mat){
  TN <- mat[1,1]
  FN <- mat[1,2]
  FP <- mat[2,1]
  TP <- mat[2,2]
  accuracy <- (TN+TP)/sum(mat)
  precision <- TP/(TP+FP)
  recall <- TP/(TP+FN)
  specificity <- TN/(TN+FP)
  F1 <- 2/(1/precision+1/recall)
  ans <- c(accuracy, precision, recall, specificity, F1)
  return(ans)
}

fun(tab)

## 20次模擬
n <- 20
RES_L <- matrix(NA, ncol = 6, nrow = n)
RES_R <- matrix(NA, ncol = 6, nrow = n)
for (i in (1:n)) {
  trainIndex <- createDataPartition(data6$survived, p = .8, 
                                    list = FALSE, 
                                    times = 1)
  
  traindata <- data6[trainIndex,]
  testdata <- data6[-trainIndex,]
  traindata$survived <- as.factor(traindata$survived)
  ## logistic
  m_step <- glm(survived ~ class + gender + age + 
                  sibsp + embarked, data =  traindata, 
                family = binomial(link = "logit"))
  predict.prob <- predict(m_step, testdata, type = "response")
  opt.cutoff <- optimalCutoff(testdata$survived, predict.prob)[1]
  mat <- confusionMatrix(testdata$survived, predict.prob, threshold = opt.cutoff)
  res <- fun(mat)
  y <- as.numeric(testdata$survived)-1
  AUC_L <- AUROC(y, predict.prob)
  RES_L[i,] <- c(res, AUC_L)
  ## random forest
  titanic_rf <- randomForest(survived ~ class + gender + age + 
                               sibsp + parch + fare + embarked + country
                             +crew, data = traindata, 
                             nodesize = 6, importance=TRUE, proximity=TRUE)
  prob_rf <- predict(titanic_rf, testdata, type = "prob")
  pred_rf <- predict(titanic_rf, testdata, type = "response")
  tab <- table(pred_rf, testdata$survived)
  res <- fun(tab)
  prob_rf2 <- as.numeric(prob_rf[,2])
  y <- as.numeric(testdata$survived)-1
  AUC_R <- AUROC(y, prob_rf2)
  RES_R[i,] <- c(res, AUC_R)
  print(i)
}

mean_L <- apply(RES_L, 2, mean)
mean_R <- apply(RES_R, 2, mean)
mean_L
mean_R

mat <- as.data.frame(rbind(mean_L, mean_R))
colnames(mat) <- c("accuracy", "precision", "recall", "specificity", "F1_score", "AUC")
row.names(mat) <- c("羅吉士迴歸", "隨機森林")
write.csv(mat, "model_res.csv")