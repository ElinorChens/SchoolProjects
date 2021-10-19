## 使用CDNOW資料進行MCMC參數估計和評估模型預測效果
## 自動尋找最佳的sigma使得接受率可以維持在0.23-0.44

rm(list = ls())

library(tidyverse)
library(mpcmp)
library(rstan)

### data----------------

CDNOW <- read.csv("C:\\Rproject\\PAPER\\CDNOW_new_fun\\CDNOW_input_xijC_0522.csv", header = T)

Tx <- 39 # 觀察時間終點

# 由大到小排列 : mi, xi., ID
## 使用每個人不同觀察週數Ti
CDNOW2 <- arrange(CDNOW, desc(mi), desc(xi.),ID)


### functions----------------
# full conditionals----------

## mu

fun.mu.item <- function(k, lambda, nu, mu, Z){
  res <- (exp(-mu * (beta + k - 1)) - exp(-mu * (beta + k))) / Z^k
  return(sum(res))
}

fun.mu <- function(lambda, nu, mu, mi, Ti){
  Z <- exp(logZ_c(log(lambda), nu, summax = 10^6))
  res_time <- c(mi:Ti)
  item <- fun.mu.item(res_time, lambda, nu, mu, Z)
  res <- mu^(s - 1) * (exp(-mu * (beta + Ti))/Z^Ti + item)
  return(res)
}

## lambda

fun.lam.item <- function(k, lambda, nu, mu, Z){
  res <- (exp(-mu * (k - 1)) - exp(-mu * k)) / Z^k
  return(sum(res))
}

fun.lam <- function(lambda, nu, mu, xi., mi, Ti){
  Z <- exp(logZ_c(log(lambda), nu, summax = 10^6))
  res_time <- c(mi:Ti)
  item <- fun.lam.item(res_time, lambda, nu, mu, Z)
  res <- lambda^(xi. + r - 1) * exp(-alpha * lambda) * Z / (Z - 1) * 
    (exp(-mu * Ti)/Z^Ti + item)
  return(res)
}

## nu

fun.nu <- function(lambda, nu, mu, xi., mi, Ti, data_i){
  Z <- exp(logZ_c(log(lambda), nu, summax = 10^6))
  item1 <- prod(factorial(data_i))
  res_time <- c(mi:Ti)
  item2 <- fun.lam.item(res_time, lambda, nu, mu, Z)
  res <- nu^(omega - 1) * exp(-gamma * nu) * Z / ((Z-1) * item1^nu) *
    (exp(-mu * Ti)/Z^Ti + item2)
  return(res)
}

### MCMC steps-------------

## nn : 樣本數
Metro <- function(lam_in, nu_in, mu_in, data, nn, sigma_L, sigma_n, sigma_m){
  # data
  xi. <- data[1,"xi."]
  mi <- data[1,"mi"]
  Ti <- data[1,"Ti"]
  Fx <- data[1,"Fx"]
  id <- data[1,"ID"]
  data_i <- as.numeric(data[1, c(Fx:Tx)])
  ## con 變數:確定三個參數的接受率有無落入設定範圍內
  conL_1 <- conL_2 <- conn_1 <- conn_2 <- conm_1 <- conm_2 <- 0
  conA <- 1
  times <- 0
  while (conA != 0) {
    times <- times + 1 # 計算共調整參數幾次
    lambda <- lam_in
    nu <- nu_in 
    mu <- mu_in
    ## 自動調整sigma數值，使接受率落入設定範圍中
    sigma_L <- sigma_L + conL_1 * sigma_L*0.05 - conL_2 * sigma_L*0.05
    sigma_n <- sigma_n + conn_1 * sigma_n*0.05 - conn_2 * sigma_n*0.05
    sigma_m <- sigma_m + conm_1 * sigma_m*0.05 - conm_2 * sigma_m*0.05
    acc_L <- 0
    acc_n <- 0
    acc_m <- 0
    lambda_sample <- rep(NA, nn)
    nu_sample <- rep(NA, nn)
    mu_sample <- rep(NA, nn)
    ## 開始抽樣 ##
    for (j in (1:nn)) {
      # 抽 mu
      mu_new <- rnorm(1, mu, sigma_m)
      ## 若新抽出參數樣本小於0，直接拒絕新樣本
      if (mu_new <= 0) {
        mu_sample[j] <- mu
      } else {
        u <- runif(1)
        con <- fun.mu(lambda, nu, mu_new, mi, Ti) / fun.mu(lambda, nu, mu, mi, Ti)
        #print(paste("con, id, lambda, nu, mu_new, mu", con, id, lambda, nu, mu_new, mu))
        A <- min(1, con)
        mu <- ifelse(u <= A, mu_new, mu)
        mu_sample[j] <- mu
        acc_m <- acc_m + sum(u <= A)
      }
      
      # 抽 lambda
      lambda_new <- rnorm(1, lambda, sigma_L)
      if (lambda_new <= 0) {
        lambda_sample[j] <- lambda
      } else {
        u <- runif(1)
        con <- fun.lam(lambda_new, nu, mu, xi., mi, Ti) / fun.lam(lambda, nu, mu, xi., mi, Ti)
        #print(paste("con, id, lambda_new, nu, mu, lambda", con, id, lambda_new, nu, mu, lambda))
        con <- ifelse(is.na(con), 0, con)
        A <- min(1, con)
        lambda <- ifelse(u <= A, lambda_new, lambda)
        lambda_sample[j] <- lambda
        acc_L <- acc_L + sum(u <= A)
      }
      
      # 抽nu
      nu_new <- rnorm(1, nu, sigma_n)
      if (nu_new <= 0) {
        nu_sample[j] <- nu
      } else {
        u <- runif(1)
        upper <- fun.nu(lambda, nu_new, mu, xi., mi, Ti, data_i)
        lower <- fun.nu(lambda, nu, mu, xi., mi, Ti, data_i)
        con <- upper/lower
        #print(paste(upper, lower))
        #print(paste("con, id, lambda, nu_new, mu, nu", con, id, lambda, nu_new, mu, nu))
        con <- ifelse(is.na(con), 0, con)
        A <- min(1, con)
        nu <- ifelse(u <= A, nu_new, nu)
        nu_sample[j] <- nu
        acc_n <- acc_n + sum(u <= A)
      }
    }
    # 判斷接受率是否在範圍內
    acc_ra_L <- acc_L/nn
    acc_ra_n <- acc_n/nn
    acc_ra_m <- acc_m/nn
    acc_U <- 0.44 # 接受率上界
    acc_L <- 0.23 # 接受率下界
    print(paste("ACC_RATE:",acc_ra_L,acc_ra_n,acc_ra_m))
    print(paste("times", times))
    print(paste("ID", data[1,"ID"]))
    conL_1 <- sum(acc_ra_L > acc_U) # =T=1表示lambda接受率超過上界，要將sigma_L調大
    conL_2 <- sum(acc_ra_L < acc_L) # =T=1表示小於下界，要將sigma_L調小
    conn_1 <- sum(acc_ra_n > acc_U)
    conn_2 <- sum(acc_ra_n < acc_L)
    conm_1 <- sum(acc_ra_m > acc_U)
    conm_2 <- sum(acc_ra_m < acc_L)
    conA <- conL_1 + conL_2 + conn_1 + conn_2 + conm_1 + conm_2
  }
  # 輸出結果
  sample <- cbind(lambda_sample, nu_sample, mu_sample)
  ## 一個參數一欄,維度nn*3
  res <- list(ID = data$ID,
              sample = sample,
              sigma = c(sigma_L, sigma_n, sigma_m),
              acc_rate = c(acc_ra_L, acc_ra_n, acc_ra_m),
              times = times)
  return(res)
}

## 使用comilper套件cmpfun函數，增進函數運算速度
library(compiler)
cMetro <- cmpfun(Metro)

### initial value-----------------

# prior dist parameter
s <- 1
beta <- 1
r <- 1
alpha <- 1
gamma <- 1
omega <- 1

### Metro函數測試------------

test2 <- cMetro(0.83, 0.184, 0.044, data_cal_2[data_cal_2$ID == "396",], 
                10000, 0.6522869, 0.1827341, 0.2292463)

test2$sigma
test2$acc_rate
test2$times
monitor(test2$sample[,1])
monitor(test2$sample[,2])
monitor(test2$sample[,3])
ts.plot(test2$sample[,1])
ts.plot(test2$sample[,2])
ts.plot(test2$sample[,3])
acf(test2$sample[,1])
acf(test2$sample[,2])
acf(test2$sample[,3])
mean2 <- apply(test2$sample, 2, mean)

## 開始 MCMC 參數估計-----------

## 抽出lambda, nu, mu起始值函數
par_in_create <- function(){
  lam_in <- rgamma(1,1,1)
  nu_in <- rgamma(1,1,1)
  ## 設定lambda, nu 抽出範圍，避免算出Z值太大
  while (lam_in > (10^nu_in)){
    lam_in <- rgamma(1,1,1)
    nu_in <- rgamma(1,1,1)
  }
  mu_in <- rgamma(1,1,1)
  par_in <- c(lam_in, nu_in, mu_in)
  return(par_in)
}

## 多ID完整迴圈---------

## CDNOW3 : 要進行抽樣的ID和其資料
CDNOW3 <- CDNOW2[c(1:268),]

## pre:上次抽樣結果，可先用相同sigma，加速抽樣過程
pre <- read.csv("C:\\Rproject\\PAPER\\CDNOW_new_fun\\PGCMPac_res2357_2W_0619.csv", header = T)

## 設定第一個ID的sigma
## 沒有之前抽樣sigma的時候，先使用第一個ID找出合乎接受率範圍的sigma數值
test$sigma
sigma_L <- 0.6522869
sigma_n <- 0.1827341
sigma_m <- 0.2292463

## 開始抽樣----------
# chain : 抽樣序列數(回數)
# nn : 每回抽樣數

chains <- 4
nn <- 5000  
RES_all <- vector("list",nrow(CDNOW3))
sims_all <- vector("list",nrow(CDNOW3))
for (i in (1:nrow(CDNOW3))) {
  print(i)
  data <- CDNOW3[i,]
  no <- data$ID
  ## 若有上次抽樣結果產生的sigma數值，則用下三行程式碼，否則讓函數自行找出符合條件的sigma
  # sigma_L <- pre[pre$ID == no, "sigma_L"]
  # sigma_n <- pre[pre$ID == no, "sigma_N"]
  # sigma_m <- pre[pre$ID == no, "sigma_M"]
  sims <- array(NA, c(nn, chains, 3),
                dimnames = list(NULL, NULL, c("lambda", "nu", "mu")))
  for (w in (1:chains)) {
    par_in <- par_in_create()
    lam_in <- par_in[1]
    nu_in <- par_in[2]
    mu_in <- par_in[3]
    RES <- cMetro(lam_in, nu_in, mu_in, data, nn, sigma_L, sigma_n, sigma_m)
    ## 輸出抽出樣本為 array
    sims[,w,1] <- RES$sample[,1]
    sims[,w,2] <- RES$sample[,2]
    sims[,w,3] <- RES$sample[,3]
    ## 讓下一回一開始使用的sigma為這一回sigma最後調好的數值，減省下一回找sigma的時間
    sigma_L <- RES$sigma[1]
    sigma_n <- RES$sigma[2]
    sigma_m <- RES$sigma[3]
  }
  ## 收集和輸出所有人的結果
  RES_all[[i]] <- RES ## 存最後一回的資訊
  sims_all[[i]] <- sims
}
Sys.time() # 記錄最後跑完迴圈的時間

## 輸出MCMC結果-----------
# 將輸出的array存成RDS物件
saveRDS(RES_all, "RES_cd_2357_nwF_0701.rds")
saveRDS(sims_all, "sims_cd_2357_nwF_0701.rds")

# 產生用來分析的數值
# nr : 這次迴圈跑的ID數

nr <- nrow(CDNOW3)
res_mat <- matrix(NA, nrow = nr, ncol = 22)
mon <- vector("list", nr)
for (i in (1:nr)) {
  mon[[i]]  <- mon_i <- monitor(sims_all[[i]])
  RES_i <- RES_all[[i]]
  res_mat[i,1] <- RES_i$ID
  res_mat[i,c(2:4)] <- mon_i$mean
  res_mat[i,c(5:7)] <- mon_i$Rhat
  res_mat[i,c(8:10)] <- mon_i$Bulk_ESS
  res_mat[i,c(11:13)] <- mon_i$Tail_ESS
  res_mat[i,c(14:16)] <- mon_i$n_eff
  res_mat[i,c(17:19)] <- RES_i$sigma
  res_mat[i,c(20:22)] <- RES_i$acc_rate
}
res_df <- as.data.frame(res_mat)
names(res_df) <- c("ID", "mean_L",  "mean_N",  "mean_M",
                   "Rhat_L", "Rhat_N","Rhat_M",
                   "Bulk_ESS_L", "Bulk_ESS_N", "Bulk_ESS_M",
                   "Tail_ESS_L", "Tail_ESS_N", "Tail_ESS_M",
                   "n_eff_L", "n_eff_N", "n_eff_M",
                   "sigma_L", "sigma_N", "sigma_M",
                   "acc_rate_L", "acc_rate_N", "acc_rate_M")

data_buy <- select(CDNOW3, ID, xi., mi)
MHac_res <- left_join(res_df, data_buy, by = "ID")

write.csv(MHac_res, "MH_s3_449_500_75N_0630.csv", row.names = F)

## 檢視抽樣情形
# sima_all的維度[x,y,z]
# x:每回抽樣數(ie. nn=5000)
# y:抽樣回數(ie. chain=4)
# z:哪個參數(ie. lambda, nu, mu)
ts.plot(sims_all[[1]][,,"lambda"])
ts.plot(sims_all[[187]][,3,"nu"])
ts.plot(sims_all[[1]][,,"mu"])

acf(as.numeric(sims_all[[1]][c(5000:10000),,"lambda"]))


### 計算預測值----------------
## function
## CMP dist 期望值計算 : copy from compoisson package

com.log.sum = function(x,y)		# log.sum(x,y) = log( exp(x) + exp(y) )
{
  if (x == -Inf)
  { return (y); }
  else if (y == -Inf)
  { return (x); }
  else if (x > y)
  { return (x + log( 1 + exp(y - x) ) ); }
  else
  { return (y + log( 1 + exp(x - y) ) ); }
}

com.log.density = function(x, lambda, nu, log.z = NULL)
{
  # Perform argument checking
  if (lambda < 0 || nu < 0)
    stop("Invalid arguments, only defined for lambda >= 0, nu >= 0");
  if (x < 0 || x != floor(x))
    return (0);
  if (is.null(log.z)) { log.z = mpcmp::logZ_c(log(lambda), nu, summax = 10^6); }
  
  # Return log pmf
  return ((x * log(lambda) - nu * com.log.factorial(x)) - log.z);
}

# com.log.factorial == lfactorial()
com.log.factorial = function(x)	# log(factorial(x))
{
  if (is.vector(x) && length(x) > 1)
  {
    for (i in 1:length(x))
      x[i] = com.log.factorial(x[i]);
    return (x);
  }
  else if (is.numeric(x))
  {
    if (x == 0) { x = 1; }
    return (sum(log(seq(from = 1, to = x, by = 1))));
  }
  else { stop("x must be a vector or number."); }
}

com.expectation = function(f, lambda, nu, log.error = 0.001)
{
  log.z = mpcmp::logZ_c(log(lambda), nu, summax = 10^6);
  
  # Initialize variables
  ex = -Inf;
  ex.last = 0;
  j = 0;
  
  # Continue until we have reached specified precision
  while ((ex == -Inf && ex.last == -Inf) || abs(ex - ex.last) > log.error)
  {
    ex.last = ex;
    ex = com.log.sum(ex, log(f(j)) + com.log.density(j, lambda, nu, log.z));
    
    j = j + 1;
  }
  return (exp(ex));
}

com.mean = function(lambda, nu)
{
  return ( com.expectation(function (x) {x}, lambda, nu) );
}

## CMP的期望值計算函數
com.mean(0.8,2.66)

# P-alive 函數

fun.PAlive.item <- function(k, lambda, nu, mu, Z){
  res <- (exp(-mu * (k -1)) - exp(-mu * k)) / Z^k
  return(sum(res))
}

P_Alive <- function(lambda, nu, mu, xi., mi, Ti){
  Z <- exp(logZ_c(log(lambda), nu, summax = 10^6))
  res_time <- c(mi:Ti)
  item <- fun.PAlive.item(res_time, lambda, nu, mu, Z)
  res <- exp(-mu * Ti) / (exp(-mu * Ti) + Z^Ti * item)
  return(res)
}

# 未來t時間購買次數期望值函數

PRED <- function(t, lambda, nu, mu, p_alive){
  x_hat = com.mean(lambda,nu)
  res = x_hat/(1 - exp(-mu)) * (1 - exp(-mu * t)) * 
    p_alive
  return(res)
}

# 預測評估指標
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

### 計算預測值-------------------
Tx <- 39
## 載入資料

## 合併所有ID的資料
MH_res_2347 <- read.csv("C:\\Rproject\\PAPER\\MH_2357_0630.csv", header = T)

## 載入驗證期實際交易次數:ans

ans <- read.csv("C:\\Rproject\\PAPER\\CDNOW_new_fun\\ansC_0522.csv", header = T)
names(ans)[1] <- "ID"
names(ans)[1] 

## 合併抽樣結果與ans
MH_res_2347_2 <- left_join(MH_res_2347_1, ans[,c("ID", "ans_trans")], by = "ID")

## 參數估計值的敘述統計
summary(MH_res_2347_2[,c(2:4)])

mean(MH_res_2347_2$mean_L)
mean(MH_res_2347_2$mean_N)
mean(MH_res_2347_2$mean_M)

apply(MH_res_2347_2[,c(2:4)], 2, var)

hist(MH_res_2347_2$mean_L)

p1 <- ggplot(MH_res_2347_2) +
  geom_histogram(aes(x = mean_L), binwidth = 0.1) +
  labs(x = "參數 λ 估計值", y ="次數")

p2 <- ggplot(MH_res_2347_2) +
  geom_histogram(aes(x = mean_N), binwidth = 0.1) +
  labs(x = "參數 ν 估計值", y ="次數")

p3 <- ggplot(MH_res_2347_2) +
  geom_histogram(aes(x = mean_M), binwidth = 0.1) +
  labs(x = "參數 μ 估計值", y ="次數")
library(patchwork)
p1+p2+p3

## 計算p_alive估計值--------

nr <- nrow(MH_res_2347_2)
palive <- c()
for (i in (1:nr)) {
  lambda <- MH_res_2347_2$mean_L[i]
  nu <- MH_res_2347_2$mean_N[i]
  mu <- MH_res_2347_2$mean_M[i]
  xi. <- MH_res_2347_2$xi.[i]
  mi <- MH_res_2347_2$mi[i]
  Ti <- MH_res_2347_2$Ti[i]
  #Ti <- Tx
  palive[i] <- P_Alive(lambda, nu, mu, xi., mi, Ti)
}

P_alive_50 <- data.frame(
  ID = MH_res_2347_2$ID,
  PAlive = palive
)

MH_res_2347_3 <- left_join(MH_res_2347_2, P_alive_50, by = "ID")
hist(MH_res_2347_3$PAlive)

## 計算未來t時間購買次數期望值
nr <- 2357

PRED50 <- c()
for (i in (1:nr)) {
  PRED50[i] <- PRED(38.85714, MH_res_2347_3$mean_L[i], MH_res_2347_3$mean_N[i], MH_res_2347_3$mean_M[i],  MH_res_2347_3$PAlive[i])
}

PRED50_df <- data.frame(ID = MH_res_2347_3$ID,
                        PRED = PRED50)

MH_res_2347_4 <- left_join(MH_res_2347_3, PRED50_df, by = "ID")

## 輸出結果
write.csv(MH_res_2347_4, "PGCMPac_cd_2357_newF_0702.csv", row.names = F)
getwd()
## 評估預測效果---------------

MAD(MH_res_2347_4$ans_trans, MH_res_2347_4$PRED)
MSE(MH_res_2347_4$ans_trans, MH_res_2347_4$PRED)
RMSE(MH_res_2347_4$ans_trans, MH_res_2347_4$PRED)

## 與BTYD模型比較----------
res_BTYD <- read.csv("C:\\Rproject\\PAPER\\paperAppendix\\data\\pred_BTYD_C.csv", header = T)

res <- left_join(MH_res_2347_4, res_BTYD, by = "ID")

MAD(res$ans_trans, res$pred_bgnbd)
MSE(res$ans_trans, res$pred_bgnbd)
RMSE(res$ans_trans, res$pred_bgnbd)

MAD(res$ans_trans, res$pred_pnbd)
MSE(res$ans_trans, res$pred_pnbd)
RMSE(res$ans_trans, res$pred_pnbd)