## 시계열 분석(time series analysis)  ##########################################

# 시계열분석 개요
# - 시계열 데이터를 바탕으로 미래의 관측값 예측(forecating)하는 분석
# - 추세성분(trend component), 계절성분(seasonal component), 불규칙성분(irregular component)

# 시계열분석 순석
# - 시계열 데이터 생성 
# - 시각화(팬턴 관찰), 성분분해(component decompostion) -> 추세,계절,불규칙 세분화
# - 예측 지수모델링(exponential modeling), ARIMA(autoregressive integrated moving average)

# 1. 시계열 데이터
# 2. 시계열 데이터 분해
# 3. 지수예측모델
# 4. ARIMA예측모델_정상성,자기상관
# 5. ARIMA예측모델

################################################################################
# 3. 지수예측모델
# - 1.단순지수평활법(simple exponential smoothing)  - 수준
# - 2.홀트지수평활법(Holt exponential smoothing)  - 수준, 기울기
# - 3.홀트-윈터스지수평활법(Holt-winters exponential smoothing) - 수준,기울기,계절

# forecast::ets() 함수를 이용한 지수예측모델
# - A:가법모델, M:승법모델, N:불포함, Z:가법/승법모델

################################################################################


# 데이터 수집    ---------------------------------------------------------------
str(LakeHuron) # Time-Series [1:98] 

LakeHuron



# 기술 통계      ---------------------------------------------------------------
summary(LakeHuron)




# 데이터 전처리  ---------------------------------------------------------------



# 데이터 분석    ---------------------------------------------------------------

# - 1.단순지수평활법(simple exponential smoothing)  - 수준
plot(LakeHuron,
     col="royalblue",
     lwd=2,
     xlab="Year",
     ylab="Level (Feet)",
     main="Annual Level of Lake Huron")

library(forecast)

#-------------------------------------------------------------------------------
?forecast::ets # {forecast} Exponential smoothing state space model

# ets(
#   y,
#   model = "ZZZ",
#   damped = NULL,
#   alpha = NULL,
#   beta = NULL,
#   gamma = NULL,
#   phi = NULL,
#   additive.only = FALSE,
#   lambda = NULL,
#   biasadj = FALSE,
#   lower = c(rep(1e-04, 3), 0.8),
#   upper = c(rep(0.9999, 3), 0.98),
#   opt.crit = c("lik", "amse", "mse", "sigma", "mae"),
#   nmse = 3,
#   bounds = c("both", "usual", "admissible"),
#   ic = c("aicc", "aic", "bic"),
#   restrict = TRUE,
#   allow.multiplicative.trend = FALSE,
#   use.initial.values = FALSE,
#   na.action = c("na.contiguous", "na.interp", "na.fail"),
#   ...
# )
#-------------------------------------------------------------------------------

model_ann = forecast::ets(LakeHuron,
                          model="ANN")
model_ann

# Smoothing parameters:
#   alpha = 0.9999 
# 
# Initial states:
#   l = 580.3824 
# 
# sigma:  0.7491
# 
# AIC     AICc      BIC 
# 396.6777 396.9331 404.4326 

plot(model_ann)



# 예측 
#-------------------------------------------------------------------------------
?forecast::forecast # {forecast} Forecasting time series

# ## Default S3 method:
# forecast(object, ...)

# ## S3 method for class 'ts'
# forecast(
#   object,
#   h = ifelse(frequency(object) > 1, 2 * frequency(object), 10),
#   level = c(80, 95),
#   fan = FALSE,
#   robust = FALSE,
#   lambda = NULL,
#   biasadj = FALSE,
#   find.frequency = FALSE,
#   allow.multiplicative.trend = FALSE,
#   model = NULL,
#   ...
# )
#-------------------------------------------------------------------------------

model_ann_pred = forecast::forecast(model_ann,
                                    h=3)
model_ann_pred

#      Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# 1973         579.96 579.0000 580.9200 578.4918 581.4281
# 1974         579.96 578.6025 581.3175 577.8838 582.0362
# 1975         579.96 578.2974 581.6226 577.4172 582.5027

plot(model_ann_pred,
     col="royalblue",
     lwd=2,
     xlab="Year",
     ylab="Level (Feet)",
     main="Annual Level of Lake Huron")


# 예측 성능 확인

#-------------------------------------------------------------------------------
?accuracy # {forecast} Accuracy measures for a forecast model

# accuracy(object, ...)
# 
# ## Default S3 method:
# accuracy(object, x, test = NULL, d = NULL, D = NULL, f = NULL, ...)
#------------------------------------------------------------------------------

forecast::accuracy(model_ann)

#                        ME      RMSE       MAE         MPE      MAPE      MASE      ACF1
# Training set -0.004310596 0.7413889 0.5796276 -0.00082501 0.1001199 0.9898571 0.1320556






# - 2.홀트지수평활법(Holt exponential smoothing)  - 수준, 기울기
#install.packages("fpp")
library(fpp)

str(elecsales) #  Time-Series [1:20]

plot(elecsales,
     col="royalblue",
     lwd=2,
     xlab="Year",
     ylab="Electricity (Gwh)",
     main="Electricity Sales in South Australia")


model_aan = forecast::ets(elecsales,
                          model="AAN")
model_aan # "ets"


# Smoothing parameters:
#   alpha = 1e-04 
#   beta  = 1e-04 
# 
# Initial states:
#   l = 2173.2178 
#   b = 74.0902 
# 
# sigma:  116.2135
# 
# AIC     AICc      BIC 
# 255.6689 259.9546 260.6476 

model_aan_pred = forecast::forecast(elecsales,
                                    h=5)
model_aan_pred # "forecast"


#      Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# 2009       3729.110 3580.177 3878.044 3501.336 3956.884
# 2010       3803.201 3654.268 3952.135 3575.427 4030.975
# 2011       3877.292 3728.359 4026.226 3649.518 4105.066
# 2012       3951.383 3802.450 4100.317 3723.609 4179.157
# 2013       4025.474 3876.540 4174.408 3797.700 4253.248

plot(model_aan_pred,
     col="royalblue",
     lwd=2,
     xlab="Year",
     ylab="Electricity (Gwh)",
     main="Electricity Sales in South Australia")


forecast::accuracy(model_aan_pred)

#                     ME     RMSE      MAE        MPE     MAPE      MASE     ACF1
# Training set 0.4085193 103.9445 74.52426 -0.1144249 2.599136 0.6312125 0.280116



# - 3.홀트-윈터스지수평활법(Holt-winters exponential smoothing) - 수준,기울기,계절

str(AirPassengers) # Time-Series [1:144]

plot(AirPassengers,
     col="royalblue",
     lwd=2,
     xlab="Year",
     ylab="Air Passengers(Thousand)",
     main="Air Passengers")

data = log(AirPassengers)
class(data) # "ts"

plot(data,
     col="royalblue",
     lwd=2,
     xlab="Year",
     ylab="Air Passengers(Thousand) ",
     main="Air Passengers (log)")


model_aaa = forecast::ets(data,
                          model="AAA")
model_aaa 
class(model_aaa) # "ets"


model_aaa_pred = forecast::forecast(model_aaa,
                                    h=12)
model_aaa_pred 
class(model_aaa_pred) # "forecast"


plot(model_aaa_pred,
     col="royalblue",
     fcol="indianred1",
     lwd=2,
     xlab="Year",
     ylab="Air Passengers(Thousand) ",
     main="Forecast Air Passengers (log)")


# 원데이터로 변환
cbind(exp(model_aaa_pred$mean),
      exp(model_aaa_pred$lower),
      exp(model_aaa_pred$upper))







# - 4.최적 모델 자동 생성 방법
library(fpp)

str(austourists) # Time-Series [1:48]
austourists

plot(austourists,
     col="royalblue",
     fcol="indianred1",
     lwd=2,
     xlab="Year",
     ylab="Total Visitor Nights",
     main="Total Visitor Nights")


model_mam = forecast::ets(austourists)

model_mam
# ETS(M,A,M) 선택

class(model_mam) # ets


model_mam_pred = forecast::forecast(model,
                                    h=12)

plot(model_mam_pred,
     col="royalblue",
     fcol="indianred1",
     lwd=2,
     xlab="Year",
     ylab="Total Visitor Nights",
     main="Forecast Total Visitor Nights")






