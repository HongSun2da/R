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
# 4. ARIMA예측모델_정상성(stationarity),자기상관
# 5. ARIMA예측모델 (autoregressive integrated moving average forecasting modle)

################################################################################
# 4. ARIMA예측모델_정상성(stationarity),자기상관
# - 1.정상성이란 시계열 데이터의 특성이 시간의 흐름에 따라 변하지 않는다는 것을 의미(일정한 분산, 일정한 평균)
# - 2.비정상 시계열 -> 차분(differencing) -> 정상 시계열 변환 처리
# - 3.


################################################################################


# 데이터 수집    ---------------------------------------------------------------
#install.packages("fpp2")
library(fpp2)

str(goog200) # Time-Series [1:200]

goog200


# 기술 통계      ---------------------------------------------------------------




# 데이터 전처리  ---------------------------------------------------------------



# 데이터 분석    ---------------------------------------------------------------

plot(goog200,
     col="cornflowerblue",
     lwd=2,
     xlab="Day",
     ylab="Dollars",
     main="Google Stock Prices")

library(forecast)

#-------------------------------------------------------------------------------
?forecast::Acf # {forecast} (Partial) Autocorrelation and Cross-Correlation Function Estimation

# Acf(
#   x,
#   lag.max = NULL,
#   type = c("correlation", "covariance", "partial"),
#   plot = TRUE,
#   na.action = na.contiguous,
#   demean = TRUE,
#   ...
# )
# 
# Pacf(
#   x,
#   lag.max = NULL,
#   plot = TRUE,
#   na.action = na.contiguous,
#   demean = TRUE,
#   ...
# )
# 
# Ccf(
#   x,
#   y,
#   lag.max = NULL,
#   type = c("correlation", "covariance"),
#   plot = TRUE,
#   na.action = na.contiguous,
#   ...
# )
# 
# taperedacf(
#   x,
#   lag.max = NULL,
#   type = c("correlation", "partial"),
#   plot = TRUE,
#   calc.ci = TRUE,
#   level = 95,
#   nsim = 100,
#   ...
# )
# 
# taperedpacf(x, ...)
#-------------------------------------------------------------------------------

forecast::Acf(goog200) # 비정상 시계열



# 차분 시계열 만들기

#-------------------------------------------------------------------------------
?forecast::ndiffs # {forecast} Number of differences required for a stationary series

# ndiffs(
#   x,
#   alpha = 0.05,
#   test = c("kpss", "adf", "pp"),
#   type = c("level", "trend"),
#   max.d = 2,
#   ...
# )
#-------------------------------------------------------------------------------

forecast::ndiffs(goog200) # 차분 횟수 구하기


data_dif = diff(goog200)
data_dif
# [1] -0.317932 =  392.5121 - 392.8300


plot(data_dif,
     col="cornflowerblue",
     lwd=2,
     xlab="Day",
     ylab="Dollars",
     main="Google Stock Prices(diff)")


forecast::Acf(data_dif)



# 정상성 검증 하기
library(tseries)

#-------------------------------------------------------------------------------
?tseries::adf.test # {tseries} Augmented Dickey–Fuller Test

# adf.test(x, alternative = c("stationary", "explosive"),
#          k = trunc((length(x)-1)^(1/3)))
#-------------------------------------------------------------------------------

tseries::adf.test(goog200) 

# data:  goog200
# Dickey-Fuller = -1.7775, Lag order = 5, p-value = 0.6693 (비정상 데이터 이다를 기각 못함)
# alternative hypothesis: stationary


tseries::adf.test(data_dif) 

# data:  data_dif
# Dickey-Fuller = -5.7435, Lag order = 5, p-value = 0.01 (비정상 데이터 이다를 기각함)
# alternative hypothesis: stationary


