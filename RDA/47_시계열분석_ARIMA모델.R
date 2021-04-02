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
# 5. ARIMA예측모델 (autoregressive integrated moving average forecasting modle)
# - 1. AR (autoregressive model) - 과거 관측값을 이용하여 예측모델
# - 2. MA (moving average model) - 과거 예측오차를 기반으로 예측모델
# - 3. ARMA(p,q) , ARIMA(p,d,q) => forecast::Acf = q , forecast::Pacf = p


################################################################################


# 데이터 수집    ---------------------------------------------------------------

str(Nile) Time-Series [1:100]

class(Nile) # ts

# 기술 통계      ---------------------------------------------------------------




# 데이터 전처리  ---------------------------------------------------------------

data = Nile


# 데이터 분석    ---------------------------------------------------------------

# 1. 시계열 정상성 평가
plot(data,
     col="darkviolet",
     lwd=2,
     xlab="Year",
     ylab="Flow",
     main="Flow of the River Nile")

library(tseries)

#-------------------------------------------------------------------------------
?tseries::adf.test #  {tseries} Augmented Dickey–Fuller Test

# adf.test(x, alternative = c("stationary", "explosive"),
#          k = trunc((length(x)-1)^(1/3)))
#-------------------------------------------------------------------------------

tseries::adf.test(data)

# Dickey-Fuller = -3.3657, Lag order = 4, p-value = 0.0642 (비정상 데이터 이다를 기각 못함)
# alternative hypothesis: stationary



# 차분 처리 => 몇번 차분해야 하는지 확인
library(forecast)

forecast::ndiffs(data)
# [1] 1

data_dif = diff(data)

tseries::adf.test(data_dif)

# Dickey-Fuller = -6.5924, Lag order = 4, p-value = 0.01 (비정상 데이터 이다를 기각함)
# alternative hypothesis: stationary

plot(data_dif,
     col="darkviolet",
     lwd=2,
     xlab="Year",
     ylab="Flow",
     main="Flow of the River Nile (diff)")




# 2. 예측모델 생성

forecast::Acf(data_dif) # q = 1

forecast::Pacf(data_dif) # p = 2

#-------------------------------------------------------------------------------
?arima # {stats} ARIMA Modelling of Time Series

# arima(x, order = c(0L, 0L, 0L),
#       seasonal = list(order = c(0L, 0L, 0L), period = NA),
#       xreg = NULL, include.mean = TRUE,
#       transform.pars = TRUE,
#       fixed = NULL, init = NULL,
#       method = c("CSS-ML", "ML", "CSS"), n.cond,
#       SSinit = c("Gardner1980", "Rossignol2011"),
#       optim.method = "BFGS",
#       optim.control = list(), kappa = 1e6)
#-------------------------------------------------------------------------------


model = arima(data,
              order=c(0,1,1))
model


# Coefficients:
#           ma1
#       -0.7329
# s.e.   0.1143
# 
# sigma^2 estimated as 20600:  log likelihood = -632.55,  aic = 1269.09

accuracy(model)
#                    ME     RMSE      MAE       MPE     MAPE     MASE      ACF1
# Training set -11.9358 142.8071 112.1752 -3.574702 12.93594 0.841824 0.1153593





# 3. 예측모델 평가 와 예측

qqnorm(model$residuals,
       pch=21,
       col="black",
       bg="gold",
       main="Q-Q Plot of Residuals")
qqline(model$residuals)


Box.test(model$residuals)

# data:  model$residuals
# X-squared = 1.3308, df = 1, p-value = 0.2487


model_pred = forecast::forecast(model, h=5)

class(model_pred) # forecast

model_pred
#      Point Forecast    Lo 80     Hi 80    Lo 95    Hi 95
# 1971       798.3673 614.4307  982.3040 517.0605 1079.674
# 1972       798.3673 607.9845  988.7502 507.2019 1089.533
# 1973       798.3673 601.7495  994.9851 497.6663 1099.068
# 1974       798.3673 595.7063 1001.0284 488.4240 1108.311
# 1975       798.3673 589.8381 1006.8965 479.4494 1117.285


plot(model_pred,
     col="darkviolet",
     lwd=2,
     xlab="Year",
     ylab="Flow",
     main="Flow of the River Nile")





# ARIMA 모델 자동 생성 하기

gas
str(gas) # Time-Series [1:476]

model = forecast::auto.arima(gas)

class(model) # "forecast_ARIMA" "ARIMA"          "Arima"    

model

# Series: gas 
# ARIMA(2,1,1)(0,1,1)[12] 
# 
# Coefficients:
#          ar1     ar2      ma1     sma1
#       0.3756  0.1457  -0.8620  -0.6216
# s.e.  0.0780  0.0621   0.0571   0.0376
# 
# sigma^2 estimated as 2587081:  log likelihood=-4076.58
# AIC=8163.16   AICc=8163.29   BIC=8183.85


model_pred = forecast::forecast(model, h=12*5)

plot(model_pred)












