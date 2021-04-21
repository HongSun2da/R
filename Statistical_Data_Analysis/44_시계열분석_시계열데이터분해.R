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
# 2. 시계열 데이터 분해
# - 1. 비계절 데이터 분해
#   - 평활법 : 단순이동평균(simple moving average) / 중심이동평균(centered moving average)

# - 2. 계절 데이터 분해
#   - 가법모델(additive model), 승법모델(multiplicative model)


################################################################################


# 데이터 수집    ---------------------------------------------------------------
nhtemp
View(nhtemp)

str(nhtemp) # Time-Series [1:60]



# 기술 통계      ---------------------------------------------------------------



# 데이터 전처리  ---------------------------------------------------------------



# 데이터 분석    ---------------------------------------------------------------

# - 1. 비계절 데이터 분해
#   - 평활법 : 단순이동평균(simple moving average) / 중심이동평균(centered moving average)
plot(nhtemp, 
     col="dimgray",
     lwd=2,
     ylab="Temperature",
     main="Base Time series")

# install.packages("forecast")
library(forecast)

#-------------------------------------------------------------------------------
?forecast::ma # {forecast} Moving-average smoothing

# ma(x, order, centre = TRUE)
#-------------------------------------------------------------------------------

forecast::ma(nhtemp, 3)
forecast::ma(nhtemp, 7)
forecast::ma(nhtemp, 11)

plot(forecast::ma(nhtemp, 3))
plot(forecast::ma(nhtemp, 7))
plot(forecast::ma(nhtemp, 11))



# - 2. 계절 데이터 분해
#   - 가법모델(additive model), 승법모델(multiplicative model)

str(co2) # Time-Series [1:468]

data = window(co2, 
              start=c(1985,5),
              end=c(1996,12))

str(data) # Time-Series [1:140]

plot(data)

#-------------------------------------------------------------------------------
?stl # {stats} Seasonal Decomposition of Time Series by Loess

# stl(x, s.window, s.degree = 0,
#     t.window = NULL, t.degree = 1,
#     l.window = nextodd(period), l.degree = t.degree,
#     s.jump = ceiling(s.window/10),
#     t.jump = ceiling(t.window/10),
#     l.jump = ceiling(l.window/10),
#     robust = FALSE,
#     inner = if(robust)  1 else 2,
#     outer = if(robust) 15 else 0,
#     na.action = na.fail)
#-------------------------------------------------------------------------------

data_decomp = stl(data,
                  s.window="periodic")
class(data_decomp)

data_decomp #  "seasonal" "trend" "remainder"

plot(data_decomp,
     col="darkcyan",
     col.range="skyblue",
     lwd=2)

data_decomp$time.series

# 348.74 == 3.17270692 + 345.6029 - 0.0355793802

data

# 계절 효과를 제외한 그래프

plot(data - data_decomp$time.series[, "seasonal"],
     col="tomato",
     lwd=2,
     xlab="Year",
     ylab="CO2 Concentration",
     main="Co2 Concentration Time Series With Remain")

# 월별 그래프
monthplot(data)


forecast::seasonplot(data,
                     year.labels = TRUE)



str(AirPassengers) # Time-Series [1:144]
View(AirPassengers)

plot(AirPassengers,
     col="maroon",
     lwd=2,
     xlab="Year",
     ylab="Air Passengers",
     main="Air Passengers")

#   - 가법모델(additive model) <- 승법모델(multiplicative model)

lair = log(AirPassengers)
plot(lair,
     col="maroon",
     lwd=2,
     xlab="Year",
     ylab="Air Passengers",
     main="Air Passengers(Log)")

# - 가법모델(additive model) 분해
lair_decomp = stl(lair, s.window = "periodic")
plot(lair_decomp)


lair_decomp$time.series

exp(lair_decomp$time.series)












