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
# 1. 시계열 데이터

################################################################################


# 데이터 수집    ---------------------------------------------------------------

utility = read.table(url("http://jse.amstat.org/datasets/utility.dat.txt"))
utility

str(utility) # 'data.frame':	81 obs. of  13 variables:
View(utility)




# 기술 통계      ---------------------------------------------------------------
library(psych)

psych::describe(utility)
psych::pairs.panels(utility)





# 데이터 전처리  ---------------------------------------------------------------

# 데이터 변환(시계열)
#-------------------------------------------------------------------------------
?ts # {stats} Time-Series Objects

# ts(data = NA, start = 1, end = numeric(), frequency = 1,
#    deltat = 1, ts.eps = getOption("ts.eps"), class = , names = )
# as.ts(x, ...)
# is.ts(x)
#-------------------------------------------------------------------------------

data = ts(data=utility[7],
          start=c(1990,9),
          frequency=12)

str(data) # Time-Series [1:81, 1]
summary(data)


# 데이터 분석    ---------------------------------------------------------------

plot(data)

plot(data,
     col="salmon",
     lwd=2,
     xlab="Year",
     ylab="Electricity Usage",
     main="Electricity Usage Trend of Boston Area")

start(data)
end(data)
frequency(data)

deltat(data) # 0.08333333 == 1 / frequency(data)

time(data) # 한칸 기준은 deltat(data)임

window(data, start=c(1991,1), end=c(1992,6))
window(data, start=c(1991,1), frequency=1)
window(data, start=c(1991,1), frequency=2)
window(data, start=c(1991,1), frequency=4)










