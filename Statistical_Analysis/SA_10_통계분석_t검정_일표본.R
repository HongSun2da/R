################################################################################
# 통계분석[Statistical Analysis]
#         차이검정
#                 t-검정   - 2개
#                 (t-test)      
#                               일표본 t-검정   - 1개             [One Sample t-test]
#                               독립표본 t-검정 - 2개             [Independent Sample t-test]
#                               대응표본 t-검정 - 반복(2)         [Paired Sample t-test]
#                 분산분석 - 3개
#                 (ANOVA - Analysis of Variance)       
#                               일원 분산분석     - 3개           [One Way ANOVA]
#                               반복측정 분산분석 - 반복(3)       [Repeated Measures ANOVA]
#                               이원 분산분석     - 요인 + 요인   [Two Way ANOVA] - 상호작용 확인
#                               이원 반복측정 분산- 반복 + 요인   [Two Way Repeated Measures ANOVA]
#         관계검정
#                 교차분석 - (명목)
#                 (Chi Square)
#                 상관분석 - (1:1)
#                 (Correlation)
#                 회귀분석 - (1:N)
#                 (Regression)  
#                               단순회귀분석      - 1:1종속(연속) [Linear Regression]
#                               다중회귀분석      - 1:N종속(연속) [Multiple Linear Regression]
#                               로지스틱 회귀분석 - 1:N종속(명목) [Logistic Regression]
################################################################################

#-------------------------------------------------------------------------------
# 통계분석 > 차이검정 > t-검정> 일표본 t-검정   - 1개             [One Sample t-test]
#-------------------------------------------------------------------------------

# 1. -------------------------------
# 데이터 수집
data = read.csv("./data/ost.csv",
                  header=TRUE,
                  na.strings = ".")

# 데이터 이해
View(data)
str(data)   # 'data.frame':	100 obs. of  1 variable:

# H0 : 평균 무게가 320 이다.
# H1 : 평균 무게가 320 아니다.


# 기술통계
library(psych)
psych::describe(data)

#    vars   n   mean    sd median trimmed   mad    min    max  range  skew kurtosis se
# X1    1 100 295.44 20.04 295.77  296.45 19.73 241.97 347.55 105.58 -0.33    -0.12  2


# t-test
test = t.test(data,
               alternative = c("two.sided"),
               mu=320,
               conf.level = 0.95)
test

# One Sample t-test
# 
# data:  data
# t = -12.252, df = 99, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 320
# 95 percent confidence interval:
#   291.4682 299.4213
# sample estimates:
#   mean of x 
# 295.4448 


# 2. -------------------------------
# 데이터 수집
data = read.csv("./data/satisfaction.csv",
                header=TRUE,
                na.strings = ".")

# 데이터 이해
View(data)
str(data)   # 'data.frame':	200 obs. of  2 variables:

# H0 : 만족도 50 이다.
# H1 : 만족도 50 아니다.


# 기술통계
library(psych)
psych::describe(data)

#       vars   n   mean    sd median trimmed   mad  min   max range  skew kurtosis   se
# no       1 200 112.82 64.58  113.5  113.02 83.03  1.0 222.0 221.0 -0.02    -1.24 4.57
# satis    2 200  49.98 17.17   48.4   48.98 15.05 16.2  96.3  80.1  0.52     0.17 1.21


# t-test
test = t.test(data$satis,
              alternative = c("two.sided"),
              mu=50,
              conf.level = 0.95)

test

# One Sample t-test
# 
# data:  data$satis
# t = -0.01277, df = 199, p-value = 0.9898
# alternative hypothesis: true mean is not equal to 50
# 95 percent confidence interval:
#   47.59103 52.37797
# sample estimates:
#   mean of x 
# 49.9845 


# 3. -------------------------------
# 데이터 수집
data = read.csv("./data/calorie.csv",
                header=TRUE,
                na.strings = ".")

# 데이터 이해
View(data)
str(data)   # 'data.frame':	40 obs. of  1 variable:

# H0 : 칼로리가 500 보다 크다.
# H1 : 칼로리가 500 보다 작다.


# 기술통계
library(psych)
psych::describe(data)

#    vars  n   mean   sd median trimmed  mad min max range  skew kurtosis   se
# X1    1 40 498.18 6.37  499.5  498.94 3.71 476 510    34 -1.37     2.86 1.01


# t-test
test = t.test(data,
              alternative = c("less"),
              mu=500,
              conf.level = 0.95)

test

# One Sample t-test
# 
# data:  data
# t = -1.8113, df = 39, p-value = 0.0389
# alternative hypothesis: true mean is less than 500
# 95 percent confidence interval:
#   -Inf 499.8727
# sample estimates:
#   mean of x 
# 498.175 






































