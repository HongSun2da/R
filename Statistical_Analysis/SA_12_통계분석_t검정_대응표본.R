################################################################################
# 통계분석[Statistical Analysis]
#         차이검정
#                 t-검정   - 2개
#                 (t-test)      
#                               일표본 t-검정   - 1개             [One Sample t-test]
#                               독립표본 t-검정 - 2개             [Independent Sample t-test]
#                               대응표본 t-검정 - 반복(2)         [Paired Sample t-test]
#                 분산분석 - 3개
#                 (ANOVA)       
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
# 통계분석 > 차이검정 > t-검정> 대응표본 t-검정 - 반복(2)         [Paired Sample t-test]
#-------------------------------------------------------------------------------


# 1. -------------------------------
# 데이터 수집
data = read.csv("./data/pst.csv",
                header=TRUE,
                na.strings = ".")


# 데이터 이해
View(data)
str(data)   # 'data.frame':	20 obs. of  3 variables:

# H0 : 다이어트약을 먹기 전과 후의 체중은 변화가 없다.
# H1 : 다이어트약을 먹기 전과 후의 체중은 변화가 있다.


# 기술통계
library(psych)
psych::describe(data)

#      vars  n  mean   sd median trimmed  mad min max range  skew kurtosis   se
# ID      1 20 10.50 5.92   10.5   10.50 7.41   1  20    19  0.00    -1.38 1.32
# pre     2 20 73.15 6.43   75.0   73.88 3.71  54  83    29 -1.26     1.88 1.44
# post    3 20 70.60 6.10   71.5   71.62 3.71  50  77    27 -1.88     3.90 1.36


# t-test # var.equal = TRUE 등분산 확인 후 설정
test = t.test(data$pre, data$post,
              alternative = c("two.sided"),
              paired=TRUE,
              conf.level = 0.95)
test

# Paired t-test
# 
# data:  data$pre and data$post
# t = 3.6355, df = 19, p-value = 0.00176
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   1.081935 4.018065
# sample estimates:
#   mean of the differences 
# 2.55 


