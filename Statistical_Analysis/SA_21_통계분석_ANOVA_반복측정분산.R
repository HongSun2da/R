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
# 통계분석 > 차이검정 > 분산분석(ANOVA) > 반복측정 분산분석 - 반복(3)       [Repeated Measures ANOVA]
#-------------------------------------------------------------------------------


# 1. -------------------------------
# 데이터 수집
train = read.csv("./data/rma.csv",
                 header=TRUE,
                 na.strings = ".")

train$time = factor(train$time,
                     levels = c(1:3),
                     labels = c("Init", "3Month", "6Month"))

# 데이터 이해
View(train)
str(train)   # 'data.frame':	45 obs. of  4 variables:

# H0 : 어학프로그램은 시점(M, M+3, M+6)에 따라 차이가 없다.
# H1 : 어학프로그램은 시점(M, M+3, M+6)에 따라 차이가 있다.
#    -> [사후검정]   


# 기술통계
library(psych)
psych::describe(train)

#     vars  n  mean    sd median trimmed   mad min max range  skew kurtosis   se
# id     1 45 23.00 13.13     23   23.00 16.31   1  45    44  0.00    -1.28 1.96
# m      2 45 60.20  3.22     60   60.16  2.97  53  67    14  0.18    -0.71 0.48
# m.3    3 45 61.60  3.60     61   61.49  4.45  55  70    15  0.24    -0.92 0.54
# m.6    4 45 68.91  1.14     69   68.95  1.48  66  71     5 -0.19    -0.48 0.17


# ANOVA 검정
model = aov(score ~ time + Error(id/time),
                 data = train)
summary(model)

#              Df Sum Sq Mean Sq F value Pr(>F)    
# time          1   1707  1707.4     169 <2e-16 ***
# Residuals   133   1344    10.1      



# 다중비교 (Multicamparison test) - t-value 포함

#install.packages("multcomp")
library(multcomp)

model_lm = lm(score ~ time, data=train)

summary(model_lm)


tukey.result = glht(model_lm, linfct = mcp(time="Tukey"))

summary(tukey.result)
#                       Estimate Std. Error t value Pr(>|t|)    
# 3Month - Init == 0     1.4000     0.6035    2.32   0.0565 .  
# 6Month - Init == 0     8.7111     0.6035   14.44   <1e-04 ***
# 6Month - 3Month == 0   7.3111     0.6035   12.12   <1e-04 ***

plot(tukey.result)



































































