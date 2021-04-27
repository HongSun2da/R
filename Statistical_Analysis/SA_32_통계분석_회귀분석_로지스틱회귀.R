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
# 통계분석 > 차이검정 > 회귀분석(Regression) > 로지스틱 회귀분석 - 1:N종속(명목) [Logistic Regression]
#-------------------------------------------------------------------------------


# 1. -------------------------------
# 데이터 수집
train = read.csv("./data/Lline_reg.csv",
                 header=TRUE,
                 na.strings = ".")

train$chun = factor(train$chun,
                    levels = c(0, 1),
                    labels = c("No", "Yes"))

# 데이터 이해
View(train)
str(train)   # 'data.frame':	60 obs. of  3 variables:


# 기술통계
#install.packages("psych")
library(psych)
psych::describe(train)

#       vars   n  mean   sd median trimmed  mad min max range  skew kurtosis   se
# phy      1 100 44.86 7.45     45   44.94 4.45  26  60    34 -0.10     0.05 0.74
# psy      2 100 21.78 4.88     22   21.80 4.45   9  30    21 -0.12    -0.56 0.49
# cmmt     3 100 22.95 4.21     24   23.24 4.45  12  32    20 -0.52    -0.36 0.42
# exp      4 100  0.52 0.50      1    0.52 0.00   0   1     1 -0.08    -2.01 0.05
# chun*    5 100  1.28 0.45      1    1.23 0.00   1   2     1  0.97    -1.08 0.05

# 그래프 확인(상관 관계)
psych::pairs.panels(train)



# 로지시틱회귀분석
model = glm(chun ~ phy + psy + cmmt + exp, 
            data = train,
            family = binomial)

model    # chun = 14.80336 + (-0.05610*phy) + (-0.06552*psy) + (-0.60321*cmmt) + (2.02826*exp)

summary(model)

# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept) 14.80336    3.60443   4.107 4.01e-05 ***
# phy         -0.05610    0.06693  -0.838   0.4019    
# psy         -0.06552    0.09872  -0.664   0.5069    
# cmmt        -0.60321    0.12860  -4.691 2.72e-06 ***
# exp          2.02826    0.85186   2.381   0.0173 *  
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 118.591  on 99  degrees of freedom
# Residual deviance:  55.449  on 95  degrees of freedom
# AIC: 65.449
# 
# Number of Fisher Scoring iterations: 6


# 로지스틱 회귀분석 Odds 계산 추가
odds = data.frame(summary(model)$coefficients,
                  odds = exp(coef(model)))
odds
#                Estimate Std..Error    z.value     Pr...z..         odds
# (Intercept) 14.80335609 3.60443490  4.1069839 4.008592e-05 2.685443e+06
# phy         -0.05609676 0.06692506 -0.8382027 4.019169e-01 9.454476e-01
# psy         -0.06552442 0.09871971 -0.6637420 5.068555e-01 9.365762e-01
# cmmt        -0.60320582 0.12859706 -4.6906657 2.723176e-06 5.470551e-01
# exp          2.02826264 0.85185868  2.3809849 1.726642e-02 7.600869e+00



