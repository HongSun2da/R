#####################################################################################################
# 통계분석
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
#####################################################################################################

#---------------------------------------------------------------------------------------------------
# 통계분석 > 관계검정 > 회귀분석 >  로지스틱 회귀분석 - 1:N종속(명목) [Logistic Regression]


df_data = read.csv("Ch1104.로지스틱회귀분석(LREG).csv",
                   header = TRUE,
                   na.strings = ".")
df_data
str(df_data)

summary(df_data)

df_data$chun = factor(df_data$chun,
                      level=c(0,1),
                      label=c("N", "Y"))


# 02. 기술통계 확인  -----------------------------------------------------------
library(psych)

describe(df_data)

pairs.panels(df_data)


# 03. 로지스틱 회귀분석 확인  --------------------------------------------------


model = glm(chun ~ phy+psy+cmmt+exp, data = df_data,
            family = binomial)

model
# Coefficients:
# (Intercept)          phy          psy         cmmt          exp  
# 14.80336     -0.05610     -0.06552     -0.60321      2.02826  
# 
# Degrees of Freedom: 99 Total (i.e. Null);  95 Residual
# Null Deviance:	    118.6 
# Residual Deviance: 55.45 	AIC: 65.45

summary(model)

# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept) 14.80336    3.60443   4.107 4.01e-05 ***
# phy         -0.05610    0.06693  -0.838   0.4019    
# psy         -0.06552    0.09872  -0.664   0.5069    
# cmmt        -0.60321    0.12860  -4.691 2.72e-06 ***
# exp          2.02826    0.85186   2.381   0.0173 * 


# 04. 로지스틱 회귀분석 Odds 계산 추가  ----------------------------------------
odds = data.frame(summary(model)$coefficients,
                  odds = exp(coef(model)))
odds
#                Estimate Std..Error    z.value     Pr...z..         odds
# (Intercept) 14.80335609 3.60443490  4.1069839 4.008592e-05 2.685443e+06
# phy         -0.05609676 0.06692506 -0.8382027 4.019169e-01 9.454476e-01
# psy         -0.06552442 0.09871971 -0.6637420 5.068555e-01 9.365762e-01
# cmmt        -0.60320582 0.12859706 -4.6906657 2.723176e-06 5.470551e-01
# exp          2.02826264 0.85185868  2.3809849 1.726642e-02 7.600869e+00







