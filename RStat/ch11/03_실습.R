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
# 통계분석 > 관계검정 > 회귀분석 >  다중회귀분석      - 1:N종속(연속) [Multiple Linear Regression]


# 01. 데이터 불러오기  ---------------------------------------------------------

df_data = read.csv("Ch1103.다중회귀분석(MREG).csv",
                   header = TRUE,
                   na.strings = ".")
df_data
str(df_data)

summary(df_data)


# 02. 기술통계 확인  -----------------------------------------------------------
library(psych)

describe(df_data)

pairs.panels(df_data)


# 03. 다중회귀분석 확인  -----------------------------------------------------------
# - 1. 전체변수 일괄 입력
# - 2. backword : 변수 제거
# - 3. forword : 변수 제거
# - 4. stepwise : backword 와 forword 동기
# - 5. AIC (Akaike information criterion), BIC(Bayesian ... )

model = lm(flow ~ ., data = df_data)

summary(model)


library(car)
# - 1. 전체변수 일괄 입력
model = lm(flow ~ design+info+comm+op+fb, data = df_data)

anova(model)

# Analysis of Variance Table
# 
# Response: flow
#            Df  Sum Sq Mean Sq F value    Pr(>F)    
# design      1  21.776 21.7762  34.066 2.656e-08 ***
# info        1  10.406 10.4063  16.279 8.265e-05 ***
# comm        1  19.093 19.0933  29.869 1.634e-07 ***
# op          1   7.577  7.5769  11.853 0.0007257 ***
# fb          1   7.242  7.2422  11.329 0.0009443 ***
# Residuals 169 108.031  0.6392 

summary(model)  # ★★★ 제일 하단의 F-statistic이 분산 분석 값

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.0006925  0.0604381  -0.011 0.990871    
# design       0.3539020  0.0605969   5.840 2.61e-08 ***
# info         0.2446373  0.0605957   4.037 8.19e-05 ***
# comm         0.3312351  0.0606013   5.466 1.63e-07 ***
# op           0.2085877  0.0605998   3.442 0.000728 ***
# fb           0.2040364  0.0606183   3.366 0.000944 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7995 on 169 degrees of freedom
# Multiple R-squared:  0.3796,	Adjusted R-squared:  0.3612 
# F-statistic: 20.68 on 5 and 169 DF,  p-value: 4.359e-16

vif(model)  # ★★★  다중분산성 > 10 보다 크면 문제

#   design     info     comm       op       fb 
# 1.000000 1.000001 1.000000 1.000000 1.000000 


# 04. 표준화 회귀계수 확인  -----------------------------------------------------------
# install.packages("lm.beta")

library(lm.beta)

model_beta = lm.beta(model)

summary(model_beta) # ★★★ Standardized Std 표준화 값

# Coefficients:
#               Estimate Standardized Std. Error t value Pr(>|t|)    
# (Intercept) -0.0006925    0.0000000  0.0604381  -0.011 0.990871    
# design       0.3539020    0.3538603  0.0605969   5.840 2.61e-08 ***
# info         0.2446373    0.2446131  0.0605957   4.037 8.19e-05 ***
# comm         0.3312351    0.3311719  0.0606013   5.466 1.63e-07 ***
# op           0.2085877    0.2085529  0.0605998   3.442 0.000728 ***
# fb           0.2040364    0.2039402  0.0606183   3.366 0.000944 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7995 on 169 degrees of freedom
# Multiple R-squared:  0.3796,	Adjusted R-squared:  0.3612 
# F-statistic: 20.68 on 5 and 169 DF,  p-value: 4.359e-16


# 05. 변수 선택 방법  -----------------------------------------------------------
# - 1. 전체변수 일괄 입력
# - 2. backword : 변수 제거
# - 3. forword : 변수 제거
# - 4. stepwise : backword 와 forword 동기
# - 5. AIC (Akaike information criterion), BIC(Bayesian ... )

# ---------------------------
# - 2. backword : 변수 제거
model.b = lm(flow ~ ., data = df_data )

summary(model.b)

model.b = step(model.b,
               direction = "backward",
               trace = TRUE)

# Start:  AIC=-71.23  ★★
# flow ~ id + design + info + comm + op + fb
# 
#          Df Sum of Sq    RSS     AIC
# - id      1    0.5015 108.03 -72.415  ★★★
# <none>                107.53 -71.229
# - fb      1    7.0573 114.59 -62.105
# - op      1    7.3985 114.93 -61.585
# - info    1    9.4385 116.97 -58.506
# - comm    1   18.4837 126.01 -45.471
# - design  1   20.6299 128.16 -42.515
# 
# Step:  AIC=-72.42  ★★
# flow ~ design + info + comm + op + fb
# 
#          Df Sum of Sq    RSS     AIC
# <none>                108.03 -72.415
# - fb      1    7.2422 115.27 -63.060
# - op      1    7.5735 115.60 -62.558
# - info    1   10.4189 118.45 -58.303
# - comm    1   19.0972 127.13 -45.929
# - design  1   21.8035 129.83 -42.243


# ---------------------------
# - 3. forword : 변수 제거
  
model.f = lm(flow ~ 1, data = df_data )

summary(model.f)

model.f = step(model.f,
               direction = "forward",
               scope=(flow ~ design+info+comm+op+fb),
               trace = TRUE)

# Start:  AIC=1.12
# flow ~ 1
# 
#          Df Sum of Sq    RSS      AIC
# + design  1   21.7762 152.35 -20.2570 ★★★
# + comm    1   19.0888 155.04 -17.1969
# + info    1   10.3936 163.73  -7.6474
# + op      1    7.5677 166.56  -4.6528
# + fb      1    7.2282 166.90  -4.2965
# <none>                174.12   1.1231
# 
# Step:  AIC=-20.26
# flow ~ design
# 
#        Df Sum of Sq    RSS     AIC
# + comm  1   19.0950 133.25 -41.692 ★★★
# + info  1   10.4063 141.94 -30.638
# + op    1    7.5707 144.78 -27.177
# + fb    1    7.2335 145.12 -26.770
# <none>              152.35 -20.257
# 
# Step:  AIC=-41.69
# flow ~ design + comm
# 
#        Df Sum of Sq    RSS     AIC
# + info  1   10.4046 122.85 -53.919 ★★★
# + op    1    7.5681 125.69 -49.925
# + fb    1    7.2400 126.01 -49.469
# <none>              133.25 -41.692
# 
# Step:  AIC=-53.92
# flow ~ design + comm + info
# 
#        Df Sum of Sq    RSS     AIC
# + op    1    7.5769 115.27 -63.060 ★★★
# + fb    1    7.2456 115.60 -62.558
# <none>              122.85 -53.919
# 
# Step:  AIC=-63.06
# flow ~ design + comm + info + op
# 
#        Df Sum of Sq    RSS     AIC
# + fb    1    7.2422 108.03 -72.415 ★★★
# <none>              115.27 -63.060
# 
# Step:  AIC=-72.42
# flow ~ design + comm + info + op + fb


# ---------------------------
# - 4. stepwise : backword 와 forword 동기



