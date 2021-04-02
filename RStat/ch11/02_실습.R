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
# 통계분석 > 관계검정 > 회귀분석 >  단순회귀분석      - 1:1종속(연속) [Linear Regression]



# 01. 데이터 불러오기  ---------------------------------------------------------

df_data = read.csv("Ch1102.단순 선형회귀분석(REG).csv",
                   header = TRUE,
                   na.strings = ".")

df_data
str(df_data)

summary(df_data)

# 02. 기술통계 확인  -----------------------------------------------------------
library(psych)

describe(df_data)

#        vars  n   mean    sd median trimmed   mad min max range  skew kurtosis   se
# weight    1 30  62.70 11.52     63   62.46 13.34  43  88    45  0.16    -0.92 2.10
# height    2 30 170.33  8.68    170  170.42  8.90 148 188    40 -0.13    -0.18 1.59

########################
# 이상치 제거 하기 #####
# 61 198.5 423.0
# 62 257.4 508.5

# df_data = df_data[c(-61:-62),]   # ★ 제거 전후 확인

########################

pairs.panels(df_data)

plot(fat ~ col, data = df_data )
abline(lm(fat ~ col, data = df_data), col="red", lty=4)


# 03. 단순회귀분석 확인  -----------------------------------------------------------

model = lm(fat ~ col, data = df_data)

model    # fat = -218.195 + (1.959 * col)

# Coefficients:
# (Intercept)     col  
# -218.195        1.959  

anova(model)

# Analysis of Variance Table
# 
# Response: fat
#           Df Sum Sq Mean Sq F value    Pr(>F)    
# col        1 184334  184334  49.938 1.925e-09 ***
# Residuals 60 221476    3691    


summary(model)

# Call:
#   lm(formula = fat ~ col, data = df_data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -88.546 -30.591  -4.843  25.235 252.243 
# 
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -218.1953    48.0773  -4.538 2.78e-05 ***
# col            1.9595     0.2773   7.067 1.93e-09 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 60.76 on 60 degrees of freedom
# Multiple R-squared:  0.4542,	Adjusted R-squared:  0.4451 
# F-statistic: 49.94 on 1 and 60 DF,  p-value: 1.925e-09

# 03. 회귀분석 가정 검정  ------------------------------------------------------
# - 1. 등분산성 : Scale-Location, ncvTest
# - 2. 정규성 : Nomal Q-Q, shapiro.test
# - 3. 선형성 : Residuals vs Fitted
# - 4. 독립성 : durbinWatsonTest
# - 5. 이상치검정 : Residuals vs Leverage(cook's distrance)

opar = par(no.readonly = TRUE)

par(mfrow=c(2,2))
plot(model)

par(opar)

# 수치로 가정 검정
library(car)

# 잔차의 등분산성 검정
car::ncvTest(model)

# Non-constant Variance Score Test 
# Variance formula: ~ fitted.values 
# Chisquare = 33.2511, Df = 1, p = 8.0994e-09  ★★★★★ 등분산 아님


# 잔차의 정규분포 검정
shapiro.test(model$residuals)

#       Shapiro-Wilk normality test
# 
# data:  model$residuals
# W = 0.84782, p-value = 1.921e-06   ★★★★★ 정규분로 아님

# 이상치 검정, sd, hat, d 통합검정
influencePlot(model, id.method="identify")

#      StudRes        Hat      CookD
# 15 0.8628319 0.09811898 0.04067052
# 61 4.9889620 0.03172057 0.29159034 ★★★
# 62 4.6626699 0.17111158 1.66756404 ★★★



# 04. 예측 확인  ---------------------------------------------------------------
predict_data = data.frame(col=c(130, 150))

predict(model, newdata = predict_data)


