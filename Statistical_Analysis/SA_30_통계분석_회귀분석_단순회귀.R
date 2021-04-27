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
# 통계분석 > 차이검정 > 회귀분석(Regression) > 단순회귀분석      - 1:1종속(연속) [Linear Regression]
#-------------------------------------------------------------------------------


# 1. -------------------------------
# 데이터 수집
train = read.csv("./data/line_reg.csv",
                 header=TRUE,
                 na.strings = ".")

# 데이터 이해
View(train)
str(train)   # 'data.frame':	60 obs. of  3 variables:


# 기술통계
#install.packages("psych")
library(psych)
psych::describe(train)

#     vars  n   mean    sd median trimmed   mad   min   max range skew kurtosis    se
# col    1 62 171.14 28.05 169.70  170.71 26.76 108.4 257.4 149.0 0.26     0.43  3.56
# fat    2 62 117.15 81.56 102.25  104.29 56.49  40.9 508.5 467.6 2.62     9.05 10.36

# 그래프 확인(상관 관계)
psych::pairs.panels(train)



# 단순회귀분석
model = lm(fat ~ col, data = train)

model    # fat = -218.195 + (1.959 * col)

# F Test
anova(model)

# Analysis of Variance Table
# 
# Response: fat
#           Df Sum Sq Mean Sq F value    Pr(>F)    
# col        1 184334  184334  49.938 1.925e-09 ***
# Residuals 60 221476    3691   

summary(model)

# Call:
#   lm(formula = fat ~ col, data = train)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -88.546 -30.591  -4.843  25.235 252.243 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -218.1953    48.0773  -4.538 2.78e-05 ***
# col            1.9595     0.2773   7.067 1.93e-09 ***
# 
# Residual standard error: 60.76 on 60 degrees of freedom
# Multiple R-squared:  0.4542,	Adjusted R-squared:  0.4451 
# F-statistic: 49.94 on 1 and 60 DF,  p-value: 1.925e-09



# 회귀분석 가정 검정(잔차)
# - 1. 등분산성 : Scale-Location, ncvTest
# - 2. 정규성   : Nomal Q-Q, shapiro.test
# - 3. 선형성   : Residuals vs Fitted
# - 4. 독립성   : durbinWatsonTest
# - 5. 이상치검정 : Residuals vs Leverage(cook's distrance)

plot(model)


# - 1. 등분산성 : Scale-Location, ncvTest
# install.packages("car")
library(car)
car::ncvTest(model)

# Non-constant Variance Score Test 
# Variance formula: ~ fitted.values 
# Chisquare = 33.2511, Df = 1, p = 8.0994e-09  ★★★★★ 등분산 아님


# - 2. 정규성   : Nomal Q-Q, shapiro.test
shapiro.test(model$residuals)

#       Shapiro-Wilk normality test
# 
# data:  model$residuals
# W = 0.84782, p-value = 1.921e-06   ★★★★★ 정규분로 아님

# 이상치 검정, sd, hat, d 통합검정
car::influencePlot(model, id.method="identify")

#      StudRes        Hat      CookD
# 15 0.8628319 0.09811898 0.04067052
# 61 4.9889620 0.03172057 0.29159034 ★★★
# 62 4.6626699 0.17111158 1.66756404 ★★★







# 2. -------------------------------
# 이상치 값 제거 후 
train = train[c(-61:-62),]   # ★ 제거 전후 확인

model = lm(fat ~ col, data = train)

model    # fat = -110.819 + (1.278 * col)

# F Test
anova(model)

# Analysis of Variance Table
# 
# Response: fat
#           Df Sum Sq Mean Sq F value    Pr(>F)    
# col        1  64716   64716    43.5 1.396e-08 ***
# Residuals 58  86288    1488  

summary(model)

# Call:
#   lm(formula = fat ~ col, data = train)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -72.687 -31.786  -1.914  24.148  91.905 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -110.8193    33.1779  -3.340  0.00147 ** 
# col            1.2783     0.1938   6.595  1.4e-08 ***
# 
# Residual standard error: 38.57 on 58 degrees of freedom
# Multiple R-squared:  0.4286,	Adjusted R-squared:  0.4187 
# F-statistic:  43.5 on 1 and 58 DF,  p-value: 1.396e-08


# - 1. 등분산성 : Scale-Location, ncvTest
# install.packages("car")
library(car)
car::ncvTest(model)

# Non-constant Variance Score Test 
# Variance formula: ~ fitted.values 
# Chisquare = 2.414986, Df = 1, p = 0.12018 ★★★★★ 등분산 아만족


# - 2. 정규성   : Nomal Q-Q, shapiro.test
shapiro.test(model$residuals)

#       Shapiro-Wilk normality test
# 
# data:  model$residuals
# W = 0.9828, p-value = 0.5577   ★★★★★ 정규분로포 만족아님

# 이상치 검정, sd, hat, d 통합검정
car::influencePlot(model, id.method="identify")

#      StudRes        Hat      CookD
# 15 0.8628319 0.09811898 0.04067052
# 61 4.9889620 0.03172057 0.29159034 ★★★
# 62 4.6626699 0.17111158 1.66756404 ★★★




# 04. 예측 확인  ---------------------------------------------------------------
predict_data = data.frame(col=c(130, 150))

predict(model, newdata = predict_data)





