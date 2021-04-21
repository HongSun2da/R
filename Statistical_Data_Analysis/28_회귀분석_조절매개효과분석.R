## 회귀분석(regression analysis)                  ##############################

# R 포뮬러 심볼
# : > y ~ x + w + x:w
# * > y ~ x * w * z           -> x + w + z + x:w + x:z + w:z + x:w:z
# ^ > y ~ (x + w + x)^2       -> x + w + z + x:w + x:z + w:z
# . > y ~ .                   -> x + w + x
# - > y ~ (x + w + x)^2 - w:z -> x + w + z + x:w + x:z
# I > y ~ x + I((w + z)^2)    -> x + (w,z 합 제곱값)
#   
# - 1. 선형회귀(linear regression equation)
# - 2. 단순회귀분석(simple regression analysis)      - 직선
# - 3. 다항회귀분석(polynomial regression analysis)  - 곡선
# - 4. 다중회귀분석(multiple regression analysis)

#  회귀분석 가정
# - 선형성 
# - 정규성
# - 등분산성
# - 독립성

# 다중공선성(multicollinearity) 확인
# - VIF(variance inflation factor) > 4  (다중공선성 존재 확인 하기)
#                                  > 10 (다중공선성 존재 가능성이 높다)

# 회귀모델 검정
# anova()                  : 분산분석표
# coefficients() / coef()  : 회귀계수
# confint()                : 회귀계수에 대한 신뢰구간
# fitted()                 : 회귀식에 의한 예측값
# residuals() / resid()    : 잔차
# summary()                : 주요 분석 정보(잔차, 회귀계수, R2, F값 등)

################################################################################

# 조절매개효과 -> 
# - 

################################################################################

# 데이터 수집                                    -------------------------------
str(mtcars)


# 데이터 전처리                                  -------------------------------



# 기술통계                                       -------------------------------



# 데이터 분석                                    -------------------------------
model_M = lm(wt ~ disp * am, data=mtcars)
summary(model_M)

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  2.091e+00  2.797e-01   7.474 3.86e-08 ***
# disp         5.779e-03  9.037e-04   6.395 6.36e-07 ***
# am          -5.234e-01  3.637e-01  -1.439    0.161    
# disp:am      9.883e-05  1.665e-03   0.059    0.953   

model_Y = lm(mpg ~ disp * am + wt * am, data=mtcars)
summary(model_Y)


# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 28.8676681  3.1657012   9.119  1.4e-09 ***
# disp        -0.0173266  0.0091641  -1.891  0.06986 .  
# am          14.8852036  4.7260896   3.150  0.00408 ** 
# wt          -1.7747993  1.2987213  -1.367  0.18346    
# disp:am     -0.0008819  0.0172530  -0.051  0.95962    
# am:wt       -5.1713183  2.4403740  -2.119  0.04380 *

# install.packages("mediation")
library(mediation)

?mediation::mediate # {mediation} Causal Mediation Analysis

# mediate(model.m, model.y, sims = 1000, boot = FALSE,
#         boot.ci.type = "perc", treat = "treat.name", mediator = "med.name",
#         covariates = NULL, outcome = NULL, control = NULL,
#         conf.level = 0.95, control.value = 0, treat.value = 1,
#         long = TRUE, dropobs = FALSE, robustSE = FALSE, cluster = NULL,
#         group.out = NULL, use_speed = FALSE, ...)



set.seed(12)
model_med1 = mediation::mediate(model.m = model_M,
                                 model.y = model_Y,
                                 covariates = list(am=0),
                                 treat = "disp",
                                 mediator = "wt",
                                 boot = TRUE,
                                 sims = 500)

summary(model_med1)
#                Estimate 95% CI Lower 95% CI Upper p-value    
# ACME            -0.0103      -0.0234         0.01    0.18    (간접효과)
# ADE             -0.0173      -0.0388         0.00    0.06 .  
# Total Effect    -0.0276      -0.0372        -0.02  <2e-16 ***
# Prop. Mediated   0.3719      -0.2061         1.02    0.18   

set.seed(12)
model_med2 = mediation::mediate(model.m = model_M,
                                model.y = model_Y,
                                covariates = list(am=1),
                                treat = "disp",
                                mediator = "wt",
                                boot = TRUE,
                                sims = 500)

summary(model_med2)
#                Estimate 95% CI Lower 95% CI Upper p-value    
# ACME            -0.0408      -0.0905        -0.02   0.016 *  (간접효과)
# ADE             -0.0182      -0.1201         0.01   0.160    
# Total Effect    -0.0590      -0.1575        -0.04  <2e-16 ***
# Prop. Mediated   0.6916       0.1343         1.13   0.016 *  







set.seed(12)
model_med = mediation::mediate(model.m = model_M,
                                model.y = model_Y,
                                treat = "disp",
                                mediator = "wt",
                                boot = TRUE,
                                sims = 500)

summary(model_med)
#                Estimate 95% CI Lower 95% CI Upper p-value    
# ACME            -0.0227      -0.0448        -0.01   0.004 ** 
# ADE             -0.0177      -0.0580         0.00   0.024 *  
# Total Effect    -0.0404      -0.0801        -0.03  <2e-16 ***
# Prop. Mediated   0.5618       0.1759         0.95   0.004 ** 

?test.modmed



set.seed(12)
test.modmed(object = model_med,
            covariates.1 = list(am=0),
            covariates.2 = list(am=1),
            sims = 500)


# Test of ACME(covariates.1) - ACME(covariates.2) = 0
# 
# data:  estimates from model_med
# ACME(covariates.1) - ACME(covariates.2) = 0.030573, p-value = 0.0588
# alternative hypothesis: true ACME(covariates.1) - ACME(covariates.2) is not equal to 0
# 95 percent confidence interval:
#   -0.001384198  0.082504767
# 
# 
# Test of ADE(covariates.1) - ADE(covariates.2) = 0
# 
# data:  estimates from model_med
# ADE(covariates.1) - ADE(covariates.2) = 0.0008819, p-value = 0.92
# alternative hypothesis: true ADE(covariates.1) - ADE(covariates.2) is not equal to 0
# 95 percent confidence interval:
#   -0.03364509  0.11037050


















