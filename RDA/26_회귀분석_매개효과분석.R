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

# 매개효과분석 -> 간접효과(indirect effect)
# - 첫 번째 변수 X가 두 번째 변수 M에 영향을 미치고, 이어서 차례로
#   그 변수가 다시 세 번째 변수 Y에 영향을 미치는 연쇄적인 영향관계

# 소벨(Sobel) 검정
# 부

################################################################################

# 데이터 수집                                    -------------------------------
View(mtcars)
str(mtcars)



# 데이터 전처리                                  -------------------------------



# 기술통계                                       -------------------------------
library(psych)
psych::describe(mtcars)

summary(mtcars)


# 데이터 분석                                    -------------------------------
model_total = lm(mpg ~ disp, data=mtcars)
summary(model_total)
str(model_total)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.8922 -2.2022 -0.9631  1.6272  7.2305 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 29.599855   1.229720  24.070  < 2e-16 ***
# disp        -0.041215   0.004712  -8.747 9.38e-10 ***

plot(mtcars$mpg ~ mtcars$disp)
abline(model_total)




model_M = lm(wt ~ disp, data=mtcars)
summary(model_M)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.89044 -0.29775 -0.00684  0.33428  0.66525 
# 
# Coefficients:
#         Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 1.5998146  0.1729964   9.248 2.74e-10 ***
# disp        0.0070103  0.0006629  10.576 1.22e-11 ***

plot(mtcars$wt ~ mtcars$disp)
abline(model_M)


model_Y = lm(mpg ~ disp + wt, data=mtcars)
summary(model_Y)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.4087 -2.3243 -0.7683  1.7721  6.3484 
# 
# Coefficients:
#        Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 34.96055    2.16454  16.151 4.91e-16 ***
# disp        -0.01773    0.00919  -1.929  0.06362 .  
# wt          -3.35082    1.16413  -2.878  0.00743 ** 


# mpg ~ disp 
#   disp : 9.38e-10 / -0.041215 
#   
# wt ~ disp  
#   disp : 1.22e-11 / 0.0070103
#   
# mpg ~ disp + wt   => wt로 disp가 영향을 받음
#   disp : 0.06362 / -0.01773
#   wt   : 0.00743 / -3.35082 

# 간접 효과 산정 (disp가 mpg의 간접효과)  ★★★
0.0070103 * (-3.35082 ) # -0.02349025



# 모델 검정 하기                                 -------------------------------
# install.packages("multilevel")
library(multilevel)

model_sob = multilevel::sobel(pred=mtcars$disp, med=mtcars$wt, out=mtcars$mpg)
model_sob

# $z.value
# [1] -2.777371
pnorm(abs(model_sob$z.value), lower.tail=FALSE) # 0.002740032
pnorm(abs(model_sob$z.value), lower.tail=FALSE) * 2 # 0.005480063


?multilevel::sobel # {multilevel} Estimate Sobel's (1982) Test for Mediation

# sobel(pred,med,out)


# install.packages("bda")
library(bda)

bda::mediation.test(mv=mtcars$wt, iv=mtcars$disp, dv=mtcars$mpg)

#                Sobel      Aroian      Goodman
# z.value -2.777370744 -2.76588301 -2.789002812
# p.value  0.005480063  0.00567689  0.005287061

#install.packages("mediation")
library(mediation)


model_M = lm(wt ~ disp, data=mtcars)
model_Y = lm(mpg ~ disp + wt, data=mtcars)

set.seed(123)
model_mediation = mediation::mediate(model.m = model_M,
                                     model.y = model_Y,
                                     treat = "disp",
                                     mediator = "wt",
                                     boot = TRUE,
                                     sims = 500)

summary(model_mediation)

plot(model_mediation)














