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

# 조절효과 -> 
# - 

################################################################################

# 데이터 수집                                    -------------------------------
str(mtcars)


# 데이터 전처리                                  -------------------------------



# 기술통계                                       -------------------------------



# 데이터 분석                                    -------------------------------
mtcars_lm = lm(mpg ~ hp + wt + hp:wt, data=mtcars)
summary(mtcars_lm)


# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 49.80842    3.60516  13.816 5.01e-14 ***
# hp          -0.12010    0.02470  -4.863 4.04e-05 ***
# wt          -8.21662    1.26971  -6.471 5.20e-07 ***
# hp:wt        0.02785    0.00742   3.753 0.000811 *** (hp:wt 상로작용 존재) -> mpg와 hp는 wt따른 변동 한다.

library(effects)
m = round(mean(mtcars$wt), 1)
s = round(sd(mtcars$wt), 1)
m;s

plot(
effects::effect(term="hp:wt", 
                mod=mtcars_lm, 
                xlevels=list(wt=c(m-s, m , m+s)))
)

plot(
effects::effect(term="hp:wt", 
                mod=mtcars_lm, 
                xlevels=list(wt=c(m-s, m , m+s)),
                lines=list(multiline=TRUE, lwd=2, lty=c(3,2,1), col=c("royalblue", "violet","maroon")))
)

?effects # {effects} Effect Displays for Linear, Generalized Linear, and Other Models


# install.packages("rockchalk")
library(rockchalk)

rockchalk::plotSlopes(model=mtcars_lm,
                      plotx="hp",
                      modx="wt",
                      modxVals="std.dev.")



















