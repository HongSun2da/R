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

# 가정, 진단, 모델 수정

# 데이터 수집                                    -------------------------------

View(mtcars)
str(mtcars)


# 데이터 전처리                                  -------------------------------


# 기술통계                                       -------------------------------


# 데이터 분석                                    -------------------------------
mtcars_lm = lm(mpg ~ hp + wt + disp + drat, data=mtcars)

plot(mtcars_lm)
# - Residuals vs Fitted    -> 선형성 가정   (y:잔차, x:예측)
# - Normal Q-Q             -> 정규성 가정   (y:잔차, x:정규분포 잔차)
# - Scale-Location         -> 등분산성 가정 (y:표준화잔차, x:예측)
# - Residuals vs Leverage  -> 이상치 가정   (y:이상치, x:예측이상치)



# 데이터 분석(다중공선성)                        -------------------------------
library(car)
car::vif(mtcars_lm)

#       hp       wt     disp     drat 
# 2.894373 5.096601 8.209402 2.279547 













