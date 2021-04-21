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

# 더미변수 -> 범주형 독립변수를 변화(원 핫 인코딩)

################################################################################

# 데이터 수집                                    -------------------------------
?InsectSprays
# A data frame with 72 observations on 2 variables.
# 
# [,1]	count	numeric	Insect count
# [,2]	spray	factor	The type of spray

View(InsectSprays)
str(InsectSprays)

levels(InsectSprays$spray) # "A" "B" "C" "D" "E" "F"


# 데이터 전처리                                  -------------------------------



# 기술통계                                       -------------------------------
summary(InsectSprays)

tapply(InsectSprays$count, InsectSprays$spray, mean)




# 데이터 분석                                    -------------------------------
spray_lm = lm(count ~ spray, data=InsectSprays)  # 범주형 독립변수를 자동으로 더미처리 함

summary(spray_lm)

# 더미 변수 확인
contrasts(InsectSprays$spray)


# 분산분석으로 비교
spray_aov = aov(count ~ spray, data=InsectSprays)
summary(spray_aov)


#         Df Sum Sq Mean Sq F value Pr(>F)    
# spray        5   2669   533.8    34.7 <2e-16 *** (귀무가설 기각)
# Residuals   66   1015    15.4    


# 사후 분석 하기
TukeyHSD(spray_aov)


# 기준 범주를 변경하기
respray = relevel(InsectSprays$spray, ref=6)
contrasts(respray)



# 다시 분석
spray_lm = lm(count ~ respray, data=InsectSprays)  # 범주형 독립변수를 자동으로 더미처리 함

summary(spray_lm)








