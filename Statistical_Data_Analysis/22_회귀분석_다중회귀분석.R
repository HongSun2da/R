## 회귀분석(regression analysis)                  ##############################
# 
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

# 회귀모델 검정
# anova()                  : 분산분석표
# coefficients() / coef()  : 회귀계수
# confint()                : 회귀계수에 대한 신뢰구간
# fitted()                 : 회귀식에 의한 예측값
# residuals() / resid()    : 잔차
# summary()                : 주요 분석 정보(잔차, 회귀계수, R2, F값 등)

################################################################################

# - 4. 다중회귀분석(multiple regression analysis)

# 데이터 수집                                    -------------------------------
View(mtcars)
str(mtcars)



# 데이터 전치리                                  -------------------------------
mtcars = mtcars[c("mpg","hp","wt","disp","drat")]



# 기술 통계                                      -------------------------------
library(psych)

psych::describe(mtcars)
psych::pairs.panels(mtcars)

cor(mtcars)



# 데이터 분석                                    -------------------------------
library(car)
car::scatterplotMatrix(mtcars)

car::scatterplotMatrix(mtcars,
                       pch=19,
                       col="royalblue",
                       cex=1.2,
                       regLine=list(method=lm, lty=1, lwd=3, col="green"),
                       smooth=list(smoother=loessLine, spread=FALSE, lty.smooth=1, lwd.smooth=3, col.smooth="red"))


mtcars_lm = lm(mpg ~ hp + wt + disp + drat, data=mtcars)
summary(mtcars_lm)




# 데이터 분석(표준화)                            -------------------------------

mtcars_lm = lm(scale(mpg) ~ scale(hp) + scale(wt) + scale(disp) + scale(drat), data=mtcars)
summary(mtcars_lm)

install.packages("QuantPsyc")
library(QuantPsyc)

mtcars_lm = lm(mpg ~ hp + wt + disp + drat, data=mtcars)
               
QuantPsyc::lm.beta(mtcars_lm) # 표준화한 상관계수 확인
















