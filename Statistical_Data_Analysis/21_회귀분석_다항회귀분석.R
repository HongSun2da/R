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

# - 3. 다항회귀분석(polynomial regression analysis)

# 데이터 수집                                    -------------------------------
library(car)
View(Prestige)

str(Prestige)
class(Prestige)



# 데이터 전치리                                  -------------------------------



# 기술 통계                                      -------------------------------
library(psych)
psych::describe(Prestige)
psych::pairs.panels(Prestige)



# 데이터 분석                                    -------------------------------

Prestige_lm = lm(Prestige$income ~ Prestige$education)
summary(Prestige_lm)


plot(Prestige$income ~ Prestige$education,
     col="cornflowerblue",
     pch=19,
     xlab="Education(years)",
     ylab="Income (dollars)",
     main="Income and Education")
abline(Prestige_lm,
       col="salmon",
       lwd=2)


lm(income ~ education, data=Prestige,
   subset = (education > 10.73804))   # 평균 10.73804 기준으로 앞, 뒷가 차이가 많이남

lm(income ~ education, data=Prestige,
   subset = (education <= 10.73804))



# 데이터 분석2 (곡선 확인)                      -------------------------------

car::scatterplot(income ~ education, data=Prestige,
                 pch=19,
                 cex=1.2,
                 col="orangered",
                 regLine=list(method=lm, lty=2, lwd=3, col="blue"),
                 smooth=list(method=loessLine, spread=FALSE, lty.smooth=1, lwd.smooth=3, col.smooth="red"),
                 xlab="Education (years)",
                 ylab="Income (dollars)",
                 main="Education and Income"
                 )

?car::scatterplot #  {car} Enhanced Scatterplots with Marginal Boxplots, Point Marking, Smoothers, and More

# ## S3 method for class 'formula'
# scatterplot(formula, data, subset, xlab, ylab, id=FALSE,
#             legend=TRUE, ...)
# 
# ## Default S3 method:
# scatterplot(x, y, boxplots=if (by.groups) "" else "xy",
#             regLine=TRUE, legend=TRUE, id=FALSE, ellipse=FALSE, grid=TRUE,
#             smooth=TRUE,
#             groups, by.groups=!missing(groups),
#             xlab=deparse(substitute(x)), ylab=deparse(substitute(y)),
#             log="", jitter=list(), cex=par("cex"),
#             col=carPalette()[-1], pch=1:n.groups,
#             reset.par=TRUE, ...)
# 
# sp(x, ...)



# 다항 회귀 분석                             -----------------------------------
Prestige_lm2 = lm(income ~ education + I(education^2), data=Prestige)
summary(Prestige_lm2)


plot(Prestige$income ~ Prestige$education,
     col="cornflowerblue",
     pch=19,
     xlab="Education(years)",
     ylab="Income (dollars)",
     main="Income and Education")
lines(Prestige$education, fitted(Prestige_lm2))


# 모델 비교
summary(Prestige_lm)
summary(Prestige_lm2)




# 데이터 분석                                    -------------------------------

View(faithful)
str(faithful)

car::scatterplot(eruptions ~ waiting  , data=faithful,
                 pch=19,
                 cex=1.2,
                 col="deepskyblue",
                 regLine=list(method=lm, lty=2, lwd=3, col="blue"),
                 smooth=list(method=loessLine, spread=FALSE, lty.smooth=1, lwd.smooth=3, col.smooth="red"),
                 xlab="Waiting",
                 ylab="Eruptions",
                 main="Eruptions and Waiting")

# 3차 다항식
faithful_lm3 = lm(eruptions ~ waiting + I(waiting^2) + I(waiting^3), data=faithful)
summary(faithful_lm3)



plot(faithful$eruptions ~ faithful$waiting,
     col="cornflowerblue",
     pch=19,
     xlab="Education(years)",
     ylab="Income (dollars)",
     main="Income and Education")

library(dplyr)

df = data.frame(faithful$waiting, fitted(faithful_lm3))

lines(arrange(sort(df))































