## 회귀분석(regression analysis)                  ##############################

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

# - 2. 단순회귀분석(simple regression analysis)

# 데이터 수집                                    -------------------------------
library(car)
View(Prestige)

str(Prestige)
class(Prestige)



# 데이터 전처리                                  -------------------------------



# 기술통계                                       -------------------------------
summary(Prestige)

library(psych)
describe(Prestige)

pairs.panels(Prestige)



# 데이터 분석                                    -------------------------------
Prestige_lm = lm(income ~ education, data = Prestige)
summary(Prestige_lm)


# 회귀모델 검정
# anova()                  : 분산분석표
# coefficients() / coef()  : 회귀계수
# confint()                : 회귀계수에 대한 신뢰구간
# fitted()                 : 회귀식에 의한 예측값
# residuals() / resid()    : 잔차
# summary()                : 주요 분석 정보(잔차, 회귀계수, R2, F값 등)


coef(summary(Prestige_lm))

anova(Prestige_lm)

confint(Prestige_lm)
confint(Prestige_lm, level = 0.99)


fitted(Prestige_lm)[1:3]
Prestige$income[1:3]

residuals(Prestige_lm)[1:3]


# 예측
Prestige_new = data.frame(education=c(5,10,15))
Prestige_new


predict(Prestige_lm, newdata = Prestige_new)

predict(Prestige_lm, newdata = Prestige_new, interval="confidence")




?predict # predict {arules} = Model Predictions

# ## S4 method for signature 'itemMatrix'
# predict(object, newdata, labels = NULL, blocksize = 200,...)



# 
mean(Prestige$education) # 교육 기간 평균(10.73804)보다 많은 사람과 작은 사람의 비교

lm(income ~ education, data=Prestige,
   subset = (education > 10.73804))

lm(income ~ education, data=Prestige,
   subset = (education <= 10.73804))





# 그래프 확인                                    -------------------------------
plot(Prestige$income ~ Prestige$education,
     col="cornflowerblue",
     pch=19,
     xlab="Education(years)",
     ylab="Income (dollars)",
     main="Income and Education")
abline(Prestige_lm,
       col="salmon",
       lwd=2)



