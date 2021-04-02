## 일반선형모델 한계             ###############################################
# 1. 이항로지스틱회귀분석 (binomial logistic regression analysis)
# 2. 페널티로지스틱회귀분석 (penalized logistic regressiion analysis)
# 3. 다항로지스틱회귀분석 (multinomial logistic regressing analysis)
# 4. 포아송회귀분석 (Poisson regression analysis)


################################################################################
# 4. 포아송회귀분석 (Poisson regression analysis)
#   -> 종속변수가 특정 기간 동안의 사건발생횟수인 경우에 적용

################################################################################

# 데이터 수집                                    -------------------------------
#install.packages("robust")
library(robust)

data("breslow.dat")
class(breslow.dat)
View(breslow.dat)

str(breslow.dat) # 'data.frame':	59 obs. of  12 variables:

# 기술 통계      ---------------------------------------------------------------
library(psych)

psych::describe(breslow.dat)

psych::describeBy(breslow.dat, breslow.dat$Trt)

psych::pairs.panels(breslow.dat)


# 데이터 전처리  ---------------------------------------------------------------
data = breslow.dat[c("Base", "Age", "Trt", "sumY")]

str(data) # 'data.frame':	59 obs. of  4 variables:
summary(data)

psych::pairs.panels(data)




# 데이터 분석    ---------------------------------------------------------------
model = glm(sumY ~ Base + Age + Trt,
            data=data,
            family=poisson())
summary(model)


# 일탈도는 카이제곱분포를 사용
pchisq(q=2122.73-559.44,
       df=58-55,
       lower.tail = FALSE)  # [1] 0

# 회귀 계수 확인
exp(coef(model))

# (Intercept)         Base          Age Trtprogabide 
#   7.0204403    1.0229102    1.0230007    0.8583864 


# 과산포 확인
#install.packages("qcc")
library(qcc)

qcc::qcc.overdispersion.test(data$sumY, 
                             type="poisson")

# Overdispersion test Obs.Var/Theor.Var Statistic p-value
#        poisson data          62.87013  3646.468       0 (귀무가설 과산포가 아니다를 기각 한다.)


model_quasi = glm(sumY ~ Base + Age + Trt,
                  data=data,
                  family=quasipoisson())
summary(model_quasi)


################################################################################

library(MASS)

str(ships)
View(ships)
summary(ships)

psych::describe(ships)
psych::pairs.panels(ships)





















