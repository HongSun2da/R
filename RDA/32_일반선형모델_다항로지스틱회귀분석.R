## 일반선형모델 한계             ###############################################
# 1. 이항로지스틱회귀분석 (binomial logistic regression analysis)
# 2. 페널티로지스틱회귀분석 (penalized logistic regressiion analysis)
# 3. 다항로지스틱회귀분석 (multinomial logistic regressing analysis)
# 4. 포아송회귀분석


################################################################################
# 3. 다항로지스틱회귀분석 (multinomial logistic regressing analysis)
#   -> 세 게 이상의 사건(범주형 독립변수) 발생 확률

################################################################################

# 데이터 수집                                    -------------------------------
# install.packages("EffectStars")
library(EffectStars)

data(PID)
View(PID)

class(PID)
str(PID)  # 'data.frame':	944 obs. of  6 variables:

table(PID$PID) # 세 게 이상의 사건(범주형 독립변수) 발생 확률
# Democrat Independent  Republican 
# 380         239         325 

# 기술 통계      ---------------------------------------------------------------
library(psych)
psych::describe(PID)
psych::pairs.panels(PID)




# 데이터 전처리  ---------------------------------------------------------------
table(PID$PID)




# 데이터 분석    ---------------------------------------------------------------
library(VGAM)
model = VGAM::vglm(PID ~ ., 
                   data=PID,
                   family=multinomial())
summary(model)

# PID$PID$Republican 기준으로 분석
# Democrat Independent  Republican 
# 380         239         325 

# odds 확인
exp(coef(model))

cbind(model@coefficients, exp(coef(model)))

# 모델로 예측한 결과값 확인
model_fit = fitted(model)
head(model_fit)



# new test set 만들기
test_dt = data.frame(
                      TVnews = mean(PID$TVnews),
                      PID = mean(PID$TVnews),
                      Income = mean(PID$TVnews),
                      Education = c("low","high"),
                      Age = mean(PID$TVnews),
                      Population = mean(PID$TVnews)
                    )
#    Democrat Independent Republican
# 1 0.6245855   0.2059226  0.1694919
# 2 0.5983472   0.1849573  0.2166956

model_pred = predict(model, newdata=test_dt, type="response")

#     TVnews      PID   Income Education      Age Population
# 1 3.727754 3.727754 3.727754       low 3.727754   3.727754
# 2 3.727754 3.727754 3.727754      high 3.727754   3.727754

cbind(test_dt, model_pred)


test_dt = data.frame(
                    TVnews = mean(PID$TVnews),
                    PID = mean(PID$TVnews),
                    Income = seq(20, 100, 20),
                    Education = rep("low", 5),
                    Age = mean(PID$TVnews),
                    Population = mean(PID$TVnews)
                  )

model_pred = predict(model, newdata=test_dt, type="response")

cbind(test_dt, model_pred)
#     TVnews      PID Income Education      Age Population  Democrat Independent Republican
# 1 3.727754 3.727754     20       low 3.727754   3.727754 0.5601984   0.2408117  0.1989899
# 2 3.727754 3.727754     40       low 3.727754   3.727754 0.4784347   0.2849561  0.2366092
# 3 3.727754 3.727754     60       low 3.727754   3.727754 0.3978090   0.3282839  0.2739071
# 4 3.727754 3.727754     80       low 3.727754   3.727754 0.3223711   0.3685961  0.3090328
# 5 3.727754 3.727754    100       low 3.727754   3.727754 0.2551757   0.4042533  0.3405709



################################################################################

# 데이터 수집                                    -------------------------------
library(MASS)
class(fgl)
str(fgl)  # 'data.frame':	214 obs. of  10 variables:

View(fgl)




# 기술 통계      ---------------------------------------------------------------

table(fgl$type)
# WinF WinNF   Veh   Con  Tabl  Head 
#   70    76    17    13     9    29 

psych::describe(fgl)
psych::pairs.panels(fgl)

psych::describeBy(fgl, group=fgl$type)




# 데이터 전처리  ---------------------------------------------------------------
data = cbind(scale(fgl[1:9]), fgl[10])

set.seed(123)
data_idx = sample(nrow(fgl), 0.7*nrow(fgl))

data_train = data[data_idx,]
data_test = data[-data_idx,]

# 분포 비율 확인
table(data_train$type)
prop.table(table(data_train$type))

table(data_test$type)
prop.table(table(data_test$type))




# 데이터 분석    ---------------------------------------------------------------
library(nnet)

model = nnet::multinom(type ~ .,
                       data=data_train)
summary(model)

model_pred = predict(model,
                     newdata=data_test,
                     type="probs")

cbind(round(model_pred, 3), data_test["type"])

# 예측값 확인
max.col(model_pred)

colnames(model_pred)[max.col(model_pred)]

cbind(pred=colnames(model_pred)[max.col(model_pred)], data_test["type"])

table(cbind(pred=colnames(model_pred)[max.col(model_pred)]
            , data_test["type"]))


# 정확도 0.6307692
mean(colnames(model_pred)[max.col(model_pred)] == data_test["type"])



################################################################################
# Cross Val

model_cv = numeric()
for (i in 1:100){
  data_idx = sample(nrow(fgl), 0.7*nrow(fgl))
  data_train = data[data_idx,]
  data_test = data[-data_idx,]
  
  model = nnet::multinom(type ~ .,
                         data=data_train)
  model_pred = predict(model,
                       newdata=data_test,
                       type="probs")
  model_cv[i] = mean(colnames(model_pred)[max.col(model_pred)] == data_test["type"])
}

boxplot(model_cv)
plot(model_cv)







