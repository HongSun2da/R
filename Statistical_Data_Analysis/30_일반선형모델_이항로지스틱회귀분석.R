## 일반선형모델 한계             ###############################################
# 1. 이항로지스틱회귀분석 (binomial logistic regression analysis)
# 2. 페널티로지스틱회귀분석
# 3. 다항로지스틱회귀분석
# 4. 포아송회귀분석


################################################################################
# 1. 이항로지스틱회귀분석 (binomial logistic regression analysis)
#   -> 종속변수가 이분형 범주를 가질 때 독립변수로부터 종속변수의 범주를 예측
#   -> 0과 1사이 확률값 (기준값 0.5)
#   -> 오즈비(odds ratio):독립변수 한 단위의 증가에 따른 오즈의 변화 비

################################################################################

# 데이터 수집                                    -------------------------------
install.packages("modeldata")
library(modeldata)
data(mlc_churn)
str(mlc_churn)

View(mlc_churn)



# 데이터 전처리                                  -------------------------------
data = mlc_churn
class(data)
str(data)
head(data)

data = data[-c(1,3)] # state, area_code 제외
data$churn = factor(ifelse(data$churn=="no", 1, 2),
                    level=c(1,2),
                    labels=c("no", "yes"))

data_train = data[1:3333,]
data_test = data[3334:5000,]

# - 비율 비교
table(data_train$churn) # 2850  483 
prop.table(table(data_train$churn)) # 0.8550855 0.1449145 

table(data_test$churn) # 1443  224 
prop.table(table(data_test$churn)) # 0.8656269 0.1343731 



# 기술 통계                                      -------------------------------
library(psych)
psych::describeBy(data_train, group=data_train$churn)

psych::pairs.panels(data_train)



# 데이터 분석                                   --------------------------------

#- 모델 생성
table(data_train$international_plan)
table(data_train$voice_mail_plan)

model = glm(churn ~ .,
            data=data_train,
            family=binomial(link="logit"))
summary(model)

odds = exp(coef(model))

cbind(model$coefficients, odds)

#     Null deviance: 2758.3  on 3332  degrees of freedom
# Residual deviance: 2158.7  on 3315  degrees of freedom
# 이탈도 확인
pchisq(q=(2758.3-2158.7),
       df=(3332-3315),
       lower.tail = FALSE)  # 1.731898e-116


#- 예측 확인
model_pred = predict(model,
                      newdata = data_test,
                      type="response")

head(cbind(model_pred, data_test$churn))

model_pred_logit = factor(model_pred > 0.5,
                          levels=c(FALSE, TRUE),
                          labels=c("no", "yes"))

cbind(model_pred, model_pred_logit, data_test$churn)

table(data_test$churn, 
      model_pred_logit,
      dnn=c("Actual","Predicted"))

#         Predicted
# Actual    no  yes
#     no  1414   29
#     yes  181   43

#- 예측 정확도
mean(data_test$churn == model_pred_logit) # 0.8740252



# 모델 성능 향상                               ---------------------------------

model_setp = step(model)
summary(model_setp)

#- 예측 확인
model_pred = predict(model_setp,
                     newdata = data_test,
                     type="response")

model_pred_logit = factor(model_pred > 0.5,
                          levels=c(FALSE, TRUE),
                          labels=c("no", "yes"))

cbind(model_pred, model_pred_logit, data_test$churn)

table(data_test$churn, 
      model_pred_logit,
      dnn=c("Actual","Predicted"))
      
#       Predicted
# Actual   no  yes
# no    1408   35
# yes   182    42

#- 예측 정확도
mean(data_test$churn == model_pred_logit) # 0.869826




# 모델 분석 (독립변수 변화 확인)          --------------------------------------
table(data_test$number_customer_service_calls)

#   0   1   2   3   4   5   6   7 
# 326 605 368 236  86  30  12   4 

data_new1 = data.frame(
                      number_customer_service_calls=c(0:7),
                      international_plan="no",
                      voice_mail_plan="no",
                      number_vmail_messages=mean(data_test$number_vmail_messages),
                      total_day_charge=mean(data_test$total_day_charge),
                      total_eve_minutes=mean(data_test$total_eve_minutes),
                      total_night_charge=mean(data_test$total_night_charge),
                      total_intl_calls=mean(data_test$total_intl_calls),
                      total_intl_charge=mean(data_test$total_intl_charge)
                      )
data_new1
data_new1$prob = predict(model_setp,
                         newdata=data_new1,
                         type="response")

# 고객센터 콜수에 따른 변화값 
data_new1[c("number_customer_service_calls", "prob")]



# 과산표 문제                             --------------------------------------
deviance(model_setp) / df.residual(model_setp) # 기준은 1 => 0.6505038







