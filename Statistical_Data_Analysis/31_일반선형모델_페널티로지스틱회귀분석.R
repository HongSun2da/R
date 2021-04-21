## 일반선형모델 한계             ###############################################
# 1. 이항로지스틱회귀분석 (binomial logistic regression analysis)
# 2. 페널티로지스틱회귀분석 (penalized logistic regressiion analysis)
# 3. 다항로지스틱회귀분석
# 4. 포아송회귀분석


################################################################################
# 2. 페널티로지스틱회귀분석 (penalized logistic regressiion analysis)
#   -> 

################################################################################

# 데이터 수집                                    -------------------------------
# install.packages("mlbench")
library(mlbench)

data("PimaIndiansDiabetes2")
View(PimaIndiansDiabetes2) 

str(PimaIndiansDiabetes2) # 'data.frame':	768 obs. of  9 variables:





# 기술 통계                                      -------------------------------
summary(PimaIndiansDiabetes2)
library(psych)

psych::describe(PimaIndiansDiabetes2)
psych::pairs.panels(PimaIndiansDiabetes2)




# 데이터 전처리                                  -------------------------------

#-------------------------------------------------------------------------------
?na.omit # {stats} Handle Missing Values in Objects
# na.fail(object, ...)
# na.omit(object, ...)
# na.exclude(object, ...)
# na.pass(object, ...)
#-------------------------------------------------------------------------------

data = na.omit(PimaIndiansDiabetes2) # 'data.frame':	392 obs. of  9 variables:
str(data)

psych::describe(data)
psych::pairs.panels(data)



# - 데이터 분리(훈련, test)
library(caret)

#-------------------------------------------------------------------------------
?caret::createDataPartition # {caret} Data Splitting functions
# createDataPartition(
#                     y,
#                     times = 1,
#                     p = 0.5,
#                     list = TRUE,
#                     groups = min(5, length(y))
#                   )
#-------------------------------------------------------------------------------
set.seed(123)
data_idx = caret::createDataPartition(y=data$diabetes,
                                      p=0.7,
                                      list=FALSE)
data_idx
data_train = data[data_idx, ]
data_test = data[-data_idx, ]

table(data_train$diabetes)
prop.table(table(data_train$diabetes))

table(data_test$diabetes)
prop.table(table(data_test$diabetes))


# 종속변수 데이터 분리 작업
data_train_x = model.matrix(diabetes ~ ., data_train)
data_train_x = model.matrix(diabetes ~ ., data_train)[, -1]
data_train_y = ifelse(data_train$diabetes=="pos", 1, 0)

data_test_x = model.matrix(diabetes ~ ., data_test)
data_test_x = model.matrix(diabetes ~ ., data_test)[, -1]
data_test_y = ifelse(data_test$diabetes=="pos", 1, 0)




# 데이터 분석   lambda                          --------------------------------

# lambda.min 찾기
library(glmnet)
#-------------------------------------------------------------------------------
?cv.glmnet # {glmnet} Cross-validation for glmnet
# cv.glmnet(
#           x,
#           y,
#           weights = NULL,
#           offset = NULL,
#           lambda = NULL,
#           type.measure = c("default", "mse", "deviance", "class", "auc", "mae", "C"),
#           nfolds = 10,
#           foldid = NULL,
#           alignment = c("lambda", "fraction"),
#           grouped = TRUE,
#           keep = FALSE,
#           parallel = FALSE,
#           gamma = c(0, 0.25, 0.5, 0.75, 1),
#           relax = FALSE,
#           trace.it = 0,
#           ...
#         )
#-------------------------------------------------------------------------------
set.seed(123)
model_cv = cv.glmnet(x=data_train_x,
                      y=data_train_y,
                      family="binomial",
                      alpha=1) # lasso

model_cv$lambda.min # 0.01578334
model_cv$lambda.1se # 0.05289948

coef(model_cv, model_cv$lambda.min)
coef(model_cv, model_cv$lambda.1se)


# - lambda.min
# 모델 생성 [lasso]
model = glmnet(x=data_train_x,
                y=data_train_y,
                family="binomial",
                alpha=1,
                lambda=model_cv$lambda.min) 
# 예측
model_pred = predict(model, data_test_x)

model_pred = ifelse(model_pred > 0.5, "pos", "neg")

# 오분류
table(model_pred, data_test$diabetes,
      dnn=c("Actual","Predicted"))

#         Predicted
# Actual neg pos
# neg     72  24
# pos      6  15

# 정확도
mean(model_pred == data_test$diabetes) #  0.7435897 (정확도)



# - lambda.1se
# 모델 생성 [lasso]
model1 = glmnet(x=data_train_x,
               y=data_train_y,
               family="binomial",
               alpha=1,
               lambda=model_cv$lambda.1se) 
# 예측
model_pred1 = predict(model1, data_test_x)

model_pred1 = ifelse(model_pred1 > 0.5, "pos", "neg")

# 오분류
table(model_pred1, 
      data_test$diabetes,
      dnn=c("Actual","Predicted"))

#       Predicted
# Actual neg pos
#    neg  76  30
#    pos   2   9

# 정확도
mean(model_pred1 == data_test$diabetes) #  0.7264957 (정확도)




# 데이터 분석   logit                          --------------------------------
model_logit = glm(diabetes ~ ., 
                  data=data_train,
                  family=binomial(link="logit"))
summary(model_logit)

# 예측
model_logit_pred = predict(model_logit, 
                           newdata=data_test,
                           type="response")

model_logit_pred = ifelse(model_logit_pred > 0.5, "pos", "neg")


# 오분류
table(model_logit_pred, 
      data_test$diabetes,
      dnn=c("Actual","Predicted"))

#       Predicted
# Actual neg pos
#    neg  66  20
#    pos  12  19

# 정확도
mean(model_logit_pred == data_test$diabetes) #  0.7264957 (정확도)

































