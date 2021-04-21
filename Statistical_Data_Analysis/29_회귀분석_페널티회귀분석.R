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

# 페널티회귀분석 -> 
# - 너무 많은 독립변수를 사용하는 모델에 패널티를 부과하는 방식으로 보다 간명한 모델을 생성

# - 1. 릿지회귀분석(ridge regression analysis) 
#    -> 모델의 설명력에 기여하지 못하는 독립변수의 회귀계수 크기를 0에 근접하도록 축소
#    -> L2-norm 패널티항 (람다 제곱사용)

# - 2. 라소회귀분석(lasso regreesion analysis)
#    -> 모델의 설명력에 기여하지 못하는 독립변수의 회귀계수 크기를 0으로 만듦
#    -> L1-norm 패널티항 (람다 절대값 사용)

# - 3. 일래스틱넷회귀분석(elasticnet regreesion analysis)
#    -> 릿지회귀분석 + 라소회귀분석
#    -> L1-norm과 L2-norm 패널티항 모두 사용 (알파 사용, 람다 제곱 + 절대값 사용)

################################################################################

# 데이터 수집                                    -------------------------------
library(MASS)

View(Boston)
str(Boston)



# 데이터 전처리                                  -------------------------------




# 기술 통계                                      -------------------------------
#install.packages("psych")
library(psych)

psych::describe(Boston)

psych::pairs.panels(Boston)



# 데이터 분석                                    -------------------------------
#install.packages("caret")
library(caret)

# 1. 데이터 분리 (훈련, Test)
#-------------------------------------------------------------------------------
?caret::createDataPartition # {caret} Data Splitting functions

# createDataPartition(
#                       y,
#                       times = 1,
#                       p = 0.5,
#                       list = TRUE,
#                       groups = min(5, length(y))
#                     )
#-------------------------------------------------------------------------------

train = caret::createDataPartition(y=Boston$medv,
                                   p=0.7,
                                   list=FALSE)
head(train)   # 356
length(train)


Boston_train = Boston[train, ]
Boston_test = Boston[-train, ]

nrow(Boston_train) # 356
nrow(Boston_test)  # 150



# - 1. 릿지회귀분석(ridge regression analysis)  [alpha = 0] -------------------- 
#install.packages("gmlnet")
library(glmnet)

#-------------------------------------------------------------------------------
?glmnet # {glmnet} fit a GLM with lasso or elasticnet regularization

# glmnet(
#   x,
#   y,
#   family = c("gaussian", "binomial", "poisson", "multinomial", "cox", "mgaussian"),
#   weights = NULL,
#   offset = NULL,
#   alpha = 1, => (0:ridge, 1:lasso, 0~1:elasticnet) ★★★
#   nlambda = 100,
#   lambda.min.ratio = ifelse(nobs < nvars, 0.01, 1e-04),
#   lambda = NULL,
#   ...
# )
#-------------------------------------------------------------------------------

# glmnet의 x(독립변수) 만들기 
x_train = model.matrix(medv ~ ., data=Boston_train)
x_train
str(x_train)
class(x_train)
View(x_train)

x_train = model.matrix(medv ~ ., data=Boston_train)[, -1]
head(x_train)

# glmnet의 y(종속변수) 만들기
y_train = Boston_train$medv
head(y_train)



# 최적의 lambda 값 구하기 (K-묶음 교차검증) k-fold cross-validation
#-------------------------------------------------------------------------------
?glmnet::cv.glmnet # {glmnet} Cross-validation for glmnet

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
#           gamma = c(0, 0.25, 0.5, 0.75, 1),
#           ...
#         )
#-------------------------------------------------------------------------------
set.seed(123)
Boston_cv = glmnet::cv.glmnet(x = x_train,
                                y = y_train,
                                family = "gaussian",   
                                alpha = 0)
plot(Boston_cv)
  
Boston_cv$lambda.min      # 최적의 lambda  0.7130849
log(Boston_cv$lambda.min) # log(lambda)   -0.3381548


# 모델 만들기
Boston_gnet = glmnet(x = x_train,
                     y = y_train,
                     family = "gaussian",   
                     alpha = 0,
                     lambda = Boston_cv$lambda.min)
coef(Boston_gnet)

# (Intercept)  29.715561769
# crim         -0.051154562
# zn            0.032546800
# indus        -0.054607265
# chas          2.697408225
# nox         -13.435755007
# rm            3.781034746
# age           0.008544603
# dis          -1.134104191
# rad           0.202975468
# tax          -0.004615840
# ptratio      -0.932221562
# black         0.013641284
# lstat        -0.557536128




# 모델 예측 확인 하기
x_test = model.matrix(medv ~ ., data=Boston_test)[, -1]
head(x_test)

# 예측
Boston_pred = predict(Boston_gnet, newx = x_test)
head(Boston_pred)

# glmnet의 y(종속변수) 만들기
y_test = Boston_test$medv
head(y_test)



# 예측 결과(성능) 확인 하기
cbind(Boston_pred, y_test)

#-------------------------------------------------------------------------------
?caret::postResample # {caret} Calculates performance across resamples

# defaultSummary(data, lev = NULL, model = NULL)
# postResample(pred, obs)
# twoClassSummary(data, lev = NULL, model = NULL)
# mnLogLoss(data, lev = NULL, model = NULL)
# multiClassSummary(data, lev = NULL, model = NULL)
# prSummary(data, lev = NULL, model = NULL)
#-------------------------------------------------------------------------------

caret::postResample(pred=Boston_pred,
                    obs=y_test)

#      RMSE  Rsquared       MAE 
# 4.2979072 0.7241669 3.2405845 









# - 2. 라소회귀분석(lasso regreesion analysis)  [alpha = 1] -------------------- 
Boston_cv = glmnet::cv.glmnet(x = x_train,
                              y = y_train,
                              family = "gaussian",   
                              alpha = 1)
plot(Boston_cv)

Boston_cv$lambda.min      # 최적의 lambda  0.01275459
log(Boston_cv$lambda.min) # log(lambda)   -4.361864

Boston_cv$lambda.1se      # 최적의 lambda  0.7646176
log(Boston_cv$lambda.1se ) # log(lambda)   -0.2683795

coef(Boston_cv, Boston_cv$lambda.min)

# (Intercept)  37.73361381
# crim         -0.06232167
# zn            0.04416737
# indus         .         
# chas          2.44652386
# nox         -19.69736961
# rm            3.54271919
# age           0.01616547
# dis          -1.45230333
# rad           0.34768514
# tax          -0.01017185
# ptratio      -1.02095840
# black         0.01459330
# lstat        -0.62570707

coef(Boston_cv, Boston_cv$lambda.1se)

# (Intercept) 15.3576914
# crim         .        
# zn           .        
# indus        .        
# chas         1.0354744
# nox          .        
# rm           3.8840281
# age          .        
# dis          .        
# rad          .        
# tax          .        
# ptratio     -0.6904002
# black        0.0066733
# lstat       -0.5315738


# 모델 만들기(비교 하기)
# - lambda.min
Boston_gnet1 = glmnet(x = x_train,
                       y = y_train,
                       family = "gaussian",   
                       alpha = 1,
                       lambda = Boston_cv$lambda.min)
coef(Boston_gnet1)

Boston_pred1 = predict(Boston_gnet1, newx = x_test)

caret::postResample(pred=Boston_pred1, obs=y_test)

#      RMSE  Rsquared       MAE 
# 4.4911734 0.7083641 3.4149621 



# - lambda.1se
Boston_gnet2 = glmnet(x = x_train,
                      y = y_train,
                      family = "gaussian",   
                      alpha = 1,
                      lambda = Boston_cv$lambda.1se)
coef(Boston_gnet2)

Boston_pred2 = predict(Boston_gnet2, newx = x_test)

caret::postResample(pred=Boston_pred2, obs=y_test)

#      RMSE  Rsquared       MAE 
# 4.3947364 0.7085962 3.4408307 





# - 3. 일래스틱넷회귀분석(elasticnet regreesion analysis)  [alpha = 0.1] -------

#-------------------------------------------------------------------------------
?caret::train # {caret} Fit Predictive Models over Different Tuning Parameters
# train(
#       x,
#       y,
#       method = "rf",
#       preProcess = NULL,
#       ...,
#       weights = NULL,
#       metric = ifelse(is.factor(y), "Accuracy", "RMSE"),
#       maximize = ifelse(metric %in% c("RMSE", "logLoss", "MAE"), FALSE, TRUE),
#       trControl = trainControl(),
#       tuneGrid = NULL,
#       tuneLength = ifelse(trControl$method == "none", 1, 3)
#     )
#-------------------------------------------------------------------------------

Boston_cv = caret::train(form=medv ~ ., 
                         data=Boston,
                         method="glmnet",
                         trControl=trainControl(method="cv", number=10),
                         tuneLength=10)

Boston_cv$bestTune
#   alpha     lambda
#     0.3 0.08918486


# - alpha     lambda
Boston_gnet3 = glmnet(x = x_train,
                      y = y_train,
                      family = "gaussian",   
                      alpha = Boston_cv$bestTune$alpha,
                      lambda = Boston_cv$bestTune$lambda)
coef(Boston_gnet3)

Boston_pred3 = predict(Boston_gnet3, newx = x_test)

caret::postResample(pred=Boston_pred3, obs=y_test)
#      RMSE  Rsquared       MAE 
# 4.4485863 0.7117362 3.3730510 






# 모델 성능 비교 학                 -------------------------------------------- 

lambda = 10^seq(-5, 5, length=100)
lambda

set.seed(123)
ridge = caret::train(form=medv ~ ., 
                       data=Boston_train,
                       method="glmnet",
                       trControl=trainControl(method="cv", number=10),
                       tuneGrid=expand.grid(alpha=0, lambda=lambda))
coef(ridge$finalModel, ridge$bestTune$lambda)

ridge_pred = predict(ridge, newx = x_test)
caret::postResample(pred=ridge_pred, obs=y_test)
# RMSE   Rsquared        MAE 
# NA 0.09351317         NA 

##############################

set.seed(123)
lasso = caret::train(form=medv ~ ., 
                     data=Boston_train,
                     method="glmnet",
                     trControl=trainControl(method="cv", number=10),
                     tuneGrid=expand.grid(alpha=1, lambda=lambda))
coef(lasso$finalModel, lasso$bestTune$lambda)

lasso_pred = predict(lasso, newx = x_test)
caret::postResample(pred=lasso_pred, obs=y_test)
# RMSE   Rsquared        MAE 
# NA 0.09686203         NA 


##############################

set.seed(123)
elastic = caret::train(form=medv ~ ., 
                     data=Boston_train,
                     method="glmnet",
                     trControl=trainControl(method="cv", number=10),
                     trueLength=10)
coef(elastic$finalModel, elastic$bestTune$lambda)

elastic_pred = predict(elastic, newx = x_test)
caret::postResample(pred=elastic_pred, obs=y_test)
# RMSE   Rsquared        MAE 
# NA 0.09554968         NA 


##############################

models = list(ridge=ridge, lasso=lasso, elastic=elastic)
summary(resamples(models))

summary(resamples(models), metric="RMSE")

summary(diff(resamples(models), metric="RMSE"))






















