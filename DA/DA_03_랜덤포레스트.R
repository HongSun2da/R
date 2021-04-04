## 랜덤포레스트     ############################################################
# - 앙상블학습(ensemble learning)
# - 배깅(bagging, bootstrap aggergation)
# - 



################################################################################

# 데이터 수집    ---------------------------------------------------------------
library(mlbench)

data("BreastCancer")
str(BreastCancer) # 'data.frame':	699 obs. of  11 variables:

View(BreastCancer)

summary(BreastCancer)



# 기술 통계      ---------------------------------------------------------------
library(psych)
psych::describe(BreastCancer)
psych::pairs.panels(BreastCancer)





# 데이터 전처리  ---------------------------------------------------------------
table(BreastCancer$Class)
# benign malignant 
# 458       241 

mean(BreastCancer$Class=="benign")    # 0.6552217
mean(BreastCancer$Class=="malignant") # 0.3447783

sum(!complete.cases(BreastCancer))

data = BreastCancer[, -1]

# 데이터 numeric형으로 변경하기
data = cbind(lapply(data[, -10],
                    function(x) as.numeric(as.character(x))), 
             data[10])
str(data)

# 데이터 분리 하기

set.seed(567)
data_idx = sample(nrow(data), 0.7*nrow(data))
data_train = data[data_idx, ]
data_test = data[-data_idx, ]

table(data_train$Class)
# benign malignant 
# 313       176 

table(data_test$Class)
# benign malignant 
# 145        65 




# 데이터 분석    ---------------------------------------------------------------
#install.packages("randomForest")
library(randomForest)


#-------------------------------------------------------------------------------
?randomForest::randomForest # {randomForest} Classification and Regression with Random Forest

# ## S3 method for class 'formula'
# randomForest(formula, data=NULL, ..., subset, na.action=na.fail)
# ## Default S3 method:
# randomForest(x, y=NULL,  xtest=NULL, ytest=NULL, ntree=500,
#              mtry=if (!is.null(y) && !is.factor(y))
#                max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x))),
#              replace=TRUE, classwt=NULL, cutoff, strata,
#              sampsize = if (replace) nrow(x) else ceiling(.632*nrow(x)),
#              nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1,
#              maxnodes = NULL,
#              importance=FALSE, localImp=FALSE, nPerm=1,
#              proximity, oob.prox=proximity,
#              norm.votes=TRUE, do.trace=FALSE,
#              keep.forest=!is.null(y) && is.null(xtest), corr.bias=FALSE,
#              keep.inbag=FALSE, ...)
# ## S3 method for class 'randomForest'
# print(x, ...)
#-------------------------------------------------------------------------------

# 모델 생성
model = randomForest::randomForest(Class ~ ., 
                                   data=data_train,
                                   na.action=na.roughfix,
                                   importance=TRUE)
model

#   randomForest(formula = Class ~ ., data = data_train, importance = TRUE,      na.action = na.roughfix) 
# Type of random forest: classification (분류 모델)
# Number of trees: 500 (나무 생성 숫자)
# No. of variables tried at each split: 3 (예측변수 9개중에서 분류는 9에 root인 3을 기본으로 함 )
# 
# OOB estimate of  error rate: 4.09% (복원 추출시 한번도 선택된적이 없는 샘플) # data[model$oob.times,]
# Confusion matrix:
#           benign malignant class.error
# benign       304        11  0.03492063
# malignant      9       165  0.05172414

model_pred = predict(model,
                     newdata=data_test,
                     type="class")
model_pred

model_pred = predict(model,
                     newdata=data_test,
                     type="response")
model_pred

table(data_test$Class, model_pred,
      dnn=c("Actual", "Predicted"))

#           Predicted
# Actual      benign malignant
# benign       135         3
# malignant      1        65

# 정확도 0.9803922
mean(data_test$Class == model_pred, na.rm = TRUE)




# 모델 분석

model_predAll = predict(model,
                        newdata=data_test,
                        predict.all=TRUE,
                        type="response")

str(model_predAll) # $ individual: chr [1:204, 1:500] 500개의 결과 값 저장
# $ aggregate : Factor w/ 2 levels "benign","malignant": 1 1 2 1 1 1 2 1 2 1 ...
# ..- attr(*, "names")= chr [1:210] "1" "5" "6" "11" ...
# $ individual: chr [1:204, 1:500] "benign" "benign" "malignant" "benign" ...
# ..- attr(*, "dimnames")=List of 2
# .. ..$ : chr [1:204] "1" "5" "6" "11" ...
# .. ..$ : NULL

model_predAll$aggregate

# 6번째 나무 결과 확인
model_predAll$individual[6, ]
table(model_predAll$individual[6, ])
na.omit(model_predAll$individual[6, ])

apply(model_predAll$individual[21:25, ], 1, table)
#            53  55  57  61  63
# benign     45   2 103 129  87
# malignant 455 498 397 371 413

#결과 비교
na.omit(model_predAll$aggregate)[21:25]
#        53        55        57        61        63 
# malignant malignant malignant malignant malignant 




# 예측변수의 결정 중요도 # importance=TRUE
randomForest::importance(model)

randomForest::varImpPlot(model)












