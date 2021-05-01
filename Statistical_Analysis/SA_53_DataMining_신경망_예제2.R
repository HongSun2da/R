################################################################################
# 데이터 마이닝[Data Mining]
# 
# 지도학습(Supervised learning)
#         분류(범주)
#                     로지스틱 회귀         [Logistic Regression]
#                     나이브 베이즈         [Naive Bayes Classification]
#                     의사결정 분류나무     [Decision Tree Classification]  
#                     K-최근접 이웃         [k-Nearest Neighbors]
#                     서포트 벡터 머신      [SVM, Support Vector Machine]
#                     신경망                [Neural Network]
#         예측(연속)
#                     선형회귀              [Linear Regression]
#                     의사결정 회귀나무     [Decision Tree Regression]  
#                     시계열 분석
#
# 비지도학습(Unsupervised learning)
#         군집(Clustering)
#                     K-평균                [k-Means]
#                     계층 군집 분석        [HCA, Hierarchical Cluster Analysis]
#                     기댓값 최대화         [Expectation Maximization]
#         시각화(Visualization)와 차원 축소(Dimensionality Reduction)
#                     주성분 분석           [PCA, Principal Component Analysis]
#                     커널                  [PCA(Kernel PCA]
#                     지역적 선형 임베딩    [LLE, Locally-Linear Embedding]
#                     t-SNE                 [t-distributed Stochastic Neighbor Embedding]
#         연관 규칙 학습(Association Rule Learning)
#                     어프라이어리          [Apriori]
#                     이클렛                [Eclat]
#
# 강화학습(Reinforcement learning)
#
#
################################################################################

#-------------------------------------------------------------------------------
# 지도학습 > 분류(범주) > 신경망                [Neural Network]
#-------------------------------------------------------------------------------

# install.packages("neuralnet")
# install.packages("nnet")
# install.packages("caret")
# install.packages("e1071")

library(neuralnet)
library(nnet)
library(caret)
library(e1071)



# 01. 데이터 불러 오기                  ----------------------------------------
data_df = iris

View(data_df)
str(data_df)    # 'data.frame':	150 obs. of  5 variables:
# $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...      꽃받침의 길이
# $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...    꽃받침의 넓이
# $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...  꽃잎의 길이
# $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...  꽃잎의 너비
# $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...

summary(data_df)



# 02. 데이터 전처리                     ----------------------------------------
# 수치형 자료 : 표준화(scale)
# 범주형 자료 : 범주화(class.ind)

data_iris = cbind(scale(data_df[-5]),
                    class.ind(data_df[,]$Species))

str(data_iris)
summary(data_iris)


# 03. 기술통계 확인                     ----------------------------------------
library(psych)

psych::describe(data_iris)
psych::pairs.panels(data_iris)



# 03. 데이터 분리 (훈련용, 검증용)   PASS     ----------------------------------
set.seed(2)

train_index = sample(c(1:dim(data_iris)[1]),
                     dim(data_iris)[1]*0.6)

train_df = data_iris[train_index, ]
test_df = data_iris[-train_index, ] 

describe(train_df)
describe(test_df)



# 03. 모델 생성 (ANN 신경망)            ----------------------------------------

model = neuralnet(setosa + versicolor + virginica ~ .,
                  data = train_df,
                  hidden = 2)
model


# 04. 모델 결과 확인 하기              -----------------------------------------
prediction(model)
plot(model, rep="best")


# 05. 모델 평가 하기 하기              -----------------------------------------
#  compute(net, covariate) : 데이터셋의 각 개체별로 출력값 산출
pred = compute(model,
               data.frame(test_df[, c(1:4)]))

head(pred)
predicted_class = apply(pred$net.result,
                        1,
                        which.max) - 1

predicted_class = factor(predicted_class,
                         levels = c(0,1,2),
                         labels = c("setosa","versicolor","virginica"))

confusionMatrix(as.factor(predicted_class),
                as.factor(data_df[-train_index, ]$Species))


#               Reference
# Prediction   setosa versicolor virginica
# setosa         18          0         0
# versicolor      0         20         1
# virginica       0          1        20
# 
# Overall Statistics
# 
# Accuracy : 0.9667          
# 95% CI : (0.8847, 0.9959)
# No Information Rate : 0.35            
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.9499          
# 
# Mcnemar's Test P-Value : NA        









