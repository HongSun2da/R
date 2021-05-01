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

#install.packages("neuralnet")
library(neuralnet)
library(caret)


# 01. 데이터 불러 오기                  ----------------------------------------
data_df = read.csv("./data/tinydata.csv",
                   header = TRUE,
                   na.strings = ".")
View(data_df)
str(data_df)
summary(data_df)



# 02. 데이터 전처리                     ----------------------------------------
data_df$Acceptance = factor(data_df$Acceptance)

data_df$Like = data_df$Acceptance == "like"
data_df$DisLike = data_df$Acceptance == "dislike"



# 03. 기술통계 확인                     ----------------------------------------
library(psych)

psych::describe(data_df)
psych::pairs.panels(data_df)



# 03. 데이터 분리 (훈련용, 검증용)   PASS     ----------------------------------
# 데이터가 너무 작아서 분리 안함



# 03. 모델 생성 (ANN 신경망)            ----------------------------------------

#-------------------------------------------------------------------------------
?neuralnet::neuralnet # {neuralnet} Training of neural networks

# neuralnet(formula, data, hidden = 1, threshold = 0.01,
#           stepmax = 1e+05, rep = 1, startweights = NULL,
#           learningrate.limit = NULL, learningrate.factor = list(minus = 0.5,plus = 1.2), 
#           learningrate = NULL, lifesign = "none",
#           lifesign.step = 1000, algorithm = "rprop+", err.fct = "sse",
#           act.fct = "logistic", linear.output = TRUE, exclude = NULL,
#           constant.weights = NULL, likelihood = FALSE)

# neuralnet(formula = ,        : 출력 ~ 입력 + 입력  
#           data = ,           :
#           hidden = ,         : hidden layer와 노드수 (2, 3)
#           rep = ,            : 모형의 적합 횟수
#           algorithm = ,      : 역전파 알고리즘 backprop, rporp+, rporp-
#           err.fct = ,        : 오차총합 sse, ce
#           act.fct = ,        : 활성화 함수, logistic, tanh
#           linear.output = ,  : 출력노드에서 선형 활성화 비적용
#           likelihood = )     : AIC, BIC 산술
#-------------------------------------------------------------------------------
set.seed(1)
model = neuralnet(Like + DisLike ~ Salt + Fat,
                  data = data_df,
                  linear.output = F,
                  hidden = 3)
model



# 04. 모델 결과 확인 하기              -----------------------------------------
prediction(model)
plot(model, rep="best")




# 05. 모델 평가 하기                   -----------------------------------------
#  compute(net, covariate) : 데이터셋의 각 개체별로 출력값 산출

#-------------------------------------------------------------------------------
?neuralnet::compute # {neuralnet} Deprecated function

# compute(x, covariate, rep = 1)
#-------------------------------------------------------------------------------

pred = compute(model,
               data.frame(data_df$Salt, data_df$Fat))

head(pred)
predicted_class = apply(pred$net.result,
                        1,
                        which.max) - 1


confusionMatrix(as.factor(ifelse(predicted_class == "1", "dislike", "like")),
                as.factor(data_df$Acceptance))

#           Reference
# Prediction dislike like
# dislike       3    0
# like          0    3
# 
# Accuracy : 1          
# 95% CI : (0.5407, 1)
# No Information Rate : 0.5        
# P-Value [Acc > NIR] : 0.01563  

