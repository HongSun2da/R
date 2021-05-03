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
data_df = read.csv("./data/accidentsnn.csv",
                   header = TRUE,
                   na.strings = ".")

View(data_df)
str(data_df)    # 'data.frame':	999 obs. of  5 variables:
# $ ALCHL_I   : int  2 2 1 2 2 2 2 2 2 2 ... 음주 여부 : 1=있음, 2=없읍
# $ PROFIL_I_R: int  0 1 0 0 1 0 0 1 1 0 ... 도로정보 :  0=기타, 1=level1
# $ SUR_COND  : int  1 1 1 2 1 1 2 2 1 1 ... 도로의 노면상태 : 1=건조, 2=젖음, 3=눈/진흙, 9=모름
# $ VEH_INVL  : int  1 1 1 2 2 1 1 1 1 1 ... 관련됨 차량의 수
# $ MAX_SEV_IR: int  0 2 0 1 1 0 2 1 1 0 ... 상해/치명여부 : 0=무상해, 1=상해, 2=치명 (종족변수)

summary(data_df)



# 02. 데이터 전처리                     ----------------------------------------
# 범주가 여러개(3개 이상)일 경우는 더미변수로 변화 하기 class.ind★★★
vars = c("ALCHL_I", "PROFIL_I_R", "VEH_INVL")

accidents.ctg = cbind(data_df[, vars],
                      class.ind(data_df[,]$SUR_COND),
                      class.ind(data_df[,]$MAX_SEV_IR))


names(accidents.ctg) = c(vars, 
                         paste("SUR_COND_",c(1, 2, 3, 4, 9), sep = "" ),
                         paste("MAX_SEV_IR_",c(0,1,2), sep = "" ))

accidents.ctg
str(accidents.ctg)
summary(accidents.ctg)


# 03. 기술통계 확인                     ----------------------------------------
library(psych)

psych::describe(accidents.ctg)
psych::pairs.panels(accidents.ctg)



# 03. 데이터 분리 (훈련용, 검증용)   PASS     ----------------------------------
set.seed(2)

train_index = sample(c(1:dim(accidents.ctg)[1]),
                     dim(accidents.ctg)[1]*0.6)

train_df = accidents.ctg[train_index, ]
test_df = accidents.ctg[-train_index, ] 

describe(train_df)  # 'data.frame':	599 obs. of  11 variables:
describe(test_df)   # 'data.frame':	400 obs. of  11 variables:



# 03. 모델 생성 (ANN 신경망)            ----------------------------------------

model = neuralnet(MAX_SEV_IR_0 + MAX_SEV_IR_1 + MAX_SEV_IR_2 ~ ALCHL_I + PROFIL_I_R + VEH_INVL + SUR_COND_1 + SUR_COND_2 + SUR_COND_3 + SUR_COND_4,
                  data = train_df,
                  hidden = 2)
model


# 04. 모델 결과 확인 하기              -----------------------------------------
prediction(model)
plot(model, rep="best")


# 05. 모델 평가 하기 하기              -----------------------------------------
#  compute(net, covariate) : 데이터셋의 각 개체별로 출력값 산출

pred = compute(model,
               data.frame(test_df[, -c(8:11)]))

head(pred)
predicted_class = apply(pred$net.result,
                        1,
                        which.max) - 1


confusionMatrix(as.factor(predicted_class),
                as.factor(data_df[-train_index, ]$MAX_SEV_IR))


#             Reference 
# Prediction   0   1   2
#         0  216   0  20
#         1    0 123  23
#         2    0   5  13
# 
# Overall Statistics
# 
# Accuracy : 0.88            
# 95% CI : (0.8441, 0.9102)
# No Information Rate : 0.54            
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.7851     
# 









