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
# 비지도학습 > 연관 규칙 > 
#-------------------------------------------------------------------------------

## 연관 분석     ###############################################################
#install.packages("arules")
library(arules)



# 01. 데이터 불러 오기                    --------------------------------------
data_df = read.csv("./data/Faceplate.csv",
                   header = TRUE,
                   na.strings = ".")

View(data_df)
str(data_df)
summary(data_df)




# 02. 기술 통계 확인 하기                ---------------------------------------
library(psych)

describe(data_df)
pairs.panels(data_df)




# 03. 데이터 전처리 하기                  --------------------------------------
# - 1. matrix 형으로 변환
data_mt = as.matrix(data_df[, -1])

data_mt
str(data_mt)
summary(data_mt)

# - 2. transactions 으로 변환
data_tr = as(data_mt, "transactions")

data_tr
str(data_tr)
summary(data_tr)

inspect(data_tr)

#       items                 
# [1]  {Red,White,Green}     
# [2]  {White,Orange}        
# [3]  {White,Blue}          
# [4]  {Red,White,Orange}    
# [5]  {Red,Blue}            
# [6]  {White,Blue}          
# [7]  {Red,Blue}            
# [8]  {Red,White,Blue,Green}
# [9]  {Red,White,Blue}      
# [10] {Yellow}  




# 04. 연관규칙 실행                    -----------------------------------------
# supp = 지지도, conf = 신뢰도

model = apriori(data_tr,
                parameter = list(supp = 0.2,
                                 conf = 0.5,
                                 target = "rules"))

model
summary(model)




# 05. 규칙 확인 : lift가 높은 순서로   -----------------------------------------
data_tb = inspect(model)

data_tb

inspect(sort(model, by="lift"))

data_tb[data_tb$support >= 0.04 & data_tb$confidence >= 0.7, ]
















