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
# 지도학습 > 분류(범주) > 의사결정 분류나무     [Decision Tree Classification]  
#-------------------------------------------------------------------------------

# package 설치
# install.packages("rpart")         # CART 구현 패키지
# install.packages("rpart.plot")
# install.packages("randomForest")
# install.packages("adabag")
# 
# install.packages("caret")       # 모델성능평가(ConfusionMatrx)

library(rpart) 
library(rpart.plot)
library(caret)



# 01. 데이터 불러 오기             ---------------------------------------------
df_data = read.csv("./data/UniversalBank.csv",
                   header = TRUE,
                   na.strings = ".")

View(df_data)
str(df_data)
summary(df_data)



# 02. 데이터 전처리                ---------------------------------------------
## - 1. Personal.Loan factor 처리
df_data$Personal.Loan = factor(df_data$Personal.Loan,
                               levels = c(0,1),
                               labels = c("No", "Yes"))

## - 2. ID , ZIP.Code 사용안함 삭제 처리
df_data = df_data[, -c(1, 5)]



# 03. 기술통계 확인                ---------------------------------------------
library(psych)

## - 1. describe 확인
psych::describe(df_data)

## - 2. 상관관계 확인
psych::pairs.panels(df_data)



# 04. 데이터 분리(Train, Test)     ---------------------------------------------
set.seed(1)

train_idx = sample(c(1:dim(df_data)[1]),
                   dim(df_data)[1]*0.6)    # dim(df_data)  # 5000   12

train_df = df_data[train_idx, ]
test_df =  df_data[-train_idx, ]

psych::describe(train_df)
psych::describe(test_df)



# 05. 분류나무 생성 classification tree    -------------------------------------
# rpart(method) = "anova", "poisson", "class", "exp"

#-------------------------------------------------------------------------------
?rpart::rpart # {rpart} Recursive Partitioning and Regression Trees

# rpart(formula, data, weights, subset, na.action = na.rpart, method,
#       model = FALSE, x = FALSE, y = TRUE, parms, control, cost, ...)

# rpart.control(minsplit = 20, minbucket = round(minsplit/3), cp = 0.01, 
#               maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
#               surrogatestyle = 0, maxdepth = 30, ...)
#-------------------------------------------------------------------------------

# - 1. Default 생성
model_def = rpart::rpart(Personal.Loan ~ ., 
                          data = train_df,
                          method = "class")

model_def
summary(model_def)

rpart::printcp(model_def)    # 결과 중에서 CP만 가져오기
rpart::plotcp(model_def)     # CP결과를 그래프로 그리기



# - 2. 완전모형 생성 : Figure 9.10
model_dep = rpart(Personal.Loan ~ ., 
                  data = train_df,
                  method = "class",
                  cp = 0,
                  minsplit = 1)

model_dep
summary(model_dep)
show(model_dep)


# - 3. 다른 모형 생성 







# 06. 분류나무 그래프 확인                 -------------------------------------

#-------------------------------------------------------------------------------
?rpart.plot::prp # {rpart.plot} Plot an rpart model.
#-------------------------------------------------------------------------------
rpart.plot::prp(model_def,
                type=1,
                extra=1,
                under=F,
                split.font = 1,
                varlen = -10,
                box.col=ifelse(model_def$frame$var == "<leaf>","gray","white"))



# 07. 모델 검증 - 정오행렬표 이용          -------------------------------------

# - 1. Default 모델로 검증

pred_cla = predict(model_def, 
                   test_df,
                   type = "class")

pred_cla

caret::confusionMatrix(pred_cla,
                       test_df$Personal.Loan)

# Accuracy : 0.98 

#             Reference
# Prediction    No  Yes
#         No  1787   32
#         Yes    8  173



# - 2. 완전모형 모델로 검증

pred_cla = predict(model_dep, 
                   test_df,
                   type = "class")

pred_cla

confusionMatrix(pred_cla,
                test_df$Personal.Loan)

# Accuracy : 0.9835 

#           Reference
# Prediction   No  Yes
#       No  1782   20
#       Yes   13  185



# 08. 가지치기(Pruning the Tree) - CP 계산     ---------------------------------

model_prun = rpart(Personal.Loan ~ ., 
                   data = train_df,
                   method = "class",
                   cp = 0.00001,
                   minsplit = 5,
                   xval = 5)

printcp(model_prun)   # 결과 중에서 CP만 가져오기

# Root node error: 275/3000 = 0.091667
# 
# n= 3000 
# 
#           CP nsplit rel error  xerror     xstd
# 1  0.3218182      0  1.000000 1.00000 0.057472
# 2  0.1454545      2  0.356364 0.40364 0.037596
# 3  0.0181818      3  0.210909 0.21818 0.027884
# 4  0.0169697      4  0.192727 0.21818 0.027884
# 5  0.0090909      7  0.141818 0.18545 0.025747
# 6  0.0072727      9  0.123636 0.17818 0.025246  ★★★★★
# 7  0.0048485     12  0.101818 0.19273 0.026238
# 8  0.0036364     15  0.087273 0.18545 0.025747
# 9  0.0024242     22  0.061818 0.18182 0.025498
# 10 0.0018182     25  0.054545 0.18909 0.025994
# 11 0.0000100     27  0.050909 0.18909 0.025994

plotcp(model_prun)  # CP결과를 그래프로 그리기


# 09. CP 결과를 이용한 가지치기(Pruning the Tree)     --------------------------

# - 1. 교차검정에러(xerror) 이용
pruned.ct = prune(model_prun,
                  cp = model_prun$cptable[which.min(model_prun$cptable[, "xerror"]), "CP"])

# which.min(model_prun$cptable[, "xerror"])   # 6
# 
# model_prun$cptable[6, "CP"]   # 0.007272727

# - 2. 그래프 확인

prp(pruned.ct,
    type=1,
    extra=1,
    under=F,
    split.font = 1,
    varlen = -10)


# 10. 최적화 모델     ----------------------------------------------------------

model_opt = rpart(Personal.Loan ~ ., 
                  data = train_df,
                  method = "class",
                  cp = 0.00001,
                  minsplit = 1,
                  xval = 5)

summary(model_opt)
show(model_opt)

# 7  0.004848485     12 0.10181818 0.1454545 0.02284455

pruned.ct = prune(model_opt,
                  cp = 0.004848485)


prp(pruned.ct,
    type=1,
    extra=1,
    under=F,
    split.font = 1,
    varlen = -10)


# 10. 최적화 모델(예측 하기)     ----------------------------------------------------------


pred_cla = predict(pruned.ct, 
                   test_df,
                   type = "class")

pred_cla

confusionMatrix(pred_cla,
                test_df$Personal.Loan)

# Accuracy : 0.982 

#             Reference
# Prediction   No  Yes
#         No  1788   29
#         Yes    7  176





