## 서프트벡터머신(SVM)     #####################################################
# - SVM(support vector machine)은 수학적 최적화 기법을 기반으로 분류
# - 다차원 공간상에서 두 집단을 분리하는 초평면이라고 불리는 최적의 경계면을 탐색
# - P개의 차원이 있을 경우 초평면은 P-1차원의 평평한 서브공간으로 정의



################################################################################

# 데이터 수집    ---------------------------------------------------------------
iris
str(iris)
View(iris)



# 기술 통계      ---------------------------------------------------------------
#install.packages("psych")
library(psych)

psych::describe(iris)
psych::pairs.panels(iris)

#-------------------------------------------------------------------------------
?psych::pairs.panels # {psych} SPLOM, histograms and correlations for a data matrix

# ## S3 method for class 'panels'
# pairs(x, smooth = TRUE, scale = FALSE, density=TRUE,ellipses=TRUE,
#       digits = 2,method="pearson", pch = 20, lm=FALSE,cor=TRUE,jiggle=FALSE,factor=2, 
#       hist.col="cyan",show.points=TRUE,rug=TRUE, breaks = "Sturges",cex.cor=1,wt=NULL,
#       smoother=FALSE,stars=FALSE,ci=FALSE,alpha=.05, ...)
#-------------------------------------------------------------------------------




# 데이터 전처리  ---------------------------------------------------------------

data = subset(iris,
              select=c("Sepal.Length", "Sepal.Width", "Species"),
              subset=Species %in% c("setosa","virginica"))
str(data)

psych::pairs.panels(data)


# 그래프 확인
#install.packages("ggplot2")
library(ggplot2)

ggplot2::ggplot(data, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point(aes(color=Species, shape=Species), size=2)




# 데이터 분석    ---------------------------------------------------------------

#install.packages("e1071")
library(e1071)

#-------------------------------------------------------------------------------
?e1071::svm # {e1071} Support Vector Machines

# ## S3 method for class 'formula'
# svm(formula, data = NULL, ..., subset, na.action =
#       na.omit, scale = TRUE)
# 
# ## Default S3 method:
# svm(x, y = NULL, scale = TRUE, type = NULL, kernel =
#       "radial", degree = 3, gamma = if (is.vector(x)) 1 else 1 / ncol(x),
#     coef0 = 0, cost = 1, nu = 0.5,
#     class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.1,
#     shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE,
#     ..., subset, na.action = na.omit)
#-------------------------------------------------------------------------------


set.seed(123)
model = e1071::svm(Species ~ ., 
                   data=data,
                   kernel="linear",
                   cost=1,
                   scale=FALSE)

class(model) # "svm.formula" "svm"   
summary(model)

# Parameters:
#   SVM-Type:  C-classification (분류 모델을 사용)
# SVM-Kernel:  linear  (linear, polynomial, radial, sigmoid 등이 있음)
# cost:  1 (오분류의 패널티 지정값)
# 
# Number of Support Vectors:  12 (분류 기준을 할 12개 점을 찾았다 )
# ( 6 6 ) (12개점은 각각 6개, 6개로 나누어 있다)
 
# Number of Classes:  2 (분류 개수는 2개 이다)
# 
# Levels: 
#   setosa versicolor virginica


model$index # Number of Support Vectors:  12 (index 값)
model$SV
data[model$index, ]


# 그래프 확인

ggplot2::ggplot(data, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point(aes(color=Species, shape=Species), size=2) +
  geom_point(data=data.frame(model$SV), color="darkblue", shape=21, stroke=1.0, size=5)


class(data[model$index, c(1,2)])
class(data.frame(model$SV))

# 직선 만들기

w = t(model$coefs) %*% model$SV
w
b = -model$rho
b

ggplot2::ggplot(data, 
                aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point(aes(color=Species, shape=Species), 
             size=2) +
  geom_point(data=data.frame(model$SV), 
             color="darkblue", 
             shape=21, 
             stroke=1.0, 
             size=5) +
  geom_abline(intercept=-b/w[1,2], 
              slope=-(w[1,1]/w[1,2]), 
              color="dimgray", 
              lty="dashed", 
              lwd=1)

# 모델 예측 하기
data_pred = predict(model,
                    newdata=data)

table(data$Species, 
      data_pred,
      dnn=c("Actual","Predicted"))
#            Predicted
# Actual     setosa versicolor virginica
# setosa         50          0         0
# versicolor      0          0         0
# virginica       1          0        49

# 정확도 0.99
mean(data$Species == data_pred)




# cost 변경시 분류변화 조사 하기 -----------------------------------------------
# cost 값이 올라 가면 마진 값는 줄어 들어 과적합 모델 생성 함 ★★★★★

# cost 100
set.seed(123)
model = e1071::svm(Species ~ ., 
                   data=data,
                   kernel="linear",
                   cost=100,
                   scale=FALSE)
summary(model)

# SVM-Kernel:  linear 
# cost:  100 
# Number of Support Vectors:  3 ( 2 1 )

# 그래프 확인
w = t(model$coefs) %*% model$SV
b = -model$rho

ggplot2::ggplot(data, 
                aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point(aes(color=Species, shape=Species), 
             size=2) +
  geom_point(data=data.frame(model$SV), 
             color="darkblue", 
             shape=21, 
             stroke=1.0, 
             size=5) +
  geom_abline(intercept=-b/w[1,2], 
              slope=-(w[1,1]/w[1,2]), 
              color="dimgray", 
              lty="dashed", 
              lwd=1)


# cost 10
set.seed(123)
model = e1071::svm(Species ~ ., 
                   data=data,
                   kernel="linear",
                   cost=10,
                   scale=FALSE)
summary(model)

# SVM-Kernel:  linear 
# cost:  10
# Number of Support Vectors:  4 ( 2 2 )

# 그래프 확인
w = t(model$coefs) %*% model$SV
b = -model$rho

ggplot2::ggplot(data, 
                aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point(aes(color=Species, shape=Species), 
             size=2) +
  geom_point(data=data.frame(model$SV), 
             color="darkblue", 
             shape=21, 
             stroke=1.0, 
             size=5) +
  geom_abline(intercept=-b/w[1,2], 
              slope=-(w[1,1]/w[1,2]), 
              color="dimgray", 
              lty="dashed", 
              lwd=1)




# Affairs 데이터 모델링 하기     -----------------------------------------------
#install.packages("AER")

library(AER)

data(Affairs)
str(Affairs)

View(Affairs)

library(psych)

psych::describe(Affairs)
psych::pairs.panels(Affairs)

# 데이터 분리
data = Affairs

data$affairs = factor(ifelse(data$affairs > 1, 1, 0),
                      levels=c(0, 1),
                      labels=c("No","Yes"))
table(data$affairs)
prop.table(table(data$affairs))

?sample # {base} Random Samples and Permutations

# sample(x, size, replace = FALSE, prob = NULL)
# 
# sample.int(n, size = n, replace = FALSE, prob = NULL,
#            useHash = (!replace && is.null(prob) && size <= n/2 && n > 1e7))

set.seed(123)
data_idx = sample(nrow(data), nrow(data)*0.7)

data_train = data[data_idx, ]
data_test = data[-data_idx, ]

table(data_train$affairs)
table(data_test$affairs)

# 모델 생성
set.seed(123)
model = svm(affairs ~ .,
            data=data_train)

summary(model)
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  radial 
# cost:  1 
# Number of Support Vectors:  213 ( 134 79 )
# Number of Classes:  2 
 

model_pred = predict(model,
                     newdata=data_test)

table(data_test$affairs, 
      model_pred,
      dnn=c("Actual","Predicted"))

#         Predeted
# Acturl   No Yes
# No      144   0
# Yes     37    0

# 정확도 0.7955801
mean(data_test$affairs == model_pred)



# 예측 비율 확인 하기
set.seed(123)
model2 = svm(affairs ~ .,
            data=data_train,
            probability=TRUE)

model_pred2 = predict(model2,
                     newdata=data_test,
                     probability=TRUE)

attr(model_pred2, "probabilities")[1:6, ]


cbind(attr(model_pred2, "probabilities"), model_pred2, data_train$affairs)[1:20,]




# 모델링 튜닝     --------------------------------------------------------------
set.seed(123)
model_tune = e1071::tune.svm(affairs ~ .,
                             data=data_train,
                             gamma=10^(-3:3),
                             cost=2^(-5:5))
summary(model_tune)

# Parameter tuning of ‘svm’:
# - sampling method: 10-fold cross validation 
# - best parameters:
#   gamma cost
#     0.1    4
# - best performance: 0.1857143 

model_tune$best.model$gamma
model_tune$best.model$cost




# 모델 확인(최적화 모델 생성)
model = svm(affairs ~ .,
            data=data_train,
            gamma=0.1,
            cost=4)

model_pred = predict(model,
                     newdata=data_test)

table(data_test$affairs, 
      model_pred,
      dnn=c("Actual","Predicted"))

#         Predeted
# Acturl   No Yes
# No      141   3
# Yes      29   8

# 정확도 0.8232044
mean(data_test$affairs == model_pred)




# iris 데이터 모델링 하기     --------------------------------------------------

data = iris
data_idx = sample(nrow(iris), nrow(iris)*0.7)
data_train = data[data_idx, ]
data_test = data[-data_idx, ]

model = svm(Species ~.,
            data=data)

summary(model)
# SVM-Type:  C-classification 
# SVM-Kernel:  radial 
# cost:  1 
# Number of Support Vectors:  51 ( 8 22 21 )
# Number of Classes:  3 

# 그래프 확인 하기

table(data_train[, -5])

dis = dist(data_train[, -5])
class(dis) # dist
str(dis) # dist' Named num [1:5460]

summary(dis)

cmdscale(dis)

#-------------------------------------------------------------------------------
?cmdscale # {stats} Classical (Metric) Multidimensional Scaling

# cmdscale(d, k = 2, eig = FALSE, add = FALSE, x.ret = FALSE,
#          list. = eig || add || x.ret)
#-------------------------------------------------------------------------------

data_mds = data.frame(cmdscale(dist(data_train[, -5])))

ggplot2::ggplot(data_mds,
                aes(x=X1, y=X2)) +
  geom_point(aes(color=data_train[,5], shape=data_train[,5]),
             size=2) +
  geom_point(data=data_mds[model$index,],
             color="dimgray",
             shape=21,
             stroke=1.0,
             size=5) 



# 모델 예측 하기
model_pred = predict(model,
                     newdata=data_test)

table(data_test$Species, 
      model_pred,
      dnn=c("Actual","Predicted"))

#             Predicted
# Actual       setosa versicolor virginica
# setosa           11          0         0
# versicolor        0          8         1
# virginica         0          1        24

# 정확도 0.9555556
mean(data_test$Species == model_pred)







