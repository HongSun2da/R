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

















