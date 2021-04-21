## 군집분석  ###################################################################
# 1. 유사도 측정
# 2. 계층적 군집분석
# 3. k-평균 군집분석
# 4. PAM군집분석

################################################################################
# 3. k-평균 군집분석
# - 최초의 k개 중심점 선택(연속형 데이터)

################################################################################

# 데이터 수집    ---------------------------------------------------------------
str(state.x77)
View(state.x77)



# 기술 통계      ---------------------------------------------------------------
library(psych)
psych::describe(state.x77)
psych::pairs.panels(state.x77)



# 데이터 전처리  ---------------------------------------------------------------
data_scale = scale(state.x77) 
summary(data_scale)





# 데이터 분석    ---------------------------------------------------------------
library(NbClust) 

# k값 찾기
nc = NbClust::NbClust(data=data_scale,
                      distance="euclidean",
                      method="kmeans",
                      min.nc=2,
                      max.nc=15)
nc$Best.nc

table(nc$Best.nc[1,])
# 0  1  2  [3]  4  5  6  8  9 15 
# 2  1  6  [6]  2  1  1  2  1  4 


?kmeans # {stats} K-Means Clustering

# kmeans(x, centers, iter.max = 10, nstart = 1,
#        algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
#                      "MacQueen"), trace=FALSE)
# ## S3 method for class 'kmeans'
# fitted(object, method = c("centers", "classes"), ...)

set.seed(123)
model = kmeans(data_scale,
               centers = 3,
               nstart = 25)
model

#-------------------------------------------------------------------------------
?aggregate # {stats} Compute Summary Statistics of Data Subsets

# aggregate(x, ...)
# 
# ## Default S3 method:
# aggregate(x, ...)
# 
# ## S3 method for class 'data.frame'
# aggregate(x, by, FUN, ..., simplify = TRUE, drop = TRUE)
# 
# ## S3 method for class 'formula'
# aggregate(formula, data, FUN, ...,
#           subset, na.action = na.omit)
# 
# ## S3 method for class 'ts'
# aggregate(x, nfrequency = 1, FUN = sum, ndeltat = 1,
#           ts.eps = getOption("ts.eps"), ...)
#-------------------------------------------------------------------------------

aggregate(state.x77,
          by=list(cluster=model$cluster),
          mean)

# 그래프 확인
library(cluster)

#-------------------------------------------------------------------------------
?cluster::clusplot # {cluster} Bivariate Cluster Plot (of a Partitioning Object)

# clusplot(x, ...)
# 
# ## S3 method for class 'partition'
# clusplot(x, main = NULL, dist = NULL, ...)
#-------------------------------------------------------------------------------

cluster::clusplot(x=state.x77,
                  clus=model$cluster,
                  color=TRUE,
                  shade=TRUE,
                  lables=2,
                  lines=0,
                  main="Cluster Plot")


