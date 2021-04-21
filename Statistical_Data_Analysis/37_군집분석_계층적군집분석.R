## 군집분석  ###################################################################
# 1. 유사도 측정
# 2. 계층적 군집분석
# 3. k-평균 군집분석
# 4. PAM군집분석

################################################################################
# 2. 계층적 군집분석
# - 하나의 군집 형성 -> 단계별 유사한 군집 합치기 -> 최종 하나의 군집만 남음
# - 단일연결법(single linkage)
# - 완전연결법(complete linkage)
# - 평균연결법(average linkage)
# - 중심연결법(centroid linkage)
# - 최소분산연결법(minimum variance linkage)

################################################################################

# 데이터 수집    ---------------------------------------------------------------
library(flexclust)

data("nutrient")
class(nutrient)
str(nutrient) # 'data.frame':	27 obs. of  5 variables:

View(nutrient)




# 기술 통계      ---------------------------------------------------------------
library(psych)

psych::describe(nutrient)
psych::pairs.panels(nutrient)



# 데이터 전처리  ---------------------------------------------------------------
data = nutrient
row.names(data) = tolower(row.names(data))

head(data)

# 표준화
data_scale = scale(data)
data_scale




# 데이터 분석    ---------------------------------------------------------------

# 1. 거리값 구하기
distance = dist(data_scale)

#-------------------------------------------------------------------------------
?hclust # {stats} Hierarchical Clustering

# hclust(d, method = "complete", members = NULL)
# 
# ## S3 method for class 'hclust'
# plot(x, labels = NULL, hang = 0.1, check = TRUE,
#      axes = TRUE, frame.plot = FALSE, ann = TRUE,
#      main = "Cluster Dendrogram",
#      sub = NULL, xlab = NULL, ylab = "Height", ...)
#-------------------------------------------------------------------------------


# 2. 계층적 군집
model_avg = hclust(distance, 
                   method="average")

summary(model_avg)

# Cluster Dendrogram
plot(model_avg,
     hang=-1,
     col="darkgreen",
     xlab="Food",
     main="Hierarchical Clustering with Average Link")


# 최적화 군집수 찾기
#install.packages("NbClust")
library(NbClust) 

#-------------------------------------------------------------------------------
?NbClust::NbClust # {NbClust} NbClust Package for determining the best number of clusters

# NbClust(data = NULL, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15, 
#         method = NULL, index = "all", alphaBeale = 0.1)
#-------------------------------------------------------------------------------
nc = NbClust::NbClust(data=data_scale,
                       distance="euclidean",
                       method="average",
                       min.nc=3,
                       max.nc=15)
nc$Best.nc

table(nc$Best.nc[1,])
# 0  3  4  [5]  9 10 13 14 15 
# 2  5  3  [7]  1  1  2  1  4 

model_res = cutree(model_avg, k=5)
model_res

table(model_res)
# 1  2  3  4  5 
# 7 16  1  2  1 


# Cluster Dendrogram
plot(model_avg,
     hang=-1,
     col="darkgreen",
     xlab="Food",
     main="Hierarchical Clustering with Average Link")
rect.hclust(model_avg, k=5)


a = aggregate(data_scale, by=list(cluster=model_res), mean)
n = as.vector(table(model_res))
cbind(a,n)



