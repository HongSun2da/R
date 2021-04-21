## 군집분석  ###################################################################
# 1. 유사도 측정
# 2. 계층적 군집분석
# 3. k-평균 군집분석
# 4. PAM군집분석

################################################################################
# 4. PAM군집분석
# - 관측값 중의 하나를 중심점(메도이 medoid) 사용
# - 여러 유형이 분석 수행 가능

################################################################################

# 데이터 수집    ---------------------------------------------------------------
#install.packages("rattle")
library(rattle)

str(wine)
View(wine)

# 기술 통계      ---------------------------------------------------------------
library(psych)
psych::describe(wine)
psych::describeBy(wine, group = wine$Type)
psych::pairs.panels(wine)

psych::pairs.panels(wine[wine$Type == 1,])
psych::pairs.panels(wine[wine$Type == 2,])
psych::pairs.panels(wine[wine$Type == 3,])



# 데이터 전처리  ---------------------------------------------------------------
data = wine[-1]




# 데이터 분석    ---------------------------------------------------------------
library(cluster)

#-------------------------------------------------------------------------------
?cluster::pam # {cluster} Partitioning Around Medoids

# pam(x, k, diss = inherits(x, "dist"),
#     metric = c("euclidean", "manhattan"), 
#     medoids = NULL, stand = FALSE, cluster.only = FALSE,
#     do.swap = TRUE,
#     keep.diss = !diss && !cluster.only && n < 100,
#     keep.data = !diss && !cluster.only,
#     pamonce = FALSE, trace.lev = 0)
#-------------------------------------------------------------------------------

model = pam(data, k=3, stand = TRUE)

summary(model)
# Medoids:
#       ID Alcohol Malic  Ash Alcalinity Magnesium Phenols Flavanoids Nonflavanoids Proanthocyanins Color  Hue Dilution Proline
# [1,]  36   13.48  1.81 2.41       20.5       100    2.70       2.98          0.26            1.86   5.1 1.04     3.47     920
# [2,] 107   12.25  1.73 2.12       19.0        80    1.65       2.03          0.37            1.63   3.4 1.00     3.17     510
# [3,] 175   13.40  3.91 2.48       23.0       102    1.80       0.75          0.43            1.41   7.3 0.70     1.56     750

# Numerical information per cluster:
#      size max_diss  av_diss  diameter separation
# [1,]   75 7.164558 3.611501 11.255700   2.206761
# [2,]   54 6.473747 3.511231 10.901681   2.142040
# [3,]   49 5.781398 3.232620  9.027462   2.142040


aggregate(data,
          by=list(cluster=model$clustering),
          mean)

# 그래프 확인

#-------------------------------------------------------------------------------
?cluster::clusplot # {cluster} Bivariate Cluster Plot (of a Partitioning Object)

# clusplot(x, ...)
# 
# ## S3 method for class 'partition'
# clusplot(x, main = NULL, dist = NULL, ...)
#-------------------------------------------------------------------------------

cluster::clusplot(model, 
                  main="Cluster Plot",
                  color=TRUE,
                  shade=TRUE,
                  labels=4,
                  lines=0)

# 교차표
table(wine$Type,
      model$clustering,
      dnn=c("Actual","Clustered")
)      
#          Clustered
# Actual   1  2  3
#       1 59  0  0
#       2 16 53  2
#       3  0  1 47

# 정확도 0.8932584
mean(wine$Type == model$clustering)











