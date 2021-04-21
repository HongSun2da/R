## 차원분석  ###################################################################
# 1. 주성분분석 (principal component analysis, PCA)
# 2. 요인분석 (factor analysis, FA)
# 3. 다차원척도법 (multidimensional scaling, MDS)

################################################################################
# 3. 다차원척도법 (multidimensional scaling, MDS)
# - 



################################################################################

# 데이터 수집                                    -------------------------------
str(eurodist) # dist' num [1:210] 3313 2963 3175 3339 2762 ...

View(as.matrix(eurodist))

data = as.matrix(eurodist)


# 기술 통계      ---------------------------------------------------------------
library(psych)
psych::describe(data)



# 데이터 전처리  ---------------------------------------------------------------




# 데이터 분석    ---------------------------------------------------------------

#-------------------------------------------------------------------------------
?cmdscale # {stats} Classical (Metric) Multidimensional Scaling

# cmdscale(d, k = 2, eig = FALSE, add = FALSE, x.ret = FALSE,
#          list. = eig || add || x.ret)
#-------------------------------------------------------------------------------
eurocity = cmdscale(d=eurodist, k=2)
eurocity


plot(eurocity,
     type="n",
     main="Multidimentional Scale")
text(eurocity,
     rownames(eurocity),
     col="maroon",
     cex=0.7)


# 데이터 분석    ---------------------------------------------------------------
str(USJudgeRatings) # 'data.frame':	43 obs. of  12 variables:
View(USJudgeRatings)

distance = dist(USJudgeRatings)
View(as.matrix(distance))


distance_mds = cmdscale(d=distance)

plot(distance_mds,
     type="n",
     main="USJudgeRatings Scale")
text(distance_mds,
     rownames(distance_mds),
     col="maroon",
     cex=0.7)



# 데이터 분석(범주형 데이터 포함)-----------------------------------------------
library(cluster)

#-------------------------------------------------------------------------------
?cluster::daisy # {cluster} Dissimilarity Matrix Calculation

# daisy(x, metric = c("euclidean", "manhattan", "gower"),
#       stand = FALSE, type = list(), weights = rep.int(1, p),
#       warnBin = warnType, warnAsym = warnType, warnConst = warnType,
#       warnType = TRUE)
#-------------------------------------------------------------------------------
str(mtcars)

mtcars_dis = cluster::daisy(mtcars, metric="euclidean")
View(as.matrix(mtcars_dis))

library(MASS)
mtcars_mds = MASS::isoMDS(mtcars_dis)
str(mtcars_mds)


plot(mtcars_mds$points,
     type="n",
     main="mtcars")
text(mtcars_mds$points,
     rownames(distance_mds),
     col="maroon",
     cex=0.7)





