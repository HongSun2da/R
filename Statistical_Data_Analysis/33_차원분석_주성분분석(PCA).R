## 차원분석  ###################################################################
# 1. 주성분분석(principal component analysis, PCA)
# 2. 요인분석
# 3. 다차원척도법

################################################################################
# 1. 주성분분석(principal component analysis, PCA)
# - 차원 축소 기법
# - 



################################################################################

# 데이터 수집                                    -------------------------------
View(state.x77)

str(state.x77) # num [1:50, 1:8]
head(state.x77)




# 기술 통계      ---------------------------------------------------------------
library(psych)

psych::describe(state.x77)
psych::pairs.panels(state.x77)

# 데이터 전처리  ---------------------------------------------------------------




# 데이터 분석    ---------------------------------------------------------------

#-------------------------------------------------------------------------------
?prcomp # {stats} Principal Components Analysis ★★★★★

# ## S3 method for class 'formula'
# prcomp(formula, data = NULL, subset, na.action, ...)
# 
# ## Default S3 method:
# prcomp(x, retx = TRUE, center = TRUE, scale. = FALSE,
#        tol = NULL, rank. = NULL, ...)
# 
# ## S3 method for class 'prcomp'
# predict(object, newdata, ...)
#-------------------------------------------------------------------------------

pca = prcomp(state.x77, scale=TRUE)
summary(pca)

# Importance of components:
#                           PC1    PC2    PC3     PC4     PC5     PC6     PC7     PC8
# Standard deviation     1.8971 1.2775 1.0545 0.84113 0.62019 0.55449 0.38006 0.33643
# Proportion of Variance 0.4499 0.2040 0.1390 0.08844 0.04808 0.03843 0.01806 0.01415
# Cumulative Proportion  0.4499 0.6539 0.7928 0.88128 0.92936 0.96780 0.98585 1.00000

plot(pca, 
     type="l",
     pch=19,
     col="red",
     main="Scree Plot")

# 성분적재값
round(pca$rotation, 3)

#               PC1    PC2    PC3    PC4    PC5    PC6    PC7    PC8
# Population  0.126  0.411 -0.656 -0.409  0.406 -0.011 -0.062 -0.219
# Income     -0.299  0.519 -0.100 -0.088 -0.638  0.462  0.009  0.060
# Illiteracy  0.468  0.053  0.071  0.353  0.004  0.387 -0.620 -0.339
# Life Exp   -0.412 -0.082 -0.360  0.443  0.327  0.219 -0.256  0.527
# Murder      0.444  0.307  0.108 -0.166 -0.128 -0.325 -0.295  0.678
# HS Grad    -0.425  0.299  0.050  0.232 -0.099 -0.645 -0.393 -0.307
# Frost      -0.357 -0.154  0.387 -0.619  0.217  0.213 -0.472  0.028
# Area       -0.033  0.588  0.510  0.201  0.499  0.148  0.286  0.


# 성분점수
round(scale(state.x77) %*% pca$rotation, 3)

# 성분점수(동일)
round(pca$x, 3)


# 성분간 상관관계 확인 -> 상관계수 0
round(cor(pca$x), 3)

#-------------------------------------------------------------------------------
?biplot # {stats} Biplot of Multivariate Data

# ## Default S3 method:
# biplot(x, y, var.axes = TRUE, col, cex = rep(par("cex"), 2),
#        xlabs = NULL, ylabs = NULL, expand = 1,
#        xlim  = NULL, ylim  = NULL, arrow.len = 0.1,
#        main = NULL, sub = NULL, xlab = NULL, ylab = NULL, ...)
#------------------------------------------------------------------------------- 
biplot(pca,
       cex=c(0.5,0.75),
       main="Biplot") 


biplot(pca)









