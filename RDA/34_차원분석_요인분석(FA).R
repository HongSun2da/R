## 차원분석  ###################################################################
# 1. 주성분분석 (principal component analysis, PCA)
# 2. 요인분석 (factor analysis, FA)
# 3. 다차원척도법

################################################################################
# 2. 요인분석 (factor analysis, FA)
# - 고유값
# - 스크리도표
# - 시뮬레이션
# - 설명되는 분산의 비율



################################################################################

# 데이터 수집                                    -------------------------------
#install.packages("ade4")
library(ade4)

data(olympic)
View(olympic$tab)
View(olympic$score)

str(olympic)


?olympic
# Example 357 in:
#   Hand, D.J., Daly, F., Lunn, A.D., McConway, K.J. and Ostrowski, E. (1994) A handbook of small data sets, Chapman & Hall, London. 458 p.




# 기술 통계      ---------------------------------------------------------------
library(psych)

psych::describe(olympic$tab)
psych::pairs.panels(olympic$tab)


# 데이터 전처리  ---------------------------------------------------------------




# 데이터 분석    ---------------------------------------------------------------
library(psych)

#-------------------------------------------------------------------------------
?psych::fa.parallel # {psych} Scree plots of data or correlation matrix compared to random “parallel" matrices

# fa.parallel(x,n.obs=NULL,fm="minres",fa="both",nfactors=1, 
#             main="Parallel Analysis Scree Plots",
#             n.iter=20,error.bars=FALSE,se.bars=FALSE,SMC=FALSE,ylabel=NULL,show.legend=TRUE,
#             sim=TRUE,quant=.95,cor="cor",use="pairwise",plot=TRUE,correct=.5)
# 
# fa.parallel.poly(x ,n.iter=10,SMC=TRUE,  fm = "minres",correct=TRUE,sim=FALSE,
#                  fa="both",global=TRUE)   #deprecated
# 
# ## S3 method for class 'poly.parallel'
# plot(x,show.legend=TRUE,fa="both",...)
#-------------------------------------------------------------------------------

psych::fa.parallel(olympic$tab,
                   fm="ml",
                   fa="fa",
                   n.iter=100)



# install.packages("nFactors")
library(nFactors) 

#-------------------------------------------------------------------------------
?nFactors::nScree # {nFactors} Non Graphical Cattel's Scree Test

# nScree(eig = NULL, x = eig, aparallel = NULL, cor = TRUE,
#        model = "components", criteria = NULL, ...)
#-------------------------------------------------------------------------------

nFactors::nScree(olympic$tab)

#   noc naf nparallel nkaiser
# 1   2   2         2       2


#-------------------------------------------------------------------------------
?factanal # {stats} Factor Analysis

# factanal(x, factors, data = NULL, covmat = NULL, n.obs = NA,
#          subset, na.action, start = NULL,
#          scores = c("none", "regression", "Bartlett"),
#          rotation = "varimax", control = NULL, ...)
#-------------------------------------------------------------------------------


fa = factanal(olympic$tab,
               factors=2,
               scores="regression")
fa


# 공통성
round(1 - fa$uniquenesses, 3)

#-------------------------------------------------------------------------------
?psych::factor.plot # {psych} Plot factor/cluster loadings and assign items to clusters by their highest loading.

# cluster.plot(ic.results, cluster = NULL, cut = 0, labels=NULL,
#              title = "Cluster plot",pch=18,pos,show.points=TRUE,choose=NULL,...)
# fa.plot(ic.results, cluster = NULL, cut = 0, labels=NULL,title, 
#         jiggle=FALSE,amount=.02,pch=18,pos,show.points=TRUE,choose=NULL,main=NULL,...)
# factor.plot(ic.results, cluster = NULL, cut = 0, labels=NULL,title,jiggle=FALSE,
#             amount=.02,pch=18,pos,show.points=TRUE,...)  #deprecated
#-------------------------------------------------------------------------------
psych::factor.plot(fa,
                   labels=colnames(olympic$tab),
                   pos=4,
                   main="Factor Plot")

install.packages("gplots")
library(gplots)

library(RColorBrewer)

#-------------------------------------------------------------------------------
?gplots::heatmap.2 # {gplots} Enhanced Heat Map
#-------------------------------------------------------------------------------

abs(fa$loadings)

gplots::heatmap.2(abs(fa$loadings),
                  col=brewer.pal(9, "Blues"),
                  trace="none",
                  key=FALSE,
                  dendrogram = "none",
                  cexCol=1.2,
                  main="Factor Loadings")

install.packages("semPlot")
library(semPlot)

?semPlot::semPaths





