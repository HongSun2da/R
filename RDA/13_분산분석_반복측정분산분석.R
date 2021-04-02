## 분산분석 (analysis of variance, ANOVA)             ##########################
# 여러 모집단(3개 이상)간의 평균의 동일성 검정 => 분산이동
#
# - 1. 일원분산분석 (one-way ANOVA)
# - 2. 이원분산분석 (two-way ANOVA)
# - 3. 공분산분석 (analysis of covariance, ANCOVA)
# - 4. 반복측정 분산분석 (repeated measures ANOVA)
# - 5. 다변량 분산분석 (multivariate analysis of variance, MANOVA)
# - 6. 다변량 공분산분석 (multivariate analysis of convariance, MANOCOVA)
################################################################################

# . 반복측정 분산분석 (repeated measures ANOVA)---------------------------------
# 반복 일원분석 : y ~ W + Error(Subject/W)
# 반복 이원분석 : y ~ B * W + Error(Subject/W)


# 데이터 수집                  -------------------------------------------------
View(CO2)
str(CO2)
summary(CO2)



# 기술통계 확인                -------------------------------------------------
library(psych)

describe(CO2)
pairs.panels(CO2)




# 전처리                       -------------------------------------------------
CO2sub = subset(CO2, Treatment == "chilled" )
str(CO2sub)

CO2sub$conc = factor(CO2sub$conc)
summary(CO2sub)

View(CO2sub)




# 데이터 분석                  -------------------------------------------------
CO2sub_aov = aov(uptake ~ Type * conc + Error(Plant/conc), data = CO2sub)
summary(CO2sub_aov)

# Error: Plant
#           Df Sum Sq Mean Sq F value  Pr(>F)   
# Type       1 2667.2  2667.2   60.41 0.00148 **  (귀무가설 기각)
# Residuals  4  176.6    44.1                   
# 
# 
# Error: Plant:conc
#           Df Sum Sq Mean Sq F value   Pr(>F)    
# conc       6 1472.4  245.40   52.52 1.26e-12 *** (귀무가설 기각)
# Type:conc  6  428.8   71.47   15.30 3.75e-07 *** (귀무가설 기각)
# Residuals 24  112.1    4.67  



# 그래프 확인 
boxplot(uptake ~ Type * conc, data = CO2sub)

boxplot(uptake ~ Type * conc, data = CO2sub,
        col = c("skyblue", "violet"),
        las=2,
        cex.axis = 0.7,
        xlab = "",
        ylab = "Carbon dioxied uptake rate",
        main ="Effects of Plant Type and CO2 on Carbon")
legend("topleft", 
       inset = 0.02,
       legend = c("Quebed", "Mississippi"),
       fill = c("deepskyblue","violet"))



library(HH)
interaction2wt(uptake ~ Type * conc, data = CO2sub)













