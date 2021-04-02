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

# .  다변량 분산분석 (multivariate analysis of variance, MANOVA)----------------
# 

# 데이터 수집                          -----------------------------------------
#install.packages("heplots")
library(heplots)

View(Skulls)



# 기술통계 확인                        -----------------------------------------
str(Skulls)
summary(Skulls)

library(psych)
describe(Skulls)


library(dplyr)
sample_n(Skulls, 10)



# 데이터 분석                          -----------------------------------------

y = cbind(Skulls$mb, Skulls$bh, Skulls$bl, Skulls$nh)
str(y)
class(y)
View(y)
summary(y)

aggregate(y, by=list(Skulls$epoch), mean)

Skulls_manova = manova(y ~ epoch, data=Skulls)
summary(Skulls_manova)

# epoch       4 0.35331    3.512     16    580 4.675e-06 *** (귀무가설 기각)

summary.aov(Skulls_manova)


















