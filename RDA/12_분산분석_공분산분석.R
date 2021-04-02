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

# . 공분산분석 (analysis of covariance, ANCOVA)---------------------------------
# 분산분석에 공변량(convarite)을 추가하여 분산분석모델을 확장
# 공변량을 통제하여 독립변수의 순수한 영량을 검정
# 공변량은 연속형 변수를 가정


# 데이터 수집
#install.packages("faraway")
library(faraway)

View(sexab)
str(sexab)



# 기술통계 확인
summary(sexab)

?sexab

?tapply # Apply a Function Over a Ragged Array
# tapply(X, INDEX, FUN = NULL, ..., default = NA, simplify = TRUE)

tapply(sexab$ptsd, sexab$csa, length)
tapply(sexab$ptsd, sexab$csa, mean)
tapply(sexab$ptsd, sexab$csa, sd)

library(psych)
describeBy(sexab$ptsd, sexab$csa, mat = T)

describeBy(sexab$cpa, sexab$csa, mat = T)



# 데이터 전처리




# 데이터 분석
# csa 에 따라 ptsd 차이는 0 이다.

sexab_aov = aov(ptsd ~ csa, data = sexab)
summary(sexab_aov)

# csa          1  963.5   963.5    79.9 2.17e-13 *** (귀무가설 기각) -> # csa 에 따라 ptsd 차이는 0 아니다. 차이가 있다.

# 공분산 분석
# csa 와 cpa에 따라 ptsd 차이는 0 이다
sexab_aov = aov(ptsd ~ cpa + csa, data = sexab)
summary(sexab_aov)

# cpa          1  449.8   449.8   41.98 9.46e-09 *** (귀무가설 기각)
# csa          1  624.0   624.0   58.25 6.91e-11 *** (귀무가설 기각)


#install.packages("effects")
library(effects)

effect("csa", sexab_aov)


# 그래프 확인 하기
library(HH)

ancova(ptsd ~ cpa + csa, data = sexab)
















