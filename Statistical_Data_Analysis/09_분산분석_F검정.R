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

# F 검정                        ------------------------------------------------

# F 분포 -> 오른쪽 긴 꼬리를 가지고 있음
#        -> (집단 간 분산) / (집단 내 분산) 
#        -> 보통 범위는 0 ~ 5.0 까지

# df(x, df1, df2, ncp, log = FALSE)
# pf(q, df1, df2, ncp, lower.tail = TRUE, log.p = FALSE)
# qf(p, df1, df2, ncp, lower.tail = TRUE, log.p = FALSE)
# rf(n, df1, df2, ncp)

# 1group 자유도[2-1] 2group 자유도[(5-1) + (5-1)] 이고 f=9.59 이상 발생할 활률은 ?

pf(9.59, df1=1, df=8, lower.tail = FALSE)   # 0.0147376


qf(0.05, df1=1, df2=8)  # f 분포 0.05% (왼쪽)지점의 값은 ? 

qf(0.05, df1=1, df2=8, lower.tail = FALSE)  # f 분포 0.05% (오른쪽)지점의 값은 ? 







