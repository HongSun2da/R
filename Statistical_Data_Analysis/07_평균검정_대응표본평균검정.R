## 평균 검정 (t.test)         ##################################################

# - 1. 일표본 평균 검정
# - 2. 독립표본 평균 검정
# - 3. 대응표본 평균 검정
################################################################################

# - 3. 대응표본 평균 검정
# 두 표본의 값이 쌍(pair)을 이루고 있는 경우에 쌍을 이룬 값은 서로 독립이 아니며


View(sleep)

str(sleep)
summary(sleep)

library(psych)
describe(sleep)
describeBy(sleep$extra, sleep$group, mat = T)




# 1 group과 2 group의 차이는 0 이다. - 귀무가설

t.test(extra ~ group, data = sleep, paired = TRUE) # Paired t-test

# t = -4.0621, df = 9, p-value = 0.002833 (귀무가설 기각)




library(tidyr)

sleep.wide = spread(sleep, key=group, value = extra)
View(sleep.wide)


t.test(sleep.wide$`1`, sleep.wide$`2`, paired = TRUE)

# t = -4.0621, df = 9, p-value = 0.002833











