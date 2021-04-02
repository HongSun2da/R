## 확률분포    #################################################################
#           확률밀도함수  활률분포함수  백분위수함수  난수생성함수
# 이항분포  dbinom()      pbinom()      qbinom()      rbinom()
# 정규분포  dnorm()       pnorm()       qnorm()       rnorm()
# t분포     dt()          pt()          qt()          rt()
# F분포     df()          pf()          qf()          rf()
# x2분포    dchisq()      pchisq()      qchisq()   data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==   rchisq()
# 일양분포  dunif()       punif()       qunif()       runif()
# 
# 
# 밀도 함수       (Density function)
# 누적 분포 함수  (Cumulative distribution function)
# 분위수 함수     (Quantile function)
# 난수 발생       (Random number generation)






# 01. 이항분포  dbinom()      pbinom()      qbinom()      rbinom()  ------------
dbinom(7, size = 10, prob = 0.5) # 동전(0.5)을 10번 실행시 한면이 7번 나올 확률?

pbinom(7, size = 10, prob = 0.5) # 동전(0.5)을 10번 실행시 한면이 7번 이하로 나올 확률?

dbinom(0:7, size = 10, prob = 0.5) # 0 ~ 7 각가

sum(dbinom(0:7, size = 10, prob = 0.5)) # pbinom(7, size = 10, prob = 0.5) 동일

pbinom(7, size = 10, prob = 0.5, lower.tail = FALSE) # 동전(0.5)을 10번 실행시 한면이 8번 이상로 나올 확률?

pbinom(7, size = 10, prob = 0.5) - pbinom(3, size = 10, prob = 0.5) # 동전(0.5)을 10번 실행시 한면이 4 ~ 7번 나올 확률?

pbinom(c(3,7), size = 10, prob = 0.5) # 3번 이하, 7번 이하 각각 나올 활률

diff(pbinom(c(3,7), size = 10, prob = 0.5)) # 동전(0.5)을 10번 실행시 한면이 4 ~ 7번 나올 확률?


set.seed(1)
rbinom(1, size = 10, prob = 0.5)

rbinom(20, size = 10, prob = 0.5)

hist(rbinom(500, size = 10, prob = 0.5))



# 02. 정규분포  dnorm()       pnorm()       qnorm()       rnorm()    -----------

pnorm(110, mean = 100, sd = 10) # mean = 100 이고 sd = 10인 정규분포에서 110이하인 확률?

pnorm(110, mean = 100, sd = 10, lower.tail = FALSE) # mean = 100 이고 sd = 10인 정규분포에서 110초가할 확률?

# pnorm(0) == pnorm(0, mean = 0, sd = 1)

pnorm(110, mean = 100, sd = 10) - pnorm(90, mean = 100, sd = 10) # 90 ~ 110 사이일 확률 ?

diff(pnorm(c(90,110), mean = 100, sd = 10))

qnorm(0.05, mean = 100, sd = 10)  # mean = 100 이고 sd = 10인 정규분포에서 5%이하인 수는 ?

qnorm(0.95, mean = 100, sd = 10)  # 전체중 95%이하인 수는 ?

qnorm(c(0.05, 0.95), mean = 100, sd = 10)

qnorm(0.025)
qnorm(0.975)

qnorm(c(0.025, 0.975))

set.seed(123)
rnorm(1, mean = 100, sd = 10)

rnorm(500, mean = 100, sd = 10)

hist(rnorm(500, mean = 100, sd = 10))

hist(rnorm(500))


rnorm(3, mean = c(-10, 0, 10), sd = 1)

rnorm(6, mean = c(-10, 0, 10), sd = 1)

hist(rnorm(500, mean = c(-10, 0, 10), sd = 1))

# 정규 분포 확인 하기
set.seed(123)

shapiro.test(rnorm(100, mean = 100, sd = 10))
shapiro.test(runif(100, min = 2, max = 4))

hist(rnorm(500, mean = 100, sd = 10))
hist(runif(500, min = 2, max = 4))



# qqnorm (Quantile-Quantile Plots)

qqnorm(rnorm(100, mean = 100, sd = 10), col="red")
qqline(rnorm(100, mean = 100, sd = 10))


qqnorm(runif(100, min = 2, max = 4), col="red")
qqline(runif(100, min = 2, max = 4))










