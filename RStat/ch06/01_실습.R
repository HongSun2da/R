#########################################
## 확률 분표 - 이산형
# 확률밀도함수       : d~
# 누적분포함수(점수) : p~
# 누적부포함수(확률) : q~ 

#########################################
# 01.이항 분포
# 이항분포 확률밀도
# dbionom(성공회수, size(시행횟수(n)), prob(성공확률))

# fortune 쿠키 성공사례
# 시행횟수 : 3명
# 성공활률 : 90%
# P(X=a) 확률계산

dbinom(0, size=3, prob=0.9)
dbinom(1, size=3, prob=0.9)
dbinom(2, size=3, prob=0.9)
dbinom(3, size=3, prob=0.9)

# 이항분포 활률밀도 그래프

yd = dbinom(0:3, size=3, prob=0.9)
# yd = [0.001 0.027 0.243 0.729]

plot(0:3,
     yd,
     type="h",
     lwd=10,
     col="red",
     ylab="Prob",
     xlab="Size",
     main="Binom Dist")

# 이항분포 누적확률계산
# 이하 : P(X <= a) lower.tail = TRUE
# 초과 : P(X > a) lower.tail = FALSE

# P(X <= 1)이하로 받을 확률
pbinom(1, size=3, prob=0.9, lower.tail = TRUE)

# P(X <= 2)이하로 받을 확률
pbinom(2, size=3, prob=0.9, lower.tail = TRUE)

# P(X > 2)이하로 받을 확률
pbinom(1, size=3, prob=0.9, lower.tail = FALSE)


yp = pbinom(0:3, size=3, prob=0.9)
# yp = [0.001 0.028 0.271 1.000]
# yd = [0.001 0.027 0.243 0.729]

plot(0:3,
     yp,
     type="h",
     lwd=10,
     col="red",
     ylab="Prob",
     xlab="Size",
     main="Binom Dist")

# 이항분포 퀴즈
db = dbinom(0:10, size=10, prob=0.7)
plot(0:10,
     db,
     type="h",
     lwd=10,
     col="red",
     ylab="Prob",
     xlab="Size",
     main="Binom Dist")

pb = pbinom(0:10, size=10, prob=0.7)

# db = [0.0000059049 0.0001377810 0.0014467005 0.0090016920 0.0367569090 0.1029193452 0.2001209490 0.2668279320 0.2334744405 0.1210608210 0.0282475249]
# pb = [0.0000059049 0.0001436859 0.0015903864 0.0105920784 0.0473489874 0.1502683326 0.3503892816 0.6172172136 0.8506916541 0.9717524751 1.0000000000]

plot(0:10,
     pb,
     type="h",
     lwd=10,
     col="red",
     ylab="Prob",
     xlab="Size",
     main="Binom Dist")


#########################################
# 02.포아송 분포(Poisson distribution)
# dpois(발생건수, lambda(단위시간당 평균발생건수))

# 서비스 사례
# 평균발생건수 : 1.5회
# P(x=a) 확률 계산


dpois(0, lambda = 1.5)
dpois(1, lambda = 1.5)
dpois(2, lambda = 1.5)
dpois(3, lambda = 1.5)
dpois(4, lambda = 1.5)
dpois(5, lambda = 1.5)

dp = dpois(c(0:10), lambda = 1.5)
dp

plot(dp,
     lambda=1.5,
     type="h",
     lwd=5,
     col="red")

# 포아송분포 누적확률계산
# 이하 : P(X <= a) lower.tail = TRUE
# 초가 : P(X > a) lower.tail = FALSE

# 2회 이상 받을 확률
ppois(q=1, lambda = 1.5, lower.tail = FALSE)

pp = ppois(0:10, lambda = 1.5, lower.tail = FALSE)

# 포아송분포 누적확률
plot(pp,
     lambda=1.5,
     type="h",
     lwd=5,
     col="red")


#########################################
# 03.지수 분포(Exponential Distribution)
# pexp(q=대기시간, rate=평균발생건수(lambda))

# 확률밀도함수       : d~
# 누적분포함수(점수) : p~
# 누적부포함수(확률) : q~ 

# 서비스센터 사례
# 평균발생건수 : 5분에 1.5회 => 기준시간은 5분

# 1분 p(X <= 1.5) 이내로 받을 확률

pexp(q=1/5, rate=1.5, lower.tail = TRUE)
# 1분 이내에 전화올 확률은 0.2591818, 25% 입니다.


# 95%확률로 받을수 있는 시간은 

qexp(p=0.95, rate=1.5)
# 95%확률로 받을수 있는 시간은 1.997155, 약 10분안에 받을수 있습니다.


# 지수분포 누적확률 그래프
x = seq(0, 3, length=50)
y = dexp(x, 1.5)

plot(x, y, type="l")
abline(v=0.2, h=0.3, col="red", lty=3)     




#########################################
# 04.정규 분포(Normal Distribution)
# 난수 함수 : rnorm(n, mean=0, sd=1)

x = rnorm(100, 50, 20)
x = sort(x)

y = dnorm(x, 50, 20)

plot(x,
     y,
     type="l")
x
y

# 누적분포함수(점수)
# 평균이 50 이고 표준편차 20 에서

pnorm(60, mean = 50, sd=20, lower.tail = TRUE)
# 해석 => 60점이하는 몇 %에 포함되는가? 0.6914625 69% 


pnorm(60, mean = 50, sd=20, lower.tail = FALSE)
# 해석 => 60점이상은 몇 %에 포함되는가? 0.3085375 31% 


# 누적분포함수(확률)
# 상위 35% 안에 들어가려면

qnorm(0.35, mean = 50, sd=20, lower.tail = FALSE)
# 해석 => 상위 35%안에 들어가기 위해 점수 57.70641 (58점) 이상을 받아야 한다.

qnorm(0.35, mean = 50, sd=20, lower.tail = TRUE)
# 해석 => 하위 35%안에 들어가기 위해 점수 42.29359 (42점) 이하를 받아야 한다.







