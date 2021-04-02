#####################################################################################################
# 통계분석
#         차이검정
#                 t-검정   - 2개
#                 (t-test)      
#                               일표본 t-검정   - 1개             [One Sample t-test]
#                               독립표본 t-검정 - 2개             [Independent Sample t-test]
#                               대응표본 t-검정 - 반복(2)         [Paired Sample t-test]
#                 분산분석 - 3개
#                 (ANOVA)       
#                               일원 분산분석     - 3개           [One Way ANOVA]
#                               반복측정 분산분석 - 반복(3)       [Repeated Measures ANOVA]
#                               이원 분선분석     - 요인 + 요인   [Two Way ANOVA]
#                               이원 반복측정 분산- 반복 + 요인   [Two Way Repeated Measures ANOVA]
#         관계검정
#                 교차분석 - (명목)
#                 (Chi Square)
#                 상관분석 - (1:1)
#                 (Correlation)
#                 회귀분석 - (1:N)
#                 (Regression)  
#                               단순회귀분석      - 1:1종속(연속) [Linear Regression]
#                               다중회귀분석      - 1:N종속(연속) [Multiple Linear Regression]
#                               로지스틱 회귀분석 - 1:N종속(명목) [Logistic Regression]
#####################################################################################################


#---------------------------------------------------------------------------------------------------
# 통계분석 > 차이검정 > t-검정> 일표본 t-검정   - 1개             [One Sample t-test]

library(psych)

ost.df = read.csv("Ch0701.OST.csv",
                  header=TRUE,
                  na.strings = ".")

# 기술통계
str(ost.df)
ost.df = round(ost.df, 2)
describe(ost.df)


opar = par(no.readonly = TRUE)
par(mfrow=c(1,2))

boxplot(ost.df$weight)
hist(ost.df$weight,
     breaks = 10,
     col="red")

par(opar)

# t-test
t.test(ost.df,
       alternative = c("two.sided"),
       mu=320,
       conf.level = 0.95)

#---------------------------------------------------
  
satisfaction.df = read.csv("Ch0702.satisfaction.csv",
                  header=TRUE,
                  na.strings = ".")
# 기술통계
str(satisfaction.df)  
summary(satisfaction.df)  
  
describe(satisfaction.df)

# 그래프
opar = par(no.readonly = TRUE)
par(mfrow=c(1,2))

boxplot(satisfaction.df$satis)
hist(satisfaction.df$satis,
     breaks = 10,
     col="red",
     xlab = "Jumsu",
     ylab="Gasu",
     main="manjodo Jumsu")

par(opar)

# t-test
t.test(satisfaction.df$satis,
       alternative = c("two.sided"),
       mu=50,
       conf.level = 0.95)

#통계 결과그래프
mu=50
se=1.21
inter = qt(p=0.025, df=199) # -1.971957
data = rnorm(1000, mu, se)
data = sort(data)

plot(data,
     dnorm(data, mu, se),
     type="l")
abline(v=mu, col="green", lty=5)          #평균
abline(v=mu+inter*se, col="blue", lty=5)  #
abline(v=mu-inter*se, col="blue", lty=5)  #
abline(v=49.98, col="red", lty=5)


#---------------------------------------------------

calorie.df = read.csv("Ch0703.calorie.csv",
                           header=TRUE,
                           na.strings = ".")

describe(calorie.df)


# 그래프
opar = par(no.readonly = TRUE)
par(mfrow=c(1,2))

boxplot(calorie.df$cal)
hist(calorie.df$cal,
     breaks = 10,
     col="red",
     xlab = "Jumsu",
     ylab="Gasu",
     main="manjodo Jumsu")

par(opar)

# t-test
t.test(calorie.df$cal,
       alternative = c("less"),
       mu=500,
       conf.level = 0.95)



#---------------------------------------------------------------------------------------------------
# 통계분석 > 차이검정 > t-검정> 독립표본 t-검정 - 2개             [Independent Sample t-test]
# 등분산, 이분산 구분하기 -> var.test()

ist = read.csv("Ch0801.IST.csv",
               header=TRUE,
               na.strings = ".")
ist
str(ist)
summary(ist)

ist$t_group = factor(ist$t_group,
                     level=c(1,2),
                     labels=c("ACAR_T", "BCAR_T"))

# 기술 통계
describeBy(ist$t_time, ist$t_group, mat=TRUE)

# 그래프 그리기
opar = par(no.readonly = TRUE)

layout(matrix(c(1,1,2,3), 2,2, byrow=TRUE))

boxplot(ist$t_time ~ ist$t_group)
hist(ist$t_time[ist$t_group=="ACAR_T"])
hist(ist$t_time[ist$t_group=="BCAR_T"])


#등분산 검정  F = 0.73845, num df = 29, denom df = 29, p-value = 0.4192
var.test(ist$t_time ~ ist$t_group, data=ist)


#t-test
t.test(ist$t_time ~ ist$t_group,
       data=ist,
       alternative=c("two.sided"),
       var.equal=TRUE,
       conf.level=0.95)

par(opar)


# 통계 결과 그래프 그리기
# A CAR T
x = 48670.57
se = 658.5667

x_data = rnorm(1000, x, se)
x_data = sort(x_data)

y_data = dnorm(x_data, x, se)

plot(x_data,
     y_data,
     col="blue",
     type="l",
     xlim=c(45000, 55000),
     ylim=c(0,0.0006),
     main="Car Company T-Life")


par(new=T)

# B CAR T
x = 51377.60
se = 766.3734

x_data = rnorm(1000, x, se)
x_data = sort(x_data)

y_data = dnorm(x_data, x, se)

plot(x_data,
     y_data,
     col="red",
     type="l",
     xlim=c(45000, 55000),
     ylim=c(0,0.0006),
     main="Car Company T-Life")


# 부록. 비모수통계 분석
shapiro.test(ist$t_time[ist$t_group=="ACAR_T"]) # 정규분포검정
shapiro.test(ist$t_time[ist$t_group=="BCAR_T"])

wilcox.test(t_time ~ t_group, data=ist)


# ggplot 그래프
library(ggplot2)

ggplot(ist, aes(x=ist$t_time))+
  geom_histogram(binwidth=5000) +
  ggtitle("Car Company T Life") +
  facet_grid(. ~ ist$t_group)



#---------------------------------------------------------------------------------------------------
# 통계분석 > 차이검정 > t-검정> 대응표본 t-검정 - 반복(2)         [Paired Sample t-test]

pst = read.csv("Ch0802.PST.csv",
               header=TRUE,
               na.strings = ".")
pst
str(pst)
summary(pst)

# 기술통계
library(psych)
describe(pst)

pst$dif = c(pst$post - pst$pre)
describe(pst)

# 그래프 확인

opar = par(no.readonly = TRUE)

layout(matrix(c(1,2,3,3), 2, 2, byrow=TRUE))

hist(pst$pre, main="per")
hist(pst$post)
boxplot(pst$dif, main="dif")

par(opar)

t.test(pst$post, pst$pre,
       alternative = c("two.sided"),
       paired=TRUE,
       conf.level = 0.95)


# 통계 그래프 결과 확인
x = 0
se = 0.7

inter = qt(p=0.025, df=19)

x_data = rnorm(1000, x, se)
x_data = sort(x_data)

y_data = dnorm(x_data, x, se)

plot(x_data,
     y_data,
     col="red",
     type="l",
     main="Car Company T-Life")
abline(v=mu, col="green", lty=5)






###  1. 연습 문제  ###########################################################################
##############################################################################################

edu = read.csv("Ch0803.edu.csv",
               header=TRUE,
               na.strings = ".")
edu
str(edu)

edu$구분 = factor(edu$구분,
                levels=c(1,2),
                labels=c("재학생","교원"))

# 기술통계
library(psych)
describeBy(edu$종합, edu$구분, mat=T)


# 그래프 확인

opar <- par(no.readonly = TRUE)

layout(matrix(c(1,2,2,3), 2, 2, byrow=TRUE))

boxplot(edu$종합 ~ edu$구분)
hist(edu$종합[edu$구분=="재학생"])
hist(edu$종합[edu$구분=="교원"])

par(opar)


#등분산 검정  F = 1.4423, num df = 48, denom df = 49, p-value = 0.2052
var.test(edu$종합 ~ edu$구분, data=edu)


#t-test
t.test(edu$종합 ~ edu$구분,
       data=edu,
       alternative=c("two.sided"),
       var.equal=TRUE,
       conf.level=0.95)


###  2. 연습 문제  ###########################################################################
##############################################################################################

hp = read.csv("Ch0804.호흡과 뇌파.csv",
               header=TRUE,
               na.strings = ".")
hp
str(hp)
summary(hp)

# 기술통계
library(psych)
describe(hp)


# 그래프 확인
hist(hp$ch1al)
hist(hp$ch1be)

t.test(hp$ch1be, hp$ch1al,
       alternative = c("two.sided"),
       paired=TRUE,
       conf.level = 0.95)












