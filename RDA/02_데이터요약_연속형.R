
library(MASS)

View(survey)
str(survey)
summary(survey$Pulse)


# 중앙값
median(survey$Pulse, na.rm = TRUE)  # 72.5

# 평균
mean(survey$Pulse, na.rm = TRUE)  # 74.15104
sum(survey$Pulse)

# 백분위 수 : ?quantile

## Default S3 method:
#  quantile(x, probs = seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7, ...)
quantile(survey$Pulse, na.rm = TRUE)


quantile(survey$Pulse, probs = 0.05, na.rm = TRUE)  
quantile(survey$Pulse, probs = 0.5, na.rm = TRUE)     # 50%

# 백분위 수 : 1st quantile => Q1, 3st quantile => 3Q
quantile(survey$Pulse, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)


# 비율
survey$Pulse <= 80

mean(survey$Pulse <= 80, na.rm = TRUE) # 0.7552083(%)


str(iris)
View(iris)

summary(iris$Sepal.Length)

summary(iris)

summary(as.character(iris$Species))

summary(as.list(iris))

iris_ls = as.list(iris)

lapply(as.list(iris), summary)

lapply(iris_ls[1], summary)

?lapply


range(survey$Pulse, na.rm = TRUE) # 35 104

min(survey$Pulse, na.rm = TRUE) # 35
max(survey$Pulse, na.rm = TRUE) # 104

var(survey$Pulse, na.rm = TRUE)
sd(survey$Pulse, na.rm = TRUE)


str(mtcars)
View(mtcars)

# install.packages("pastecs")
library(pastecs)

stat.desc(mtcars)

stat.desc(mtcars[c("mpg", "hp", "wt")])

stat.desc(survey)

# install.packages("psych")
library(psych)

describe(mtcars)

describe(survey)

pairs.panels(mtcars)

# 집단별 내용 확인
str(survey)
levels(survey$Exer) # "Freq" "None" "Some"

# tapply(X, INDEX, FUN = NULL, ..., default = NA, simplify = TRUE)
tapply(survey$Pulse, INDEX = survey$Exer, FUN = mean, na.rm = TRUE) # 열단위

tapply(survey$Pulse, INDEX = survey$Sex, FUN = mean, na.rm = TRUE) # 열단위

tapply(survey$Pulse, 
       INDEX = list(survey$Exer, survey$Sex), 
       FUN = mean, 
       na.rm = TRUE) # 행열 단위

aggregate(survey$Pulse, 
          by = list(Exer=survey$Exer, Sex=survey$Sex), 
          FUN = mean, 
          na.rm = TRUE)

aggregate(survey[c("Pulse", "Age", "Height")], 
          by = list(Exer=survey$Exer, Sex=survey$Sex), 
          FUN = mean, 
          na.rm = TRUE)

# 사용자 정의 함수
myStats = function(x, na.rm=FALSE){
  if(na.rm) x = x[!is.na(x)]
  n = length(x)
  mean = mean(x)
  sd = sd(x)
  skew = sum((x-mean)^3/sd^3)/n
  kurt = sum((x-mean)^4/sd^4)/n - 3
  return(c(n=n,
           mean=mean,
           sd=sd,
           skewness=skew,
           durtosis=kurt))
}

# 사용자 정의 함수 사용
aggregate(survey[c("Pulse", "Age")], 
          by = list(Exer=survey$Exer), 
          FUN = myStats, na.rm=TRUE)

by(survey[c("Pulse", "Age")],
   INDICES = list(Exer=survey$Exer), 
   FUN = summary)

aggregate(survey[c("Pulse", "Age")], 
          by = list(Exer=survey$Exer), 
          FUN = summary)



describeBy(survey[c("Pulse", "Age")],
           group = list(Exer=survey$Exer))







