
wgt = read.csv("0401.wgt.csv",
               header=TRUE,
               na.strings=".")
wgt
summary(wgt)

wgt$sex = factor(wgt$sex,
                 levels=c(1,2),
                 labels=c("남자", "여자"))
str(wgt)


# 기하평균
# 식으로 계산
(2363/635)^(1/4) - 1

# 비율로 계산
cagr = c(1.5716, 1.2675,1.3446, 1.3885)
prod(cagr)^(1/length(cagr)) - 1

# install.packages("psych")
library(psych)

# psych 통계패키지 
geometric.mean(cagr) - 1  


# 조화평균
hm = c(400, 100)
1/mean(1/hm)


###########################################
# 05. 분포
summary(wgt$weight)
wgt

min(wgt$weight)
max(wgt$weight)
diff(range(wgt$weight)) # max - min
quantile(wgt$weight, c(0.25, 0.5, 0.75, 1.0))
var(wgt$weight)     #분사
sd(wgt$weight)      #표준편차
skew(wgt$weight)    # 왜도
kurtosi(wgt$weight) # 측도

boxplot(wgt$weight)
hist(wgt$weight)

###########################################
# 06. 표준화

wgt = transform(wgt,
                weight.z = scale(weight))
wgt
summary(wgt)

var(wgt$weight.z)     #분사
sd(wgt$weight.z)      #표준편차

boxplot(wgt$weight.z)
hist(wgt$weight.z)

###########################################
# packages 이용
describe(wgt)

wgt = wgt[!(wgt$weight > 80), ]
describe(wgt)

summary(wgt)

#weight      2 62 58.0 11.67  56.50   56.82  8.15 40.00 120.00 80.00 2.61    11.19 1.48
#weight.z    4 62  0.0  1.00  -0.13   -0.10  0.70 -1.54   5.31  6.85 2.61    11.19 0.13
#          vars  n  mean    sd median trimmed   mad   min  max range skew kurtosis   se
#weight      2 60 56.43  7.47  56.00   56.35  7.41 40.00 72.0 32.00 0.07    -0.53 0.96
#weight.z    4 60 -0.13  0.64  -0.17   -0.14  0.64 -1.54  1.2  2.74 0.07    -0.53 0.08


boxplot(wgt$weight.z)
hist(wgt$weight.z)

###########################################
# 09. 그룹간 연속변수 특성 비교
tapply(wgt$weight, wgt$sex, mean)
tapply(wgt$weight, wgt$sex, summary)

describeBy(wgt[c("weight")], wgt$sex, mat=TRUE)
describeBy(wgt$weight, wgt$sex, mat=TRUE)

describeBy(wgt$weight, wgt$sex, mat=FALSE)






