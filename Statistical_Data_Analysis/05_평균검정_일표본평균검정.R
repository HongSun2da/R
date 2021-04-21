## 평균 검정 (t.test)          #################################################

# - 1. 일표본 평균 검정
# - 2. 독립표본 평균 검정
# - 3. 대응표본 평균 검정
################################################################################

# - 1. 일표본 평균 검정 ("One Sample t-test")
# 하나의 표본 데이터를 이용하여 모집단의 평균이 특정 값과 같은지 검정
# 표본집단이 특정 모집단과 일치하는지 혹은 그렇지 않은지 알고 싶을 때 이용

?t.test

# ## Default S3 method:
# t.test(x, y = NULL,
#        alternative = c("two.sided", "less", "greater"), => 양측검정, 단측검정
#        mu = 0, paired = FALSE, var.equal = FALSE,
#        conf.level = 0.95, ...)



library(MASS)

View(cats)
str(cats)


# 고양이 몸무게는 2.6Kg 이다. - 귀문가설


t.test(x=cats$Bwt, mu=2.6)

# t = 3.0565, df = 143, p-value = 0.002673 (귀문가설 기각)

# alternative hypothesis: true mean is not equal to 2.6
# 평균이 2.6Kg과 같지 않다 - 대립가설


t.test(x=cats$Bwt, mu=2.7)

# t = 0.58382, df = 143, p-value = 0.5603 (귀문가설 체택)



# 고양이 몸무게는 2.6Kg 작다. - 귀문가설

t.test(x=cats$Bwt, mu=2.6, alternative = "greater")

# t = 3.0565, df = 143, p-value = 0.001337 (귀문가설 채택)
# alternative hypothesis: true mean is greater than 2.6


# t.test 내용 확인하기
cats.t = t.test(x=cats$Bwt, mu=2.6)

str(cats.t)


# 어느 야구팀이 30경기중 18승을 하여 승율이 50% 이상이다 - 귀문가설

# 검정하기 (1-sample proportions test with continuity correction)
prop.test(x=18, n=30, p=0.5, alternative = "greater")

# X-squared = 0.83333, df = 1, p-value = 0.1807 (대리가설 기각 - 귀문가설 채택)




