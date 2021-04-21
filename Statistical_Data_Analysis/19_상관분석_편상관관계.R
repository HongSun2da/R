## 편상관관계(partial correlation coefficient)##################################

# 두 변수 간의 순수한 상관관계
# - 가짜 상관을 찾아내는 데 활용(예:연봉과 혈압 ~ 나이)
# - 숨겨진 관계를 찾는데 활용(예:구매필요성과 구매의향 ~ 소득)


# 데이터 수집                                 ----------------------------------
View(mtcars)

class(mtcars)
str(mtcars)




# 기술 통계                                   ----------------------------------
summary(mtcars)

library(psych)
describe(mtcars)

pairs.panels(mtcars)

library(corrgram)
corrgram(mtcars)




# 데이터 분석                                 ----------------------------------

mtcars2 = mtcars[, c("mpg","cyl","hp","wt")]
head(mtcars2)

cor(mtcars2)

install.packages("graph") # 설치 안됨
install.packages("ggm")   # 설치 안됨
library(ggm)

pcor(c("mpg","hp","cyl","wt"), cov(mtcars2))



?pcor # Polychoric correlation

# pcor(x, y, X, start, ...)

# install.packages("ppcor")
library(ppcor)

pcor(mtcars2)

?pcor # Partial correlation

# pcor(x, method = c("pearson", "kendall", "spearman"))


pcor.test(mtcars2["mpg"],mtcars2["hp"], mtcars2[c("cyl","wt")])


?pcor.test # Partial correlation for two variables given a third variable.

# pcor.test(x, y, z, method = c("pearson", "kendall", "spearman"))
