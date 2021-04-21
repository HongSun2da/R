## X2검점(chi-square test)을 이용한            #################################
## 독립성검정(independence test)               #################################

# 두 범주형 변수가 서로 독립인지 검정
# 독립이라는 것은 두 변수가 서로 관련이 없다는 의미
# 귀무가설 : 두 변수는 독립이다.


# 데이터 수집                     ----------------------------------------------
View(Titanic)

str(Titanic)
summary(Titanic)
class(Titanic)

?margin.table() # Compute table margins

# marginSums(x, margin = NULL)
# margin.table(x, margin = NULL)



Titanic_margin = margin.table(Titanic, c("Survived", "Class"))
Titanic_margin

addmargins(Titanic_margin)

addmargins(Titanic_margin, 2)

prop.table(addmargins(Titanic_margin, 2), 2)

addmargins(prop.table(addmargins(Titanic_margin, 2), 2), 1)



# 기술 통계                       ----------------------------------------------
library(psych)



# 데이터 전처리                   ----------------------------------------------
chisq.test(Titanic_margin)

# Pearson's Chi-squared test
# 
# data:  Titanic_margin
# X-squared = 190.4, df = 3, p-value < 2.2e-16 (귀무가설 기각)

# 변숙간 관련성 확인
library(vcd)

assocstats(Titanic_margin)



# 그래프 확인                     ----------------------------------------------
mosaic(Titanic_margin, shade = TRUE)

?mosaic()
# ## Default S3 method:
# mosaic(x, condvars = NULL,
#        split_vertical = NULL, direction = NULL, spacing = NULL,
#        spacing_args = list(), gp = NULL, expected = NULL, shade = NULL,
#        highlighting = NULL, highlighting_fill = rev(gray.colors(tail(dim(x), 1))),
#        highlighting_direction = NULL,
#        zero_size = 0.5, zero_split = FALSE, zero_shade = NULL,
#        zero_gp = gpar(col = 0), panel = NULL, main = NULL, sub = NULL, ...)
# ## S3 method for class 'formula'
# mosaic(formula, data, highlighting = NULL,
#        ..., main = NULL, sub = NULL, subset = NULL, na.action = NULL)

Titanic_margin


library(MASS)
View(survey)

summary(survey)

chisq.test(survey$Fold, survey$Sex)

# Pearson's Chi-squared test
# 
# data:  survey$Fold and survey$Sex
# X-squared = 2.5741, df = 2, p-value = 0.2761 

prop.table(table(survey$Fold, survey$Sex))
















