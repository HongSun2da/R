################################################################################
# 통계분석[Statistical Analysis]
#         차이검정
#                 t-검정   - 2개
#                 (t-test)      
#                               일표본 t-검정   - 1개             [One Sample t-test]
#                               독립표본 t-검정 - 2개             [Independent Sample t-test]
#                               대응표본 t-검정 - 반복(2)         [Paired Sample t-test]
#                 분산분석 - 3개
#                 (ANOVA - Analysis of Variance)       
#                               일원 분산분석     - 3개           [One Way ANOVA]
#                               반복측정 분산분석 - 반복(3)       [Repeated Measures ANOVA]
#                               이원 분산분석     - 요인 + 요인   [Two Way ANOVA] - 상호작용 확인
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
################################################################################

#-------------------------------------------------------------------------------
# 통계분석 > 차이검정 > 교차분석(Chi Square) > 카이제곱
#-------------------------------------------------------------------------------


# 1. -------------------------------
# 데이터 수집
train = read.csv("./data/chi_pre.csv",
                 header=TRUE,
                 na.strings = ".")


train$group = factor(train$group,
                       level=c(1,2),
                       label=c("Vit", "NoVit"))

train$cold = factor(train$cold,
                      level=c(1,2),
                      label=c("NoCold", "Cold"))


# 데이터 이해
View(train)
str(train)   # 'data.frame':	100 obs. of  2 variables:


# 기술통계
#install.packages("psych")
library(psych)
psych::describe(train)

#       vars   n mean  sd median trimmed  mad min max range skew kurtosis   se
# group    1 100 1.50 0.5    1.5    1.50 0.74   1   2     1  0.0    -2.02 0.05
# cold     2 100 1.55 0.5    2.0    1.56 0.00   1   2     1 -0.2    -1.98 0.05

# 카이스케어 분석
# install.packages("gmodels")
library(gmodels)

tb_train = table(train$group, train$cold)

#       NoCold Cold
# Vit       33   17
# NoVit     12   38

model = gmodels::CrossTable(tb_train,
                         expected = TRUE,
                         chisq = TRUE,
                         asresid = F)


# Total Observations in Table:  100 
# 
#       |    NoCold |      Cold | Row Total | 
# ------|-----------|-----------|-----------|
#   Vit |        33 |        17 |        50 | 
#       |    22.500 |    27.500 |           | => 예상 결과
# ------|-----------|-----------|-----------|
# NoVit |        12 |        38 |        50 | 
#       |    22.500 |    27.500 |           | => 예상 결과 
# ------|-----------|-----------|-----------|
# Total |        45 |        55 |       100 | 
#       |     0.450 |     0.550 |           | 
# ------|-----------|-----------|-----------|
# 
# Pearson's Chi-squared test 
# ------------------------------------------------------------
# Chi^2 =  17.81818     d.f. =  1     p =  2.430496e-05 ★★★  동질 하시 안다.
# 
# Pearson's Chi-squared test with Yates' continuity correction 
# ------------------------------------------------------------
# Chi^2 =  16.16162     d.f. =  1     p =  5.816079e-05 


# 수정된 표준잔차
model$chisq$stdres

#         NoCold      Cold
# Vit    4.221159 -4.221159
# NoVit -4.221159  4.221159





# 2. -------------------------------
# 데이터 수집
train = read.csv("./data/chi_post.csv",
                 header=TRUE,
                 na.strings = ".")

train$cancer = factor(train$cancer,
                        level=c(1,2),
                        label=c("No", "Yes"))

train$smoking = factor(train$smoking,
                         level=c(1:5),
                         label=c("NoSmoking", "longNS", "sotNS", "reSmoking", "Smoking"))

# 데이터 이해
View(train)
str(train)   # 'data.frame':	10 obs. of  3 variables:


# 기술통계
#install.packages("psych")
library(psych)
psych::describe(train)

#          vars  n    mean       sd  median trimmed      mad min    max  range skew kurtosis       se
# cancer*     1 10     1.5     0.53     1.5     1.5     0.74   1      2      1  0.0    -2.19     0.17
# smoking*    2 10     3.0     1.49     3.0     3.0     1.48   1      5      4  0.0    -1.62     0.47
# count       3 10 32853.0 52567.56 14253.5 19668.0 20621.48 319 170867 170548  1.7     1.83 16623.32


# 카이스케어 분석
# install.packages("gmodels")
library(gmodels)

# table 형으로 변화
tb_train = xtabs(count ~ cancer + smoking, data = train)

#        smoking
# cancer  NoSmoking longNS  sotNS reSmoking Smoking
#     No     170867  51690  46598     29178   27784
#     Yes       723    370    497       319     504

model = gmodels::CrossTable(tb_train,
                            expected = TRUE,
                            chisq = TRUE,
                            asresid = F)

#         | smoking 
# cancer  | NoSmoking |    longNS |     sotNS | reSmoking |   Smoking | Row Total | 
# --------|-----------|-----------|-----------|-----------|-----------|-----------|
#      No |    170867 |     51690 |     46598 |     29178 |     27784 |    326117 | 
#         |170329.699 | 51677.628 | 46749.095 | 29280.349 | 28080.229 |           | 
# --------|-----------|-----------|-----------|-----------|-----------|-----------|
#     Yes |       723 |       370 |       497 |       319 |       504 |      2413 | 
#         |  1260.301 |   382.372 |   345.905 |   216.651 |   207.771 |           | 
#         |   229.066 |     0.400 |    66.000 |    48.351 |   422.349 |           | 
# --------|-----------|-----------|-----------|-----------|-----------|-----------|
#   Total |    171590 |     52060 |     47095 |     29497 |     28288 |    328530 | 
#         |     0.522 |     0.158 |     0.143 |     0.090 |     0.086 |           | 
# --------|-----------|-----------|-----------|-----------|-----------|-----------|
#   
#   
#   Statistics for All Table Factors
# 
# 
# Pearson's Chi-squared test 
# ------------------------------------------------------------
# Chi^2 =  771.8354     d.f. =  4     p =  9.67611e-166 

# 수정된 표준잔차
model$chisq$stdres

#         smoking
# cancer   NoSmoking      longNS       sotNS   reSmoking     Smoking
#     No   21.9786982   0.6922651  -8.8098850  -7.3153225 -21.5768568
#     Yes -21.9786982  -0.6922651   8.8098850   7.3153225  21.5768568




