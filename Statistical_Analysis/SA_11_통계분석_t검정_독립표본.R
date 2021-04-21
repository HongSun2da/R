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
# 통계분석 > 차이검정 > t-검정> 독립표본 t-검정 - 2개             [Independent Sample t-test]
#-------------------------------------------------------------------------------

# 1. -------------------------------
# 데이터 수집
data = read.csv("./data/ist.csv",
                header=TRUE,
                na.strings = ".")

# 데이터 이해
View(data)
str(data)   # 'data.frame':	60 obs. of  2 variables:

# H0 : A타이어회사화 B타이어회사의 타이어수명은 차이가 없다.
# H1 : A타이어회사화 B타이어회사의 타이어수명은 차이가 있다.

# 기술통계
library(psych)
psych::describeBy(data$t_time, data$t_group, mat = TRUE)

#     item group1 vars  n     mean       sd  median  trimmed      mad   min   max range        skew   kurtosis       se
# X11    1      1    1 30 48670.57 3607.118 49047.0 48864.88 3274.322 38214 55750 17536 -0.61983820  0.6818221 658.5667
# X12    2      2    1 30 51377.60 4197.600 51395.5 51459.54 3640.524 41852 59299 17447 -0.08715409 -0.5159939 766.3734


# 등분산 검정
var.test(data$t_time ~ data$t_group, data = data)

# F test to compare two variances
# 
# data:  data$t_time by data$t_group
# F = 0.73845, num df = 29, denom df = 29, p-value = 0.4192
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.3514742 1.5514716
# sample estimates:
#   ratio of variances 
# 0.7384458 


# t-test # paired=TRUE 대응 표본
test = t.test(data$t_time ~ data$t_group,
              data = data,
              alternative = c("two.sided"),
              paired=TRUE,
              conf.level = 0.95)
test

# Two Sample t-test
# 
# data:  data$t_time by data$t_group
# t = -2.679, df = 58, p-value = 0.009593
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -4729.6975  -684.3692
# sample estimates:
#   mean in group 1 mean in group 2 
# 48670.57        51377.60 

# 그래프 확인
library(ggplot2)

ggplot2::ggplot(data, 
                aes(x=t_time)) +
  ggplot2::geom_histogram(binwidth=5000) +
  ggplot2::ggtitle("Car Company T Life") +
  ggplot2::facet_grid(. ~ t_group)




