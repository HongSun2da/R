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
# 통계분석 > 차이검정 > 분산분석(ANOVA) > 이원 반복측정 분산- 반복 + 요인   [Two Way Repeated Measures ANOVA]
#-------------------------------------------------------------------------------


# 1. -------------------------------
# 데이터 수집
train = read.csv("./data/twrma.csv",
                 header=TRUE,
                 na.strings = ".")

train$group = factor(train$group,
                    levels = c(1:2),
                    labels = c("Control", "Scientific"))

train$time = factor(train$time,
                    levels = c(1:2),
                    labels = c("painscore1", "painscore2"))


# 데이터 이해
View(train)
str(train)   # 'data.frame':	60 obs. of  3 variables:


# 기술통계
library(psych)
psych::describeBy(train$painscore, train$group:train$time, mat = TRUE)

#     item                group1 vars  n     mean        sd median  trimmed       mad   min    max  range       skew   kurtosis       se
# X11    1    Control:painscore1    1 71 45.79549 22.714636  43.37 42.98000 12.009060 12.32 146.44 134.12  1.8821859  5.2924556 2.695731
# X12    2    Control:painscore2    1 71 45.05972 16.620947  43.56 43.86491 12.335232 12.57 104.46  91.89  0.8352065  1.4257983 1.972544
# X13    3 Scientific:painscore1    1 71 45.28268 22.459870  44.48 43.22351 19.659276  5.08 131.24 126.16  1.2314591  2.5827325 2.665496
# X14    4 Scientific:painscore2    1 71 26.47352  6.932682  26.15 26.73368  7.694694  8.62  37.94  29.32 -0.3038318 -0.4139521 0.822758


# 그래프 확인
boxplot(painscore ~ group * time,
        data = train)

library(ggplot2)
ggplot2::ggplot(train, aes(x=group, y=painscore)) +
  ggplot2::geom_boxplot(outlier.color = "red") +
  ggplot2::facet_wrap(~time) +
  ggplot2::ggtitle("Control - Experimental Group Before/After") +
  ggplot2::theme_classic()


# 다변량 분석을 이용한 구형성(sphericity) 검정 - 범주가 3개 이상일때만
# Mauchly Tests for Sphericity

library(car)
twrma.matrix = cbind(train$painscore[train$time=="painscore1"],
                     train$painscore[train$time=="painscore2"])
twrma.matrix
summary(twrma.matrix)

twrma.model.lm = lm(twrma.matrix ~ 1) # ~1 : 데이터를 하나로 묶기
summary(twrma.model.lm)

time.f = factor(c("painscore1", "painscore2"))
options(contrasts=c("contr.sum", "contr.poly"))

twrma.result.mt = Anova(twrma.model.lm,
                        idata=data.frame(time.f),
                        idesign = ~time.f,
                        type="III")

summary(twrma.result.mt, multivariate = F)

# Univariate Type III Repeated-Measures ANOVA Assuming Sphericity

#             Sum Sq num Df Error SS den Df  F value    Pr(>F)    
# (Intercept) 469354      1    62219    141 1063.641 < 2.2e-16 ***
# time.f        6781      1    44184    141   21.638 7.505e-06 ***
  


# ANOVA 검정
# - 방법 1

model = aov(painscore ~ time*group + Error(id), 
            data = train)
summary(model)

# Error: id
#       Df Sum Sq Mean Sq
# group  1   10.9    10.9
# 
# Error: Within
#             Df Sum Sq Mean Sq F value   Pr(>F)    
# time         1   6781    6781   24.73 1.15e-06 ***
# group        1  24094   24094   87.87  < 2e-16 ***
# time:group   1   5798    5798   21.15 6.46e-06 ***
# Residuals  279  76500     274   

model = aov(painscore ~ time + group + time:group, 
            data = train)
summary(model)

#              Df Sum Sq Mean Sq F value   Pr(>F)    
# time          1   6781    6781   20.17 1.04e-05 ***
# group         1   6475    6475   19.26 1.62e-05 ***
# time:group    1   5798    5798   17.25 4.36e-05 ***
# Residuals   280  94130     336   


# - 1. 상호작용 그래프 ★★★★★
interaction.plot(train$time, train$group, train$painscore)


# 사후검정(Multicamparision) 확인
# - 1. 상호작용이 있는 경우 -> 그룹별로 나누어서 분석
# - 2. 상호작용이 없는 경우 -> 걱 변수별 주효과 분석(t-test, ANOVA분석)

twrma1 = train[train$time=="painscore1", ]
twrma2 = train[train$time=="painscore2", ]

summary(twrma1)
describeBy(twrma1$painscore, twrma1$group, mat=TRUE)

summary(twrma2)
describeBy(twrma2$painscore, twrma2$group, mat=TRUE)

t.test(painscore ~ group,
       data = twrma1,
       alternative = c("two.sided"),
       var.equal = TRUE,
       conf.level = 0.95)

# Two Sample t-test
# 
# data:  painscore by group
# t = 0.13527, df = 140, p-value = 0.8926
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -6.982234  8.007868
# sample estimates:
#   mean in group Control mean in group Scientific 
# 45.79549                 45.28268 

t.test(painscore ~ group,
       data = twrma2,
       alternative = c("two.sided"),
       var.equal = TRUE,
       conf.level = 0.95)

# Two Sample t-test
# 
# data:  painscore by group
# t = 8.6963, df = 140, p-value = 8.319e-15
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   14.36073 22.81167
# sample estimates:
#   mean in group Control mean in group Scientific 
# 45.05972                 26.47352 




