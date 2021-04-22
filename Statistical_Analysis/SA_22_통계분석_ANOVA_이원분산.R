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
# 통계분석 > 차이검정 > 분산분석(ANOVA) > 이원 분산분석     - 요인 + 요인   [Two Way ANOVA] - 상호작용 확인
#-------------------------------------------------------------------------------


# 1. -------------------------------
# 데이터 수집
train = read.csv("./data/twa.csv",
                 header=TRUE,
                 na.strings = ".")

train$meth = factor(train$meth,
                    levels = c(1:2),
                    labels = c("Oven", "Oil"))

train$temp = factor(train$temp,
                    levels = c(1:2),
                    labels = c("200c", "300c"))



# 데이터 이해
View(train)
str(train)   # 'data.frame':	60 obs. of  3 variables:


# 기술통계
library(psych)
psych::describeBy(train$taste, train$meth:train$temp, mat = TRUE)

#     item    group1 vars  n     mean       sd median  trimmed    mad min max range        skew   kurtosis        se
# X11    1 Oven:200c    1 15 87.40000 1.549193     88 87.46154 1.4826  85  89     4 -0.41742154 -1.4188889 0.4000000
# X12    2 Oven:300c    1 15 94.06667 2.658320     94 94.30769 1.4826  87  98    11 -1.02487442  0.9320565 0.6863753
# X13    3  Oil:200c    1 15 94.00000 2.299068     94 94.00000 2.9652  90  98     8 -0.09874741 -1.0767470 0.5936168
# X14    4  Oil:300c    1 15 86.00000 1.195229     86 86.00000 1.4826  84  88     4 -0.23426481 -1.1706667 0.3086067


# 그래프 확인    -----------------------------------------------------------
plot(taste ~ meth + temp, data=train)

boxplot(taste ~ meth * temp, data=train)


library(ggplot2)
ggplot(train, aes(x=temp, y=taste)) + 
  geom_boxplot(outlier.color = "red") +
  facet_wrap(~meth) + 
  ggtitle("Meathod Dif taste")



# ANOVA 검정
model = aov(taste ~ meth * temp, 
            data=train)
summary(model)

#             Df Sum Sq Mean Sq F value Pr(>F)    
# meth         1    8.1     8.1   1.994  0.163    
# temp         1    6.7     6.7   1.648  0.205    
# meth:temp    1  806.7   806.7 199.411 <2e-16 ***
# Residuals   56  226.5     4.0    

model = aov(taste ~ meth + temp + meth:temp, 
            data=train)
summary(model)

#             Df Sum Sq Mean Sq F value Pr(>F)    
# meth         1    8.1     8.1   1.994  0.163    
# temp         1    6.7     6.7   1.648  0.205    
# meth:temp    1  806.7   806.7 199.411 <2e-16 ***
# Residuals   56  226.5     4.0  


# 상호작용 그래프 ★★★★★
interaction.plot(train$meth, train$temp, train$taste)


# 사후검정(Multicamparision) 확인
# - 1. 상호작용이 있는 경우 -> 그룹별로 나누어서 분석
# - 2. 상호작용이 없는 경우 -> 걱 변수별 주효과 분석(t-test, ANOVA분석)

# - 상호작용(유) 분석
tw1 = train[train$meth=="Oven", ]
tw2 = train[train$meth=="Oil", ]

summary(tw1)
psych::describeBy(tw1$taste, tw1$temp, mat=TRUE)

summary(tw2)
psych::describeBy(tw2$taste, tw2$temp, mat=TRUE)

t.test(taste ~ temp,
       data = tw1,
       alternative = c("two.sided"),
       var.equal = TRUE,
       conf.level = 0.95)

# Two Sample t-test
# 
# data:  taste by temp
# t = -8.3918, df = 28, p-value = 3.967e-09
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -8.293973 -5.039361
# sample estimates:
#   mean in group 200c mean in group 300c 
# 87.40000           94.06667 

t.test(taste ~ temp,
       data = tw2,
       alternative = c("two.sided"),
       var.equal = TRUE,
       conf.level = 0.95)

# Two Sample t-test
# 
# data:  taste by temp
# t = 11.957, df = 28, p-value = 1.62e-12
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   6.629527 9.370473
# sample estimates:
#   mean in group 200c mean in group 300c 
# 94                 86 


# - 부록 : 일반 분석 TukeyHSD 확인

TukeyHSD(model)

# $`meth:temp`
#                     diff        lwr        upr     p adj
# Oil:200-Oven:200   6.60000000   4.655352  8.5446476 0.0000000
# Oven:300-Oven:200  6.66666667   4.722019  8.6113143 0.0000000
# Oil:300-Oven:200  -1.40000000  -3.344648  0.5446476 0.2371322
# Oven:300-Oil:200   0.06666667  -1.877981  2.0113143 0.9997286
# Oil:300-Oil:200   -8.00000000  -9.944648 -6.0553524 0.0000000
# Oil:300-Oven:300  -8.06666667 -10.011314 -6.1220190 0.0000000

plot(TukeyHSD(model))






