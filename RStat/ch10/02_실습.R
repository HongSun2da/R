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
#####################################################################################################

#---------------------------------------------------------------------------------------------------
# 통계분석 > 차이검정 > 분산분석 > 이원 반복측정 분산- 반복 + 요인   [Two Way Repeated Measures ANOVA]

# 01. 데이터 불러 오기  --------------------------------------------------------

twrma.df = read.csv("Ch1002.TWRMA.csv",
                    header = TRUE,
                    na.strings = ".")
twrma.df
summary(twrma.df)

twrma.df$group = factor(twrma.df$group,
                        levels = c(1,2),
                        labels = c("ControlGroup","ExperimentalGroup"))

twrma.df$time = factor(twrma.df$time,
                        levels = c(1,2),
                        labels = c("Before","After"))
summary(twrma.df)

# 02. 기술 통계 확인  --------------------------------------------------------
library(psych)

describeBy(twrma.df$painscore, twrma.df$group:twrma.df$time, mat = T )

#     item                   group1 vars  n     mean        sd median  trimmed       mad   min    max  range       skew   kurtosis       se
# X11    1      ControlGroup:Before    1 71 45.79549 22.714636  43.37 42.98000 12.009060 12.32 146.44 134.12  1.8821859  5.2924556 2.695731
# X12    2       ControlGroup:After    1 71 45.05972 16.620947  43.56 43.86491 12.335232 12.57 104.46  91.89  0.8352065  1.4257983 1.972544
# X13    3 ExperimentalGroup:Before    1 71 45.28268 22.459870  44.48 43.22351 19.659276  5.08 131.24 126.16  1.2314591  2.5827325 2.665496
# X14    4  ExperimentalGroup:After    1 71 26.47352  6.932682  26.15 26.73368  7.694694  8.62  37.94  29.32 -0.3038318 -0.4139521 0.822758


# 03. 그래프 확인  --------------------------------------------------------
boxplot(painscore ~ group*time,
        data = twrma.df)

library(ggplot2)

ggplot(twrma.df, aes(x=group, y=painscore)) +
  geom_boxplot(outlier.color = "red") +
  facet_wrap(~time) +
  ggtitle("Control - Experimental Group Before/After") +
  theme_classic()
  


# 04. 통계 검정  -------------------------------------------------------------

# - 1. 다변량 분석을 이용한 구형성(sphericity) 검정
#      Mauchly Tests for Sphericity

library(car)
twrma.matrix = cbind(twrma.df$painscore[twrma.df$time=="Before"],
                   twrma.df$painscore[twrma.df$time=="After"])
twrma.matrix
summary(twrma.matrix)

twrma.model.lm = lm(twrma.matrix ~ 1) # ~1 : 데이터를 하나로 묶기
summary(twrma.model.lm)

time.f = factor(c("Before", "After"))
options(contrasts=c("contr.sum", "contr.poly"))

twrma.result.mt = Anova(twrma.model.lm,
                      idata=data.frame(time.f),
                      idesign = ~time.f,
                      type="III")

summary(twrma.result.mt, multivariate = F)


# ANOVA 검정
# - 방법 1

twrma.result1 = aov(painscore ~ time*group + Error(id), data = twrma.df)
summary(twrma.result1)

# - 방법 2

twrma.result2 = aov(painscore ~ time + group + time:group, data = twrma.df)
summary(twrma.result2)

# - 1. 상호작용 그래프 ★★★★★
interaction.plot(twrma.df$time, twrma.df$group, twrma.df$painscore)


# 05. 사후검정(Multicamparision) 확인    ---------------------------------------------------------
# - 1. 상호작용이 있는 경우 -> 그룹별로 나누어서 분석
# - 2. 상호작용이 없는 경우 -> 걱 변수별 주효과 분석(t-test, ANOVA분석)

# - 상호작용(유) 분석

twrma1 = twrma.df[twrma.df$time=="Before", ]
twrma2 = twrma.df[twrma.df$time=="After", ]

summary(twrma1)
describeBy(twrma1$painscore, twrma1$group, mat=TRUE)

summary(twrma2)
describeBy(twrma2$painscore, twrma2$group, mat=TRUE)

t.test(painscore ~ group,
       data = twrma1,
       alternative = c("two.sided"),
       var.equal = TRUE,
       conf.level = 0.95)

t.test(painscore ~ group,
       data = twrma2,
       alternative = c("two.sided"),
       var.equal = TRUE,
       conf.level = 0.95)





