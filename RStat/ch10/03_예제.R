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
# 통계분석 > 차이검정 > 분산분석 > 이원 분산분석     - 요인 + 요인   [Two Way ANOVA] - 상호작용 확인

# 01. 데이터 불러 오기  --------------------------------------------------------

edu.df = read.csv("Ch1003.교육역량분석.csv",
                    header = TRUE,
                    na.strings = ".")
edu.df
str(edu.df)
summary(edu.df)


edu.df$학부 = factor(edu.df$학부,
                        levels = c(1:4),
                        labels = c("1학부","2학부","3학부","4학부"))

edu.df$학년 = factor(edu.df$학년,
                   levels = c(1:4),
                   labels = c("1학년","2학년","3학년","4학년"))

# 02. 기술 통계 확인  --------------------------------------------------------
library(psych)

describeBy(edu.df$비교과운영, edu.df$학부:edu.df$학년, mat = T )


# 03. 그래프 확인  --------------------------------------------------------
boxplot(비교과운영 ~ 학부*학년,
        data = edu.df)

library(ggplot2)

ggplot(edu.df, aes(x=학년, y=비교과운영)) +
  geom_boxplot(outlier.color = "red") +
  facet_wrap(~학부) +
  ggtitle("Control - Experimental Group Before/After") +
  theme_classic()


# ANOVA 검정
# - 방법 1

edu.result1 = aov(비교과운영 ~ 학부 + 학년 + 학부:학년, data = edu.df)
summary(edu.result1)

# - 방법 2 - 상호작용효과가 의미가 없어 제거해주고 다시 계산

edu.result2 = aov(비교과운영 ~ 학부 + 학년, data = edu.df)
summary(edu.result2)



# - 1. 상호작용 그래프 ★★★★★
interaction.plot(edu.df$학부, edu.df$학년, edu.df$비교과운영)













