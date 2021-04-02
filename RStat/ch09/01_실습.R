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
#                               이원 분선분석     - 요인 + 요인   [Two Way ANOVA]
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
# 통계분석 > 차이검정 > 분산분석 > 일원 분산분석     - 3개           [One Way ANOVA]

# 01. 데이터 불러오기  -------------------------------------------
owa.df = read.csv("Ch0901.OWA.csv",
                  header = TRUE,
                  na.strings = ".")
owa.df
str(owa.df)

owa.df$group = factor(owa.df$group,
                      levels = c(1:4),
                      labels = c("Gangnam","Gangsu","Gangdong","Gangbug"))
summary(owa.df)


# 02. 기술통계 확인  -------------------------------------------
library(psych)

describeBy(owa.df$score,
           owa.df$group,
           mat=T)

#    item group1 vars  n     mean        sd median  trimmed     mad min max range       skew   kurtosis       se
# X11    1   강남    1 38 88.86842  8.279480   91.0 89.81250  7.4130  64  99    35 -1.1417237  0.8759359 1.343109
# X12    2   강서    1 32 88.18750  7.511013   87.5 88.65385  7.4130  71  99    28 -0.3858498 -0.5813980 1.327772
# X13    3   강동    1 30 82.00000 11.252586   83.0 82.62500 11.8608  56  99    43 -0.4031414 -0.6759641 2.054432
# X14    4   강복    1 39 86.05128  9.875405   88.0 87.15152  8.8956  53  99    46 -1.2130392  1.7071652 1.581330

# 03. 그래프 확인  -------------------------------------------
#install.packages("ggplot2")
library(ggplot2)

ggplot(owa.df, aes(x=group, y=score)) +
  geom_boxplot(outlier.color = "red") +
  ggtitle("Satisfaction") +
  theme_classic() +
  theme(title = element_text(color="darkblue", size=20))
  
  
ggplot(owa.df, aes(x=score)) +
  geom_histogram(binwidth = 10) +
  facet_grid(. ~ group) +
  ggtitle("Satisfaction") +
  theme_classic()
  

# 04. 통계분석  -------------------------------------------
# - 1. [[등분산 검증 하기]]
bartlett.test(score ~ group, data = owa.df)

# Bartlett's K-squared = 6.0049, df = 3, p-value = 0.1114
# - barlett test는 정규분포에 민감하기 때문에 leveneTest를 많이 사용

# install.packages("car")
library(car)

leveneTest(score ~ group, data = owa.df)

#         Df F value Pr(>F)
# group   3  1.5622 0.2015
#         135 
  
# - 2. [[등분산 일때]]  ANOVA 분석
owa.result = aov(score ~ group, data=owa.df)
owa.result

# Call:
#   aov(formula = score ~ group, data = owa.df)
# 
# Terms:
#   group Residuals
# Sum of Squares    919.116 11663.115
# Deg. of Freedom         3       135
# 
# Residual standard error: 9.294807
# Estimated effects may be unbalanced

summary(owa.result)

#               Df Sum Sq Mean Sq F value Pr(>F)  
# group         3    919  306.37   3.546 0.0163 *
# Residuals   135  11663   86.39   

# - 2. [[이분산 일때]]  welch's ANOVA test
oneway.test(owa.df$score ~ owa.df$group,
            data=owa.df,
            var.equal = FALSE)

#         One-way analysis of means (not assuming equal variances)
# 
# data:  owa.df$score and owa.df$group
# F = 2.9478, num df = 3.000, denom df = 72.465, p-value = 0.03842


# 05. 사후검정(Multicamparison test)  -------------------------------------------
# - 1. [Fisher LSD]
pairwise.t.test(owa.df$score,
                owa.df$group,
                data=owa.df,
                p.adj="non")

#           Gangnam Gangsu Gangdong
# Gangsu    0.7606  -      -       
# Gangdong  0.0030  0.0098 -       
# Gangbug   0.1859  0.3370 0.0749 

# - 2. [Bonferroni]
pairwise.t.test(owa.df$score,
                owa.df$group,
                data=owa.df,
                p.adj="bonf")

#         Gangnam Gangsu Gangdong
# Gangsu   1.000   -      -       
# Gangdong 0.018   0.059  -       
# Gangbug  1.000   1.000  0.450   
# P value adjustment method: bonferroni 


# - 2. [Tukey HSD, Duncan LSR]
TukeyHSD(owa.result)

#                   diff        lwr         upr     p adj
# Gangsu-Gangnam   -0.6809211  -6.482101  5.12025923 0.9900786
# Gangdong-Gangnam -6.8684211 -12.773642 -0.96319998 0.0155546
# Gangbug-Gangnam  -2.8171390  -8.328456  2.69417799 0.5457341
# Gangdong-Gangsu  -6.1875000 -12.332110 -0.04288951 0.0477285
# Gangbug-Gangsu   -2.1362179  -7.903298  3.63086253 0.7703242
# Gangbug-Gangdong  4.0512821  -1.820443  9.92300755 0.2803221



# 06. group 으로 표현  -------------------------------------------
# install.packages("agricolae")
library(agricolae)

LSD.test(owa.result,
         "group",
         console = T,
         p.adj = "bonf")

#          score         groups
# Gangnam  88.86842      a
# Gangsu   88.18750     ab
# Gangbug  86.05128     ab
# Gangdong 82.00000      b

duncan.test(owa.result,
             "group",
             group = T,
             console = T)   

#          score      groups
# Gangnam  88.86842      a
# Gangsu   88.18750      a
# Gangbug  86.05128     ab
# Gangdong 82.00000      b

duncan.test(owa.result,
            "group",
            group = F,
            console = T)

#                   difference pvalue signif.         LCL       UCL
# Gangbug - Gangdong  4.0512821 0.0732       .  -0.3860496  8.488614
# Gangbug - Gangnam  -2.8171390 0.2403          -7.4874493  1.853171
# Gangbug - Gangsu   -2.1362179 0.3427          -6.5735496  2.301114
# Gangdong - Gangnam -6.8684211 0.0047      ** -11.6937956 -2.043046
# Gangdong - Gangsu  -6.1875000 0.0091      ** -10.8578103 -1.517190
# Gangnam - Gangsu    0.6809211 0.7620          -3.7564106  5.118253

scheffe.test(owa.result,
            "group",
            group = F,
            console = T)

#                     Difference pvalue sig        LCL        UCL
# Gangbug - Gangdong  4.0512821 0.3625      -2.339067 10.4416307
# Gangbug - Gangnam  -2.8171390 0.6230      -8.815246  3.1809679
# Gangbug - Gangsu   -2.1362179 0.8185      -8.412679  4.1402429
# Gangdong - Gangnam -6.8684211 0.0308   * -13.295224 -0.4416183
# Gangdong - Gangsu  -6.1875000 0.0815   . -12.874836  0.4998364
# Gangnam - Gangsu    0.6809211 0.9926      -5.632651  6.9944936


# 07.통계결과 그래프  -------------------------------------------
tukeyplot = TukeyHSD(owa.result)
plot(tukeyplot)



# 08.정규분포로 확인 하기  -------------------------------------------
#    item group1 vars  n     mean        sd median  trimmed     mad min max range       skew   kurtosis       se
# X11    1   강남    1 38 88.86842  8.279480   91.0 89.81250  7.4130  64  99    35 -1.1417237  0.8759359 1.343109
# X12    2   강서    1 32 88.18750  7.511013   87.5 88.65385  7.4130  71  99    28 -0.3858498 -0.5813980 1.327772
# X13    3   강동    1 30 82.00000 11.252586   83.0 82.62500 11.8608  56  99    43 -0.4031414 -0.6759641 2.054432
# X14    4   강복    1 39 86.05128  9.875405   88.0 87.15152  8.8956  53  99    46 -1.2130392  1.7071652 1.581330

# 강남
x = 88.86842
se = 1.343109

x_data = rnorm(1000, x, se)
x_data = sort(x_data)

y_data = dnorm(x_data, x, se)

plot(x_data,
     y_data,
     col="blue",
     type="l",
     xlim=c(75, 95),
     ylim=c(0,0.3),
     main="Satisfaction")
abline(v=x, col="blue", lty=3)

par(new=T)

# 강서
x = 88.18750
se = 1.327772

x_data = rnorm(1000, x, se)
x_data = sort(x_data)

y_data = dnorm(x_data, x, se)

plot(x_data,
     y_data,
     col="red",
     type="l",
     xlim=c(75, 95),
     ylim=c(0,0.3),
     main="Satisfaction")
abline(v=x, col="red", lty=3)

par(new=T)

# 강동
x = 82.00000
se = 2.054432

x_data = rnorm(1000, x, se)
x_data = sort(x_data)

y_data = dnorm(x_data, x, se)

plot(x_data,
     y_data,
     col="green",
     type="l",
     xlim=c(75, 95),
     ylim=c(0,0.3),
     main="Satisfaction")
abline(v=x, col="green", lty=3)

par(new=T)

# 강복
x = 86.05128
se = 1.581330

x_data = rnorm(1000, x, se)
x_data = sort(x_data)

y_data = dnorm(x_data, x, se)

plot(x_data,
     y_data,
     col="black",
     type="l",
     xlim=c(75, 95),
     ylim=c(0,0.3),
     main="Satisfaction")
abline(v=x, col="black", lty=3)

