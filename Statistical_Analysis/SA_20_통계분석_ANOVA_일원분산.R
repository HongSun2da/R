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
# 통계분석 > 차이검정 > 분산분석(ANOVA) > 일원 분산분석     - 3개           [One Way ANOVA]
#-------------------------------------------------------------------------------


# 1. -------------------------------
# 데이터 수집
adata = read.csv("./data/owa.csv",
                header=TRUE,
                na.strings = ".")

adata$group = factor(adata$group,
                      levels = c(1:4),
                      labels = c("Gangnam","Gangsu","Gangdong","Gangbug"))


# 데이터 이해
View(adata)
str(adata)   # 'data.frame':	20 obs. of  3 variables:

# H0 : 4곳 매장의 고객만족도는 차이가 없다.
# H1 : 4곳 매장의 고객만족도 중 적어도 한 쌍은 차이가 있다.
#    -> [사후검정] 어느 매장이 차이가 있는가??  



# 기술통계
library(psych)
psych::describeBy(adata$score, adata$group, mat = TRUE)

#     item group1 vars  n     mean        sd median  trimmed     mad min max range       skew   kurtosis       se
# X11    1      1    1 38 88.86842  8.279480   91.0 89.81250  7.4130  64  99    35 -1.1417237  0.8759359 1.343109
# X12    2      2    1 32 88.18750  7.511013   87.5 88.65385  7.4130  71  99    28 -0.3858498 -0.5813980 1.327772
# X13    3      3    1 30 82.00000 11.252586   83.0 82.62500 11.8608  56  99    43 -0.4031414 -0.6759641 2.054432
# X14    4      4    1 39 86.05128  9.875405   88.0 87.15152  8.8956  53  99    46 -1.2130392  1.7071652 1.581330


# 그래프 데이터 확인
#install.packages("ggplot2")
library(ggplot2)

ggplot2::ggplot(adata, 
                aes(x=score)) +
  ggplot2::geom_histogram(binwidth = 5) + 
  ggplot2::facet_grid(. ~ group)

ggplot2::ggplot(adata, 
                aes(x=group, y=score)) +
  ggplot2::geom_boxplot() +
  ggplot2::facet_grid(. ~ group)
  
  

# 분산 동질성 검정: Hartley 검정, Bartlett 검정, Levene 검정
bartlett.test(score ~ group, data = adata)

# Bartlett test of homogeneity of variances
# 
# data:  score by group
# Bartlett's K-squared = 6.0049, df = 3, p-value = 0.1114


# Bartlett test는 정규분포에 민감함
#install.packages("car")
library(car)
car::leveneTest(score ~ group, data = adata)

# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   3  1.5622 0.2015
#       135  


#-------------------------------------------------------------------------------
?aov # {stats} Fit an Analysis of Variance Model

aov(formula, data = NULL, projections = FALSE, qr = TRUE,
    contrasts = NULL, ...)
#-------------------------------------------------------------------------------


# ANOVA 분석
test = aov(score ~ group, 
           data = adata)
summary(test)

#              Df Sum Sq Mean Sq F value Pr(>F)  
# group         3    919  306.37   3.546 0.0163 *
#   Residuals   135  11663   86.39  



# 부록 : 이분산일때 Welch's ANOVA test
test = oneway.test(score ~ group, 
                data = adata,
                var.equal = FALSE)
test

# One-way analysis of means (not assuming equal variances)
# 
# data:  score and group
# F = 2.9478, num df = 3.000, denom df = 72.465, p-value = 0.03842


#-------------------------------------------------------------------------------
?pairwise.t.test # {stats} Pairwise t tests

# pairwise.t.test(x, g, p.adjust.method = p.adjust.methods,
#                 pool.sd = !paired, paired = FALSE,
#                 alternative = c("two.sided", "less", "greater"),
#                 ...)
# p.adjust.methods
# c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
#-------------------------------------------------------------------------------

# 사후 검정 (차이가 나는 그룹 비교 하기)
pairwise.t.test(adata$score,
                adata$group,
                data = adata,
                p.adjust.method = "bonferroni")

# Pairwise comparisons using t tests with pooled SD 
# 
# data:  adata$score and adata$group 
# 
#          Gangnam Gangsu Gangdong
# Gangsu   1.000   -      -       
# Gangdong 0.018   0.059  -       
# Gangbug  1.000   1.000  0.450   
# 
# P value adjustment method: bonferroni 


TukeyHSD(test)
 
# Fit: aov(formula = score ~ group, data = adata)
# 
# $group
#                        diff        lwr         upr     p adj
# Gangsu-Gangnam   -0.6809211  -6.482101  5.12025923 0.9900786
# Gangdong-Gangnam -6.8684211 -12.773642 -0.96319998 0.0155546
# Gangbug-Gangnam  -2.8171390  -8.328456  2.69417799 0.5457341
# Gangdong-Gangsu  -6.1875000 -12.332110 -0.04288951 0.0477285
# Gangbug-Gangsu   -2.1362179  -7.903298  3.63086253 0.7703242
# Gangbug-Gangdong  4.0512821  -1.820443  9.92300755 0.2803221






# group 으로 표현
#install.packages("agricolae")
library(agricolae)

LSD.test(test,
         "group",
         console = T,
         p.adj = "bonf")

#          score         groups
# Gangnam  88.86842      a
# Gangsu   88.18750     ab
# Gangbug  86.05128     ab
# Gangdong 82.00000      b

duncan.test(test,
            "group",
            group = T,
            console = T)   

#          score      groups
# Gangnam  88.86842      a
# Gangsu   88.18750      a
# Gangbug  86.05128     ab
# Gangdong 82.00000      b

duncan.test(test,
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

scheffe.test(test,
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



# 통계결과 그래프
tukeyplot = TukeyHSD(test)
plot(tukeyplot)






# 정규분포로 확인 하기  -------------------------------------------
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




