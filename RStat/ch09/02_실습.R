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
# 통계분석 > 차이검정 > 분산분석 > 반복측정 분산분석 - 반복(3)       [Repeated Measures ANOVA]

# 01. 데이터 불러 오기  -------------------------------------------------------------

rma.df = read.csv("Ch0902.RMA.csv",
                  header=TRUE,
                  na.string=".")

rma.df
str(rma.df)

rma.df$time = factor(rma.df$time,
                     levels = c(1:3),
                     labels = c("Init", "3Month", "6Month"))
summary(rma.df)

# 02. 기술통계  -------------------------------------------------------------
library(psych)

describeBy(rma.df$score, rma.df$time, mat=T)

#     item group1 vars  n     mean       sd median  trimmed    mad min max range       skew   kurtosis        se
# X11    1   Init    1 45 60.20000 3.216435     60 60.16216 2.9652  53  67    14  0.1839993 -0.7112490 0.4794778
# X12    2 3Month    1 45 61.60000 3.595452     61 61.48649 4.4478  55  70    15  0.2387869 -0.9207981 0.5359783
# X13    3 6Month    1 45 68.91111 1.144596     69 68.94595 1.4826  66  71     5 -0.1866731 -0.4780593 0.1706264


# 03. 그래프 확인  -------------------------------------------------------------

boxplot(rma.df$score ~ rma.df$time,
        data = rma.df,
        ylab="score")


# 04. 통계 검정  -------------------------------------------------------------

# - 1. 다변량 분석을 이용한 구형성(sphericity) 검정
#      Mauchly Tests for Sphericity

library(car)
rma.matrix = cbind(rma.df$score[rma.df$time=="Init"],
                   rma.df$score[rma.df$time=="3Month"],
                   rma.df$score[rma.df$time=="6Month"])
rma.matrix
summary(rma.matrix)

rma.model.lm = lm(rma.matrix ~ 1) # ~1 : 데이터를 하나로 묶기
summary(rma.model.lm)

time.f = factor(c("Init", "3Month", "6Month"))
options(contrasts=c("contr.sum", "contr.poly"))

rma.result.mt = Anova(rma.model.lm,
                      idata=data.frame(time.f),
                      idesign = ~time.f,
                      type="III")

summary(rma.result.mt, multivariate = F)

# 구형성(sphericity) 여부 확인
#       Mauchly Tests for Sphericity
# 
#         Test statistic p-value
# time.f        0.92409  0.18315


# - 2. 일변량 ANOVA 검정

rma.result = aov(score ~ time + Error(id/time),
                 data = rma.df)
summary(rma.result)


# - 3. 다중비교 (Multicamparison test) - t-value 포함

# install.packages("multcomp")
library(multcomp)

result.lm = lm(score ~ time, data=rma.df)

summary(result.lm)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  63.5704     0.2464 258.028  < 2e-16 ***
# time1        -3.3704     0.3484  -9.673  < 2e-16 ***
# time2        -1.9704     0.3484  -5.655 9.22e-08 ***
  
tukey.result = glht(result.lm, linfct = mcp(time="Tukey"))

summary(tukey.result)
#                       Estimate Std. Error t value Pr(>|t|)    
# 3Month - Init == 0     1.4000     0.6035    2.32   0.0565 .  
# 6Month - Init == 0     8.7111     0.6035   14.44   <1e-04 ***
# 6Month - 3Month == 0   7.3111     0.6035   12.12   <1e-04 ***
  
plot(tukey.result)





