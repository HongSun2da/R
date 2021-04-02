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
#                 교차분석 - (명목) - 동질성(사전), 독립성(사후) 검정
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
# 통계분석 > 관계검정 > 교차분석 (동질성)

# 01. 데이터 불러 오기  --------------------------------------------------------

df_data = read.csv("Ch1201.사전설계교차분석(PreCH).csv",
                   header = TRUE,
                   na.strings = ".")
df_data
str(df_data)
summary(df_data)

df_data$group = factor(df_data$group,
                       level=c(1,2),
                       label=c("Vit", "NoVit"))

df_data$cold = factor(df_data$cold,
                       level=c(1,2),
                       label=c("NoCold", "Cold"))


# 02. 기술통계 확인  --------------------------------------------------------
library(psych)

describe(df_data)



# 03. 카이스케어 분석  --------------------------------------------------------
# install.packages("gmodels")
library(gmodels)

df_data_tb = table(df_data$group, df_data$cold)
str(df_data_tb)

df_data_res = CrossTable(df_data_tb,
                         expected = TRUE,
                         chisq = TRUE,
                         asresid = F)














