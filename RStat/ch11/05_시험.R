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

#####################
## 1. 문제
#####################

# 01. 데이터 불러오기  ---------------------------------------------------------
df_data = read.csv("Ch1105.호흡과 뇌파.csv",
                   header = TRUE,
                   na.strings = ".")
df_data
summary(df_data)
# 02. 기술통계 확인  -----------------------------------------------------------
library(psych)

describe(df_data)
pairs.panels(df_data)

# 03. 상관계수 확인  -----------------------------------------------------------
cor(df_data, 
    use="complete.obs",
    method=c("pearson"))

cor.test(df_data$ch2al371,
         df_data$ch1al371,
         method = c("pearson"))

#####################
## 2. 문제
#####################

# 01. 데이터 불러오기  ---------------------------------------------------------
df_data = read.csv("Ch1106.교육수요자 만족도.csv",
                   header = TRUE,
                   na.strings = ".")
df_data = df_data[, c(1:8)]
summary(df_data)

# 02. 기술통계 확인  -----------------------------------------------------------
library(psych)

describe(df_data)
pairs.panels(df_data)


# 03. 단순회귀분석 확인  -----------------------------------------------------------

model = lm(만족도43.3 ~ ., data = df_data)

summary(model)

model = lm(만족도43.3 ~ 홍보+이미지, data = df_data)

summary(model)


# 04. 표준화 회귀계수 확인  -----------------------------------------------------------
library(lm.beta)

model_beta = lm.beta(model)

summary(model_beta) # ★★★ Standardized Std 표준화 값


#####################
## 2. 문제
#####################

# 01. 데이터 불러오기  ---------------------------------------------------------
df_data = read.csv("Ch1107.유니버셜은행.csv",
                   header = TRUE,
                   na.strings = ".")
df_data
summary(df_data)

df_data$Loan = factor(df_data$Loan,
                      level=c(0,1),
                      label=c("N", "Y"))

# 02. 기술통계 확인  -----------------------------------------------------------
library(psych)

describe(df_data)

pairs.panels(df_data)


# 03. 로지스틱 회귀분석 확인  --------------------------------------------------

model = glm(Loan ~ ., data = df_data,
            family = binomial)

summary(model)

odds = data.frame(summary(model)$coefficients,
                  odds = exp(coef(model)))
round(odds, 8)
