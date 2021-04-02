
### 로지스틱 분석  #############################################################

# 01. 데이터 불러 오기                      ------------------------------------
data_df = read.csv("UniversalBank.csv",
                   header = TRUE,
                   na.strings = ".")
data_df
str(data_df)
summary(data_df)



# 02. 데이터 전처리 하기                    ------------------------------------
# - 1. Education
data_df$Education = factor(data_df$Education,
                           levels = c(1,2,3),
                           labels = c("Undergrad","Graduate","Advanced/Profession"))

# - 2. ID, ZIP.Code 열 제거
data_df = data_df[, -c(1, 5)]



# 03. 기술통계 확인 하기                    ------------------------------------
library(psych)

describe(data_df)         # - 1. describe 확인
pairs.panels(data_df)     # - 2. 상관관계 확인



# 04. 데이터 분리 하기                      ------------------------------------
set.seed(2)
train_index = sample(c(1:dim(data_df)[1]),
                     dim(data_df)[1]*0.6)

train_df = data_df[train_index, ]
test_df = data_df[-train_index, ] 

describe(train_df)  # 3000 obs. of  12 variables
describe(test_df)   # 2000 obs. of  12 variables


# 05. 로지스틱 분석                         ------------------------------------
options(scipen = 0)   # 소수점 10자리 표시

model = glm(Personal.Loan ~ ., 
            data = train_df,
            family = "binomial")


summary(model)


# - 1. Odds 값 확인 하기
odds = data.frame(summary(model)$coefficients, 
                  odds = exp(coef(model)))

round(odds, 5)

# coef(model)
# exp(coef(model))



# 06. 예측 모델 화인 [test_df]              ------------------------------------
pred = predict(model,
               test_df[, -8],
               type = "response")

head(pred)

# - 1. 실제 = 예측 값 비교 하기
data.frame(actual = test_df$Personal.Loan[1:30],
           predicted = round(pred[1:30], 5))



# 07. 모델 평가                             ------------------------------------
library(caret)

confusionMatrix(as.factor(ifelse(pred > 0.5, 1, 0)),
                as.factor(test_df$Personal.Loan))

#             Reference
# Prediction    0    1
#         0  1794   65
#         1    18  123
# 
# Accuracy : 0.9585         ★★★★★          
# 95% CI : (0.9488, 0.9668)
# No Information Rate : 0.906           
# P-Value [Acc > NIR] : < 2.2e-16      




# 08. gain chart 만들기                     ------------------------------------
# install.packages("gains")
library(gains)

gain = gains(test_df$Personal.Loan,
             pred,
             groups = 10)

gain


# - 1. lift chart
plot(c(0, gain$cume.pct.of.total * sum(test_df$Personal.Loan)) ~ c(0, gain$cume.obs),
     xlab="cases",
     ylab="Cumulative",
     main="",
     type="l")


# c(0, gain$cume.pct.of.total * sum(test_df$Personal.Loan))

# c(0, gain$cume.obs)


# - 2. 10분위 차트
heights = gain$mean.resp/mean(test_df$Personal.Loan)

midpoints = barplot(heights,
                    names.arg = gain$depth,
                    ylim = c(0, 9),
                    xlab = "Percentile",
                    ylab = "Mean Response",
                    main = "Decile-wise lift chart")



# 09. 중요한 변수 선택                      ------------------------------------

data_val = step(model, direction = "backward")

summary(data_val)










