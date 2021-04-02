

# 01. 데이터 불러 오기                      ------------------------------------
data_df = read.csv("FlightDelays.csv",
                   header = TRUE,
                   na.strings = ".")
data_df
str(data_df)
summary(data_df)

# 02. 데이터 전처리 하기                    ------------------------------------
# - 1. DAY_WEEK
data_df$DAY_WEEK = factor(data_df$DAY_WEEK,
                           levels = c(1:7),
                           labels = c("mon","tue","wed","thu","fri","sat","sun"))

data_df$CRS_DEP_TIME = factor(round(data_df$CRS_DEP_TIME/100))

data_df$ORIGIN = factor(data_df$ORIGIN)         
data_df$DEST = factor(data_df$DEST)    
data_df$CARRIER = factor(data_df$CARRIER) 
data_df$TAIL_NUM = factor(data_df$TAIL_NUM)   
data_df$Flight.Status = factor(data_df$Flight.Status)    

data_df$ORIGIN = relevel(data_df$ORIGIN, ref = "IAD")         
data_df$DEST = relevel(data_df$DEST, ref = "LGA")     
data_df$CARRIER = relevel(data_df$CARRIER, ref = "US")  
data_df$DAY_WEEK = relevel(data_df$DAY_WEEK, ref = "wed")    

data_df$isDelay = 1 * (data_df$Flight.Status == "delayed")



# 03. 기술통계 확인 하기                    ------------------------------------
library(psych)

describe(data_df)         # - 1. describe 확인
pairs.panels(data_df)     # - 2. 상관관계 확인



# 04. 데이터 분리 하기                      ------------------------------------
set.seed(2)
selected.var = c(10, 1, 8, 4, 2, 9, 14)
train_index = sample(c(1:dim(data_df)[1]),
                     dim(data_df)[1]*0.6)

train_df = data_df[train_index, selected.var]
test_df = data_df[-train_index, selected.var] 

describe(train_df)  # 3000 obs. of  12 variables
describe(test_df)   # 2000 obs. of  12 variables



# 05. 로지스틱 분석                         ------------------------------------
model = glm(isDelay ~ ., 
            data = train_df,
            family = "binomial")


summary(model)

# - 1. Odds 값 확인 하기
odds = data.frame(summary(model)$coefficients, 
                  odds = exp(coef(model)))

round(odds, 8)

# 06. 예측 모델 화인 [test_df]              ------------------------------------
pred = predict(model,
               test_df[, -7],
               type = "response")

head(pred)

# - 1. 실제 = 예측 값 비교 하기
data.frame(actual = test_df$isDelay[1:30],
           predicted = round(pred[1:30], 5))



# 07. 모델 평가                             ------------------------------------
library(caret)

confusionMatrix(as.factor(ifelse(pred > 0.5, 1, 0)),
                as.factor(test_df$isDelay))


#           Reference
# Prediction   0   1
#          0 692 173
#          1   1  15
# 
# Accuracy : 0.8025          
# 95% CI : (0.7746, 0.8283)
# No Information Rate : 0.7866          
# P-Value [Acc > NIR] : 0.133  


# 08. gain chart 만들기                     ------------------------------------
# install.packages("gains")
library(gains)

gain = gains(test_df$isDelay,
             pred,
             groups = 10)

gain

# - 1. lift chart
plot(c(0, gain$cume.pct.of.total * sum(test_df$isDelay)) ~ c(0, gain$cume.obs),
     xlab="cases",
     ylab="Cumulative",
     main="",
     type="l")

# - 2. 10분위 차트
heights = gain$mean.resp/mean(test_df$isDelay)

midpoints = barplot(heights,
                    names.arg = gain$depth,
                    ylim = c(0, 9),
                    xlab = "Percentile",
                    ylab = "Mean Response",
                    main = "Decile-wise lift chart")

# 09. 중요한 변수 선택                      ------------------------------------

data_val = step(model, direction = "backward")

summary(data_val)



