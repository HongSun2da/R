
## 인공 신경망 (ANN)   #########################################################
# install.packages("neuralnet")

library(neuralnet)
library(caret)




# 01. 데이터 불러 오기                  ----------------------------------------
data_df = read.csv("tinydata.csv",
                   header = TRUE,
                   na.strings = ".")

View(data_df)
str(data_df)
summary(data_df)



# 02. 데이터 전처리                     ----------------------------------------
data_df$Acceptance = factor(data_df$Acceptance)

data_df$Like = data_df$Acceptance == "like"
data_df$DisLike = data_df$Acceptance == "dislike"



# 03. 기술통계 확인                     ----------------------------------------
library(psych)

describe(data_df)

pairs.panels(data_df)




# 03. 데이터 분리 (훈련용, 검증용)   PASS     ----------------------------------
# 데이터가 너무 작아서 분리 안함




# 03. 모델 생성 (ANN 신경망)            ----------------------------------------
# neuralnet(formula = ,        : 출력 ~ 입력 + 입력  
#           data = ,           :
#           hidden = ,         : hidden layer와 노드수 (2, 3)
#           rep = ,            : 모형의 적합 횟수
#           algorithm = ,      : 역전파 알고리즘 backprop, rporp+, rporp-
#           err.fct = ,        : 오차총합 sse, ce
#           act.fct = ,        : 활성화 함수, logistic, tanh
#           linear.output = ,  : 출력노드에서 선형 활성화 비적용
#           likelihood = )     : AIC, BIC 산술

set.seed(1)
model = neuralnet(Like + DisLike ~ Salt + Fat,
                  data = data_df,
                  linear.output = F,
                  hidden = 3)
model



# 04. 모델 결과 확인 하기              -----------------------------------------
prediction(model)
plot(model, rep="best")




# 05. 모델 평가 하기 하기              -----------------------------------------
#  compute(net, covariate) : 데이터셋의 각 개체별로 출력값 산출

pred = compute(model,
                  data.frame(data_df$Salt, data_df$Fat))

head(pred)
predicted_class = apply(pred$net.result,
                        1,
                        which.max) - 1


confusionMatrix(as.factor(ifelse(predicted_class == "1", "dislike", "like")),
                as.factor(data_df$Acceptance))

#           Reference
# Prediction dislike like
# dislike       3    0
# like          0    3
# 
# Accuracy : 1          
# 95% CI : (0.5407, 1)
# No Information Rate : 0.5        
# P-Value [Acc > NIR] : 0.01563  











