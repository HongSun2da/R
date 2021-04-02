
## 인공 신경망 (ANN)   #########################################################
# install.packages("neuralnet")
# install.packages("nnet")
# install.packages("caret")
# install.packages("e1071")

library(neuralnet)
library(nnet)
library(caret)
library(e1071)



# 01. 데이터 불러 오기                  ----------------------------------------
data_df = read.csv("accidentsnn.csv",
                   header = TRUE,
                   na.strings = ".")

View(data_df)
str(data_df)
summary(data_df)




# 02. 데이터 전처리                     ----------------------------------------
vars = c("ALCHL_I", "PROFIL_I_R", "VEH_INVL")

accidents.ctg = cbind(data_df[, vars],
                      class.ind(data_df[,]$SUR_COND),
                      class.ind(data_df[,]$MAX_SEV_IR))

# data_df[, c(vars)]
# class.ind(data_df[,]$SUR_COND)
# class.ind(data_df[,]$MAX_SEV_IR)

names(accidents.ctg) = c(vars, 
                         paste("SUR_COND_",c(1, 2, 3, 4, 9), sep = "" ),
                         paste("MAX_SEV_IR_",c(0,1,2), sep = "" ))

accidents.ctg
str(accidents.ctg)
summary(accidents.ctg)


# 03. 기술통계 확인                     ----------------------------------------
library(psych)

describe(accidents.ctg)

pairs.panels(accidents.ctg)


# 03. 데이터 분리 (훈련용, 검증용)   PASS     ----------------------------------
set.seed(2)

train_index = sample(c(1:dim(accidents.ctg)[1]),
                     dim(accidents.ctg)[1]*0.6)

train_df = accidents.ctg[train_index, ]
test_df = accidents.ctg[-train_index, ] 

describe(train_df)  # 3000 obs. of  12 variables
describe(test_df)   # 2000 obs. of  12 variables


# 03. 모델 생성 (ANN 신경망)            ----------------------------------------

model = neuralnet(MAX_SEV_IR_0 + MAX_SEV_IR_1 + MAX_SEV_IR_2 ~ ALCHL_I + PROFIL_I_R + VEH_INVL + SUR_COND_1 + SUR_COND_2 + SUR_COND_3 + SUR_COND_4,
                  data = train_df,
                  hidden = 2)
model


# 04. 모델 결과 확인 하기              -----------------------------------------
prediction(model)
plot(model, rep="best")


# 05. 모델 평가 하기 하기              -----------------------------------------
#  compute(net, covariate) : 데이터셋의 각 개체별로 출력값 산출

pred = compute(model,
               data.frame(test_df[, -c(8:11)]))

head(pred)
predicted_class = apply(pred$net.result,
                        1,
                        which.max) - 1


confusionMatrix(as.factor(predicted_class),
                as.factor(data_df[-train_index, ]$MAX_SEV_IR))


#             Reference 
# Prediction   0   1   2
#         0  216   0  20
#         1    0 123  23
#         2    0   5  13
# 
# Overall Statistics
# 
# Accuracy : 0.88            
# 95% CI : (0.8441, 0.9102)
# No Information Rate : 0.54            
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.7851     
# 









