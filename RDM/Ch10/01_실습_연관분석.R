## 연관 분석     ###############################################################
# install.packages("arules")

library(arules)



# 01. 데이터 불러 오기                    --------------------------------------
data_df = read.csv("Faceplate.csv",
                   header = TRUE,
                   na.strings = ".")


View(data_df)
str(data_df)
summary(data_df)




# 02. 기술 통계 확인 하기                ---------------------------------------
library(psych)

describe(data_mt)
pairs.panels(data_mt)




# 03. 데이터 전처리 하기                  --------------------------------------
# - 1. matrix 형으로 변환
data_mt = as.matrix(data_df[, -1])

data_mt
str(data_mt)
summary(data_mt)

# - 2. transactions 으로 변환
data_tr = as(data_mt, "transactions")

data_tr
str(data_tr)
summary(data_tr)

inspect(data_tr)

#       items                 
# [1]  {Red,White,Green}     
# [2]  {White,Orange}        
# [3]  {White,Blue}          
# [4]  {Red,White,Orange}    
# [5]  {Red,Blue}            
# [6]  {White,Blue}          
# [7]  {Red,Blue}            
# [8]  {Red,White,Blue,Green}
# [9]  {Red,White,Blue}      
# [10] {Yellow}  




# 04. 연관규칙 실행                    -----------------------------------------
# supp = 지지도, conf = 신뢰도

model = apriori(data_tr,
                parameter = list(supp = 0.2,
                                 conf = 0.5,
                                 target = "rules"))

model
summary(model)




# 05. 규칙 확인 : lift가 높은 순서로   -----------------------------------------
data_tb = inspect(model)

data_tb

inspect(sort(model, by="lift"))

data_tb[data_tb$support >= 0.04 & data_tb$confidence >= 0.7, ]










