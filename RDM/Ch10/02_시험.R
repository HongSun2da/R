## 연관 분석     ###############################################################
# install.packages("arules")

library(arules)



# 01. 데이터 불러 오기                    --------------------------------------
data_df = read.csv("CharlesBookClub.csv",
                   header = TRUE,
                   na.strings = ".")


View(data_df)
str(data_df)
summary(data_df)




# 02. 기술 통계 확인 하기                ---------------------------------------
library(psych)

describe(data_df)
pairs.panels(data_df)






# 03. 데이터 전처리 하기                  --------------------------------------
# - 1. 필요 데이터 확인
data_book = data_df[, 8: 18]

data_book = ifelse(data_book > 0 , 1, 0)

# - 2. matrix 형으로 변환
data_book_mt = as.matrix(data_book[, -1])

data_book_mt


# - 3. transactions 으로 변환
data_book_tr = as(data_book_mt, "transactions")

data_book_tr


inspect(data_book_tr)



# 04. 연관규칙 실행                    -----------------------------------------
# - 1. 그래프 확인
itemFrequencyPlot(data_book_tr)


# supp = 지지도, conf = 신뢰도
model = apriori(data_book_tr,
                parameter = list(supp = 200/4000,
                                 conf = 0.5,
                                 target = "rules"))

model
summary(model)




# 05. 규칙 확인 : lift가 높은 순서로   -----------------------------------------
data_tb = inspect(model)

data_tb

inspect(sort(model, by="lift"))

data_tb[data_tb$support >= 0.04 & data_tb$confidence >= 0.7, ]












