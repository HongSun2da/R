# 조건부 확률(Conditional probability)
# -> 사건 발생으로 경우의 수(전체공간)가 변경 되는 경우
# -> 스포츠 머리를한 사람은 여자/남자 인가? 남자일 확률이 1/2 보다 높아 진다.



# 나이브베이즈

# 01. 파일 불러오기  -----------------------------------------------------------
#install.packages("e1071")

library(e1071)

df_data = read.csv("FlightDelays.csv",
                   header = TRUE,
                   na.strings = ".")

head(df_data)
str(df_data)
summary(df_data)


# 02. 데이터 전처리 하기  ------------------------------------------------------

df_data$DAY_WEEK = factor(df_data$DAY_WEEK,
                          labels = c("Mon", "Tue", "Wen", "Thu", "Fri", "Sat", "Sun"))

df_data$DEP_TIME = factor(df_data$DEP_TIME)


df_data$CRS_DEP_TIME = factor(round(df_data$CRS_DEP_TIME/100))

str(df_data)


# 03. 훈련, 검증용 데이이터 생성  ----------------------------------------------

selected.var = c(10, 1, 8, 4, 2, 13)  # 6개 변수만 사용
train.index = sample(c(1:dim(df_data)[1]),
                     dim(df_data)[1]*0.6)   # dim(df_data) (2201, 13)

x_train = df_data[train.index, selected.var]
x_test =  df_data[-train.index, selected.var]   

summary(x_train)
summary(x_test)


# 04. 나이브 베이즈 실행(naive bayes)  -----------------------------------------

model = naiveBayes(Flight.Status ~ ., data = x_train)   # 모델 생성 ★★★★★

model

# Naive Bayes Classifier for Discrete Predictors
# 
# Call:
#   naiveBayes.default(x = X, y = Y, laplace = laplace)
# 
# A-priori probabilities:
#   Y
#   delayed    ontime 
# 0.1969697 0.8030303 
# 
# Conditional probabilities:
#   DAY_WEEK
# Y              Mon        Tue        Wen        Thu        Fri        Sat        Sun
# delayed 0.20000000 0.11538462 0.14615385 0.15000000 0.17307692 0.06153846 0.15384615 -> ★ 전체 합은 1
# ontime  0.11792453 0.13396226 0.15188679 0.18301887 0.18490566 0.13396226 0.09433962  
# 
# CRS_DEP_TIME
# Y                6          7          8          9         10         11         12         13         14         15         16         17         18
# delayed 0.03846154 0.06538462 0.05000000 0.02692308 0.02307692 0.01538462 0.06538462 0.05000000 0.04230769 0.20384615 0.07307692 0.14615385 0.01153846
# ontime  0.06320755 0.06509434 0.07547170 0.04622642 0.05000000 0.04150943 0.06981132 0.06226415 0.05943396 0.11698113 0.08773585 0.09811321 0.04433962
# CRS_DEP_TIME
# Y               19         20         21
# delayed 0.09615385 0.02692308 0.06538462
# ontime  0.04716981 0.02169811 0.05094340
# 
# ORIGIN
# Y              BWI        DCA        IAD
# delayed 0.07692308 0.50769231 0.41538462
# ontime  0.05377358 0.65377358 0.29245283
# 
# DEST
# Y             EWR       JFK       LGA
# delayed 0.4076923 0.1615385 0.4307692
# ontime  0.2735849 0.1773585 0.5490566
# 
# CARRIER
# Y               CO         DH         DL         MQ         OH         RU         UA         US
# delayed 0.07307692 0.30769231 0.11923077 0.16923077 0.00000000 0.25000000 0.01153846 0.06923077
# ontime  0.03490566 0.23207547 0.18867925 0.12924528 0.01603774 0.17358491 0.01509434 0.21037736


# 05.개별 항목 비율로 전환 : prop.table  ---------------------------------------

tb_data = table(x_train$Flight.Status, x_train$DEST)

prop.table(tb_data, margin = 1)
prop.table(tb_data, margin = 2)
prop.table(tb_data)

tb_data_p = prop.table(tb_data, margin = 1)
tb_data_p

# DEST
# Y             EWR       JFK       LGA
# delayed 0.4076923 0.1615385 0.4307692
# ontime  0.2735849 0.1773585 0.5490566


# 06.검증데이터롤 확률 예측(predict probabilities)  ----------------------------
# - 1. newdata=예측을 위한 새로운 데이터
# - 2. type=raw(확률값예측), class(소속클래스)

pred_raw = predict(model,
               newdata = x_test,
               type="raw")

head(x_test, 20)
head(pred_raw, 20)

pred_cla = predict(model,
                   newdata = x_test,
                   type="class")

head(x_test, 20)
head(pred_cla, 20)


# 07.확률과 Class 합쳐 결과 보기  ----------------------------------------------

df = data.frame(actual = x_test$Flight.Status, 
                pred_raw, 
                predicted = pred_cla) # 정답, 확률, Class
str(df)
df



