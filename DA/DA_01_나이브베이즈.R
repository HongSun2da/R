## 나이브베이즈 (Naive Bayes)  #################################################
# - 사건이 발생할 확률을 추정
# - 사전에 알고 있는 정보(예측, 독립변수)를 바탕으로 어떤 사건(결과, 종속변수)이 발생할 확률
# - (예측, 독립변수) 범주형, (결과, 종속변수) 범주



################################################################################


# 데이터 수집    ---------------------------------------------------------------
library(mlbench)

data("HouseVotes84")

#write.csv(data,"HouseVotes84.csv")

str(HouseVotes84) # 'data.frame':	435 obs. of  17 variables:
class(HouseVotes84) # data.frame

View(HouseVotes84)

# 기술 통계      ---------------------------------------------------------------
library(psych)

psych::describe(HouseVotes84)
psych::pairs.panels(HouseVotes84)


# 데이터 전처리  ---------------------------------------------------------------
library(ggplot2)

data = na.omit(HouseVotes84[, c(1,2)])
str(data) # 'data.frame':	423 obs. of  2 variables:

data$V1 = factor(data$V1,
                 levels=c("n", "y"),
                 labels=c("No", "Yes"))
head(data)

ggplot2::ggplot(data, aes(x=V1, fill=Class)) +
        geom_bar(position = "dodge", width = 0.7) +
        labs(title = "Pros and Cons for Vote 1", x="Vote 1", y="Number of Congressmen", fill="Party")

# 결측값 확인

sum(is.na(HouseVotes84)) # 392

# 컬럼별 Class 구분으로 결측값 개수 확인
naCount = function(col, cls) {
  return(sum(is.na(HouseVotes84[, col]) & HouseVotes84$Class == cls))
}

naCount(2, "democrat")
naCount(2, "republican")

yesProd = function(col, cls) {
  sum_y = sum(HouseVotes84[, col] == "y" & HouseVotes84$Class == cls, na.rm=TRUE)
  sum_n = sum(HouseVotes84[, col] == "n" & HouseVotes84$Class == cls, na.rm=TRUE)
  return(sum_y / (sum_y + sum_n))
}

yesProd(2, "democrat")
yesProd(2, "republican")

set.seed(123)
for(i in 2:ncol(HouseVotes84)){
  if(sum(is.na(HouseVotes84[, i])) > 0) {

    d_na = which(is.na(HouseVotes84[, i]) & HouseVotes84$Class == "democrat")
    r_na = which(is.na(HouseVotes84[, i]) & HouseVotes84$Class == "republican")
    
    HouseVotes84[d_na, i] = ifelse(runif(naCount(i, "democrat")) < yesProd(i, "democrat"), "y", "n")
    HouseVotes84[r_na, i] = ifelse(runif(naCount(i, "republican")) < yesProd(i, "republican"), "y", "n")
  }
}

sum(is.na(HouseVotes84))

head(HouseVotes84)

#write.csv(HouseVotes84,"HouseVotes84.csv")



# 데이터 분리
data_idx = sample(nrow(HouseVotes84), 0.7*nrow(HouseVotes84))

data_train = HouseVotes84[data_idx, ]
data_test = HouseVotes84[-data_idx, ]

table(data_train$Class)
table(data_test$Class)

# 데이터 분석    ---------------------------------------------------------------

#install.packages("e1071")
library(e1071)

#-------------------------------------------------------------------------------
?naiveBayes # {e1071} Naive Bayes Classifier

# ## S3 method for class 'formula'
# naiveBayes(formula, data, laplace = 0, ..., subset, na.action = na.pass)
# ## Default S3 method:
# naiveBayes(x, y, laplace = 0, ...)
# 
# 
# ## S3 method for class 'naiveBayes'
# predict(object, newdata,
#         type = c("class", "raw"), threshold = 0.001, eps = 0, ...)
#-------------------------------------------------------------------------------

# 모델 생성
model = naiveBayes(Class ~ ., data = data_train)

model

# 모델 예측
model_pred = predict(model, 
                     newdata=data_test[, -1])
model_pred

# 분류표
table(data_test$Class, model_pred,
      dnn=c("Actual", "redicted"))

# 정확동 0.8778626
mean(model_pred == data_test$Class)





# 모델 예측(확률)
model_pred_raw = predict(model, 
                     newdata=data_test[, -1],
                     type="raw")

model_pred_raw

model_pred_raw_1 = factor(model_pred_raw[,"republican"] > 0.5,
                          levels = c(FALSE, TRUE),
                          labels = c("democrat", "republican"))
model_pred_raw_1


# 분류표
table(data_test$Class, model_pred_raw_1,
      dnn=c("Actual", "redicted"))

# 정확동 0.8778626
mean(model_pred_raw_1 == data_test$Class)


### 교차 검증 하기 #####################

nbRuns = function(fraction, run) {
  results = NULL
  for(i in 1:run) {
    
    data_idx = sample(nrow(HouseVotes84), fraction*nrow(HouseVotes84))
    
    data_train = HouseVotes84[data_idx, ]
    data_test = HouseVotes84[-data_idx, ]

    model = naiveBayes(Class ~ ., data = data_train)
    model_pred = predict(model, newdata=data_test[, -1])

    results[i] <- mean(model_pred == data_test$Class)
  }
  return(results)
}

model_cv = nbRuns(0.7, 100)

summary(model_cv)

# 분포 그래프 확인

library(ggplot2)
ggplot2::ggplot(data.frame(acc=model_cv), aes(x="", y=acc)) +
  geom_boxplot(fill="slategray", color="darkslategray", width=0.5) +
  geom_point(position="jitter", color="royalblue", alpha=0.7) +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())
  









