## 의사결정나무 ()  ############################################################
# - 
# - 
# - 



################################################################################


# 데이터 수집    ---------------------------------------------------------------

library(mlbench)
data("BreastCancer")

str(BreastCancer) # 'data.frame':	699 obs. of  11 variables:
View(BreastCancer)

summary(BreastCancer)




# 기술 통계      ---------------------------------------------------------------
library(psych)
psych::describe(BreastCancer)
psych::pairs.panels(BreastCancer)



# 데이터 전처리  ---------------------------------------------------------------
table(BreastCancer$Class)
# benign malignant 
# 458       241 

mean(BreastCancer$Class=="benign")    # 0.6552217
mean(BreastCancer$Class=="malignant") # 0.3447783

?complete.cases

sum(!complete.cases(BreastCancer))

data = BreastCancer[, -1]


#-------------------------------------------------------------------------------
?lapply # {base} Apply a Function over a List or Vector

# lapply(X, FUN, ...)
# 
# sapply(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)
# 
# vapply(X, FUN, FUN.VALUE, ..., USE.NAMES = TRUE)
# 
# replicate(n, expr, simplify = "array")
# 
# simplify2array(x, higher = TRUE)
#-------------------------------------------------------------------------------


# 데이터 numeric형으로 변경하기
data = cbind(lapply(data[, -10],
                     function(x) as.numeric(as.character(x))), 
             data[10])
str(data)

# 데이터 분리 하기

set.seed(567)
data_idx = sample(nrow(data), 0.7*nrow(data))
data_train = data[data_idx, ]
data_test = data[-data_idx, ]

table(data_train$Class)
# benign malignant 
# 313       176 

table(data_test$Class)
# benign malignant 
# 145        65 





# 데이터 분석    ---------------------------------------------------------------
library(rpart)


#-------------------------------------------------------------------------------
?rpart::rpart # {rpart} Recursive Partitioning and Regression Trees

# rpart(formula, data, weights, subset, na.action = na.rpart, method,
#       model = FALSE, x = FALSE, y = TRUE, parms, control, cost, ...)
# class : "anova", "poisson", "class" or "exp"
#-------------------------------------------------------------------------------


model = rpart::rpart(formula=Class ~ ., 
                     data=data_train,
                     method="class",
                     parm=list(split="information"))
model
class(model) # rpart

# 1) root 489 174 benign (0.644171779 0.355828221)  
#   2) Cell.size< 2.5 299  12 benign (0.959866221 0.040133779)  
#     4) Bare.nuclei< 2.5 271   1 benign (0.996309963 0.003690037) *
#     5) Bare.nuclei>=2.5 28  11 benign (0.607142857 0.392857143)  
#       10) Cl.thickness< 3.5 13   0 benign (1.000000000 0.000000000) *
#       11) Cl.thickness>=3.5 15   4 malignant (0.266666667 0.733333333) *
#   3) Cell.size>=2.5 190  28 malignant (0.147368421 0.852631579)  
#     6) Cell.size< 4.5 69  25 malignant (0.362318841 0.637681159)  
#       12) Bare.nuclei< 3.5 24   6 benign (0.750000000 0.250000000) *
#       13) Bare.nuclei>=3.5 45   7 malignant (0.155555556 0.844444444) *
#     7) Cell.size>=4.5 121   3 malignant (0.024793388 0.975206612) *
  


library(rpart.plot)

#-------------------------------------------------------------------------------
?rpart.plot # {rpart.plot} Plot an rpart model. A simplified interface to the prp function.

# rpart.plot(x = stop("no 'x' arg"),
#            type = 2, extra = "auto",
#            under = FALSE, fallen.leaves = TRUE,
#            digits = 2, varlen = 0, faclen = 0, roundint = TRUE,
#            cex = NULL, tweak = 1,
#            clip.facs = FALSE, clip.right.labs = TRUE,
#            snip = FALSE,
#            box.palette = "auto", shadow.col = 0,
#            ...)
#-------------------------------------------------------------------------------

rpart.plot::rpart.plot(model)

rpart.plot::prp(model,
                type=2,
                extra=104,
                fallen.leaves=TRUE,
                roundint=FALSE,
                main="Decision Tree from Wisconsin Breast Cancer Dataset")

model_pred = predict(model,
                     newdata=data_test,
                     type="prob")
model_pred
class(model_pred) # factor

model_pred = predict(model,
                     newdata=data_test,
                     type="class")
model_pred


# 예측 검정
table(data_test$Class, 
      model_pred,
      dnn=c("Actual","Predicted"))

#             Predicted
# Actual      benign malignant
# benign       141         2
# malignant      4        63

# 정확도 0.9714286
mean(data_test$Class == model_pred)




# 모델 튜닝 (가지치기)

model$cptable

rpart::printcp(model) # 0.020115 

rpart::plotcp(model)

model_prun = rpart::prune(model,
                          cp=0.020115)
model_prun

rpart.plot::rpart.plot(model_prun)


model_pred = predict(model_prun,
                     newdata=data_test,
                     type="class")
model_pred


# 예측 검정
table(data_test$Class, 
      model_pred,
      dnn=c("Actual","Predicted"))

#             Predicted
# Actual      benign malignant
# benign       141         2
# malignant      4        63

# 정확도 0.9714286
mean(data_test$Class == model_pred)


model_prun$cptable


# 다른 그래프
library(rattle)
rattle::fancyRpartPlot(model_prun, sub=NULL)















