## 군집분석  ###################################################################
# 1. 유사도 측정
# 2. 계층적 군집분석
# 3. k-평균 군집분석
# 4. PAM군집분석

################################################################################
# 1. 유사도 측정
# - 연속형 변수 : 유클리드거리(euclidean distance), 맨해튼거리(manhattan distance)
# - 


################################################################################

# 데이터 수집    ---------------------------------------------------------------
#install.packages("flexclust")
library(flexclust)

data(nutrient)
View(nutrient)

str(nutrient) # 'data.frame':	27 obs. of  5 variables:

# 기술 통계      ---------------------------------------------------------------
library(psych)

psych::describe(nutrient)
psych::pairs.panels(nutrient)

# 데이터 전처리  ---------------------------------------------------------------




# 데이터 분석    ---------------------------------------------------------------

#-------------------------------------------------------------------------------
?dist # {stats} Distance Matrix Computation

# dist(x, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
# 
# as.dist(m, diag = FALSE, upper = FALSE)
# ## Default S3 method:
# as.dist(m, diag = FALSE, upper = FALSE)
# 
# ## S3 method for class 'dist'
# print(x, diag = NULL, upper = NULL,
#       digits = getOption("digits"), justify = "none",
#       right = TRUE, ...)
# 
# ## S3 method for class 'dist'
# as.matrix(x, ...)
#-------------------------------------------------------------------------------

distance = dist(nutrient, method="euclidean")

str(distance)
View(as.matrix(distance))





# 데이터 분석(범주형)-----------------------------------------------------------
library(MASS)

str(survey)
View(survey)

survey_dummy = survey[c("Sex","Smoke")]
survey_dummy

#install.packages("fastDummies")
library(fastDummies)

#-------------------------------------------------------------------------------
?fastDummies::dummy_cols # {fastDummies} Fast creation of dummy variables

# dummy_cols(
#   .data,
#   select_columns = NULL,
#   remove_first_dummy = FALSE,
#   remove_most_frequent_dummy = FALSE,
#   ignore_na = FALSE,
#   split = NULL,
#   remove_selected_columns = FALSE
# )
#-------------------------------------------------------------------------------

survey_dummy = fastDummies::dummy_cols(survey_dummy,
                                        remove_selected_columns=TRUE,
                                        remove_first_dummy=TRUE,
                                        ignore_na=TRUE)
survey_dummy

distance = dist(survey_dummy, method="binary")

as.matrix(distance)[1:5, 1:5]
#     1         2         3         4         5
# 1 0.0 1.0000000 1.0000000 0.5000000 0.5000000
# 2 1.0 0.0000000 0.6666667 0.6666667 0.6666667
# 3 1.0 0.6666667 0.0000000 0.6666667 0.6666667
# 4 0.5 0.6666667 0.6666667 0.0000000 0.0000000
# 5 0.5 0.6666667 0.6666667 0.0000000 0.0000000



# 자동
library(cluster)

d = cluster::daisy(survey, metric="gower")

as.matrix(d)[1:5, 1:5]
#           1         2         3         4         5
# 1 0.0000000 0.4672569 0.5572454 0.3098419 0.3676055
# 2 0.4672569 0.0000000 0.5059821 0.4199064 0.6215528
# 3 0.5572454 0.5059821 0.0000000 0.2942501 0.5678793
# 4 0.3098419 0.4199064 0.2942501 0.0000000 0.3069926
# 5 0.3676055 0.6215528 0.5678793 0.3069926 0.0000000







