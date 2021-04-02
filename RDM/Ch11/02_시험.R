
## k-평균 군집분석                      ########################################


# 01. 데이터 불러오기                  -----------------------------------------
data_df = read.csv("Utilities.csv",
                   header = TRUE,
                   na.strings = ".")
View(data_df)
str(data_df)
summary(data_df)



# 02. 데이터 전처리 하기               -----------------------------------------
# - 1. Compary -> row name으로 지정
row.names(data_df) = data_df[, 1]

data_df = data_df[, -1]   # compary 삭제
data_df


# - 2. 데이터 정규화(표준화)
data_df_norm = sapply(data_df, scale)

row.names(data_df_norm) = row.names(data_df)

data_df_norm



# 03. 기술통계 확인 하기               -----------------------------------------
library(psych)

describe(data_df_norm)

pairs.panels(data_df_norm)




# 04. k-평균 하기                      -----------------------------------------
set.seed(2)

model = kmeans(data_df_norm, 6)

model




# 05. 결과 분석                        -----------------------------------------
plot(c(0),
     xaxt = "n",
     ylab = "",
     type = "l",
     ylim = c(min(model$centers),
              max(model$centers)),
     xlim = c(0, 8))

# min(model$centers)

# X축 라벨
axis(1, at=c(1:8), labels = names(data_df))

for (i in c(1:6))
lines(model$centers[i, ],
      lty = i,
      lwd = 2, 
      col = ifelse(i %in% c(1,3,5), "black","red"))

text(x = 0.5,
     y = model$centers[, 1],
     labels = paste("Cluster", c(1:6)))




# 05. 엘보우 챠트                      -----------------------------------------
model$withinss
model$tot.withinss

set.seed(2)
k_max = 6
elbow = sapply(1:k_max,
               function(k){kmeans(data_df_norm, 
                                  k, 
                                  nstart = 50, 
                                  iter.max = 15)$tot.withiness
                 })

elbow

plot(1:k_max,
     elbow,
     type="b",
     pch=19,
     frame=FALSE)



