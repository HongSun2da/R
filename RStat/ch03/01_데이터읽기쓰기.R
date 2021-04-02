
# 데이터 읽기
gradecsv <- read.csv("0202.grade.csv", 
                     header=TRUE,
                     na.strings = ".")

str(gradecsv)
summary(gradecsv)

gradecsv$csex <- factor(gradecsv$csex, levels = c(1,2), labels = c("남자", "여자"))

str(gradecsv)
summary(gradecsv)

# 데이터 내보내기
write.csv(gradecsv, file = "gradecsv.csv", row.names = FALSE, na = "")

# R 데이터로 저장하기
save(gradecsv, file = "grade.RData")
load(file="grade.RData")
