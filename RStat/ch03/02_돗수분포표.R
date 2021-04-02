# 데이터 가져오기
freq = read.csv("0301.frequency.csv",
                header = TRUE,
                na.strings = ".")
str(freq)
summary(freq)

freq$grade = factor(freq$grade,
                    levels = c(1:5),
                    labels = c("F", "D", "C", "B", "A")
                    )
str(freq)
summary(freq)


# 돗수 분포표 --------------------

# 빈도수
grade.n = table(freq$grade)
grade.n
str(grade.n)

#상대빈도(%)
grade.p = prop.table(grade.n)
grade.p

# 빈도 + 상대빈도
grade.t = cbind(grade.n, grade.p)
grade.t

# 빈도 + 상대빈도 + 합계
grade.a = addmargins(grade.t, margin = 1)
grade.a

grade.a2 = addmargins(grade.t, margin = 2)
grade.a2

grade.all = addmargins(grade.t)
grade.all









