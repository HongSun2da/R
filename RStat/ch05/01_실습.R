######################################
# 그래프 한글표시

install.packages("extrafont")





######################################
# 데이터 불려오기

freq = read.csv("0301.frequency.csv",
                header = TRUE,
                na.strings = ".")
str(freq)
summary(freq)
describe(freq)

boxplot(freq$grade)
hist(freq$grade)

freq$grade = factor(freq$grade,
                    levels=c(1:5),
                    labels=c("F","D","C","B","A"))
str(freq)
summary(freq)
describe(freq)

######################################
# 02. 일변량 범주형 자료

barplot(freq$grade)

ta.grade = table(freq$grade)
str(ta.grade)

barplot(ta.grade)

barplot(ta.grade,
        main = "학점별 분포",
        xlab = "학점",
        ylab = "명",
        ylim = c(0, 30),
        legend = rownames(ta.grade))

# help 예시
barplot(height = cbind(x = c(465, 91) / 465 * 100,
                       y = c(840, 200) / 840 * 100,
                       z = c(37, 17) / 37 * 100),
        beside = FALSE,
        width = c(465, 840, 37),
        col = c(1, 2),
        legend.text = c("A", "B"),
        args.legend = list(x = "topleft"))


barplot(ta.grade,
        main = "학점별 분포",
        xlab = "학점",
        ylab = "명",
        xlim = c(0, 30),
        horiz = TRUE,
        col = heat.colors(5),
        legend = rownames(ta.grade))

######################################
# 03. 원그래프

ta.grade

pie(ta.grade)

pie(ta.grade,
    main="학점별 분포",
    init.angle = 90,
    col = rainbow(length(ta.grade)))

legend(1,1,
       rownames(ta.grade),
       cex=1,
       fill=rainbow(length(ta.grade)))        


######################################
# 04. 3D 원그래프

# install.packages("plotrix")
library(plotrix)

pie3D(ta.grade,
    main="학점별 분포",
    labels=ta.grade,
    explode = 0.3,    
    col = rainbow(length(ta.grade)))

legend(0.5,1,
       rownames(ta.grade),
       cex=1,
       fill=rainbow(length(ta.grade)))        





######################################
# 다변량 범주형 자료
## 데이터 불려오기

pre = read.csv("0302.pre.csv",
               header = TRUE,
               na.strings = ".")
str(pre)

pre$treat = factor(pre$treat,
                   levels = c(1,2),
                   labels=c("비타민","Placebo"))
pre$cold = factor(pre$cold,
                   levels = c(1,2),
                   labels=c("Cold","noCold"))
summary(pre)


pre.table = table(pre$treat, pre$cold)
pre.table

barplot(pre.table,
        main = "비타민섭취에 따른 감기유병률",
        xlab = "집단",
        ylab = "%",
        col = c("blue", "red"),
        legend = rownames(pre.table))

barplot(pre.table,
        main = "비타민섭취에 따른 감기유병률",
        xlab = "집단",
        ylab = "%",
        col = c("blue", "red"),
        beside = TRUE,
        legend = rownames(pre.table))


######################################
# 모자이크 그래프

mosaicplot(pre.table,
           shade=TRUE,
           xlab = "treat",
           ylab="Cold Y/N",
           main="비타민섭취에 따른 감기유형")

######################################
# 연속 자료를 범주 자료로 변환


wgt = read.csv("0401.wgt.csv",
               header=TRUE,
               na.strings=".")
wgt
summary(wgt)

wgt$sex = factor(wgt$sex,
                 levels=c(1,2),
                 labels=c("남자", "여자"))
str(wgt)

wgt = transform(wgt,
                wgt.cut = cut(weight,
                              breaks=c(0,45,50,55,60,65,70,100),
                              right=FALSE,
                              labels=c("~40","45~50","50~55","55~60","66~65","65~70","70~")))
wgt
summary(wgt)

barplot(table(wgt$wgt.cut))

hist(wgt$weight)


######################################
# 08.다변량 수치형 자료

boxplot(weight~sex, data=wgt)



######################################
# 

load("game.RData")

summary(game)



######################################
# 09.고급 그래프 그리기

library(ggplot2)

ggplot(wgt, aes(x=sex)) + geom_bar()


ggplot(wgt, aes(x=sex, y=weight)) +
        geom_boxplot(position = "dodge") +
        ggtitle("sex / weight")









