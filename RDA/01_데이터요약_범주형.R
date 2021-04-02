# 빈도표
library(MASS)

View(survey)
levels(survey$Smoke)

# table 객체 만들기
frqtab = table(survey$Smoke)
str(frqtab)

# Heavy Never Occas Regul 
# 11   189    19    17 

frqtab[2]

# 최빈값
frqtab == max(frqtab)

frqtab[frqtab == max(frqtab)]

which.max(frqtab)
which.min(frqtab)

names(which.min(frqtab))

# 비율
frqtab.prop = prop.table(frqtab)
frqtab.prop
#      Heavy      Never      Occas      Regul 
# 0.04661017 0.80084746 0.08050847 0.07203390 

frqtab.prop * 100

# 담배를 피지 않는 사람의 비율
mean(survey$Smoke == "Never", na.rm=TRUE) # 0.8008475


View(anorexia)
str(anorexia)
summary(anorexia)

# 치료 전후 몸무게
mean(anorexia$Prewt < anorexia$Postwt)


View(mammals)
str(mammals)
summary(mammals)


abs(mammals$brain - mean(mammals$brain)) > 2*sd(mammals$brain)

mean(abs(mammals$brain - mean(mammals$brain)) > 2*sd(mammals$brain))


View(SP500)
head(SP500)
str(SP500)
summary(SP500)

diff(SP500)


mean(diff(SP500) > 0)

#install.packages("vcd")
library(vcd)

View(Arthritis)
str(Arthritis)
summary(Arthritis)


levels(Arthritis$Treatment)

# table 만들기

crosstab = table(Arthritis$Improved, Arthritis$Treatment)
crosstab
str(crosstab)
crosstab[1,2]
crosstab[1,]

crosstab = table(Arthritis$Improved, Arthritis$Treatment,
                 dnn=c("Improved","Treatment"))


crosstab = xtabs(~ Improved+Treatment, data = Arthritis)
crosstab

str(crosstab)

margin.table(crosstab, margin = 1) # Row
margin.table(crosstab, margin = 2) # Column

prop.table(crosstab, margin = 1)
prop.table(crosstab, margin = 2)

prop.table(crosstab)

addmargins(crosstab, margin = 1)
addmargins(crosstab, margin = 2)

addmargins(crosstab)

library(gmodels)
# CrossTable(x, y, digits=3, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE,
#            prop.t=TRUE, prop.chisq=TRUE, chisq = FALSE, fisher=FALSE, mcnemar=FALSE,
#            resid=FALSE, sresid=FALSE, asresid=FALSE,
#            missing.include=FALSE,
#            format=c("SAS","SPSS"), dnn = NULL, ...)

CrossTable(Arthritis$Improved, Arthritis$Treatment)


# 다차원 Table 
multab = table(Arthritis$Improved, Arthritis$Sex, Arthritis$Treatment)
multab
str(multab)


multab = xtabs(~ Arthritis$Improved+Arthritis$Sex+Arthritis$Treatment)
multab
str(multab)


ftable(multab)
ftable(multab, row.vars = c(2,3))


ftable(Arthritis[c("Improved","Sex","Treatment")],
       row.vars = c(2,3))













