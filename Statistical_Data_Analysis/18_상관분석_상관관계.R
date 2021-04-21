## 상관관계(correlation)                    ####################################

# 두 변수는 일반적으로 연속형 변수를 가정

# 상관계수(correlation coefficient)
# - 두 변수 간의 관계 강도
# - -1 < 계수 < 1
# 귀무가설 : 두 변수간 상관계수는 차이가 0 이다.

# "pearson" [연속성] vs "kendall", "spearman" [서열 측도]
# - pearson 상관계수는 정규성의 가정
# - spearman 상관계수는 정규성의 가정을 충족하지 못하는 서열척도의 데이터를 바탕으로 계산(이상점 덜 민감)




# 데이터 수집                               ------------------------------------
library(MASS)
View(cats)




# 기술 통계                                 ------------------------------------
summary(cats)

library(psych)
describe(cats)

describeBy(cats, cats$Sex, mat = T)
pairs.panels(cats)




# 데이터 분석                               ------------------------------------
plot(cats$Hwt ~ cats$Bwt,
     col = "forestgreen",
     pch = 19,
     xlab = "Body weight(kg)",
     ylab = "Heard weight(kg)",
     main = "Body Weight and Heart Weight of Cats")

cor(cats$Hwt, cats$Bwt)

?cor() # Correlation, Variance and Covariance (Matrices)



# var(x, y = NULL, na.rm = FALSE, use)
# 
# cov(x, y = NULL, use = "everything",
#     method = c("pearson", "kendall", "spearman"))
# 
# cor(x, y = NULL, use = "everything",
#     method = c("pearson", "kendall", "spearman"))
# 
# cov2cor(V)

with(cats, cor(Bwt, Hwt))

?with() # Evaluate an Expression in a Data Environment

# with(data, expr, ...)
# within(data, expr, ...)
# ## S3 method for class 'list'
# within(data, expr, keepAttrs = TRUE, ...)


cor.test(cats$Hwt, cats$Bwt)

# t = 16.119, df = 142, p-value < 2.2e-16

?cor.test() # Test for Association/Correlation Between Paired Samples

# ## Default S3 method:
# cor.test(x, y,
#          alternative = c("two.sided", "less", "greater"),
#          method = c("pearson", "kendall", "spearman"),
#          exact = NULL, conf.level = 0.95, continuity = FALSE, ...)
# 
# ## S3 method for class 'formula'
# cor.test(formula, data, subset, na.action, ...)


cor.test(~ Hwt + Bwt, data = cats)


cor.test(~ Hwt + Bwt, data = cats, subset=(Sex=="F"))




# 데이터 분석2                               ------------------------------------
# 상관계수 행렬
View(iris)

cor(iris[-5])

iris_cor = cor(iris[-5])
class(iris_cor)

iris_cor[1,2]


library(psych)
corr.test(iris[-5])

print(corr.test(iris[-5]), short=FALSE)


View(state.x77)
str(state.x77)

class(state.x77)

summary(state.x77)


cor(state.x77)

pairs.panels(state.x77)



#install.packages("corrgram")
library(corrgram)

corrgram(state.x77)

?corrgram() # Draw a correlogram

# corrgram(x, type = NULL, order = FALSE, labels, panel = panel.shade,
#          lower.panel = panel, upper.panel = panel, diag.panel = NULL,
#          text.panel = textPanel, label.pos = c(0.5, 0.5), label.srt = 0,
#          cex.labels = NULL, font.labels = 1, row1attop = TRUE, dir = "",
#          gap = 0, abs = FALSE, col.regions = colorRampPalette(c("red", "salmon",
#                                                                 "white", "royalblue", "navy")), cor.method = "pearson",
#          outer.labels = NULL, ...)





