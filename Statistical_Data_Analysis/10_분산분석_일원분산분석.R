## 분산분석 (analysis of variance, ANOVA)             ##########################
# 여러 모집단(3개 이상)간의 평균의 동일성 검정 => 분산이동
#
# - 1. 일원분산분석 (one-way ANOVA)
# - 2. 이원분산분석 (two-way ANOVA)
# - 3. 공분산분석 (analysis of covariance, ANCOVA)
# - 4. 반복측정 분산분석 (repeated measures ANOVA)
# - 5. 다변량 분산분석 (multivariate analysis of variance, MANOVA)
# - 6. 다변량 공분산분석 (multivariate analysis of convariance, MANOCOVA)
################################################################################

# . 일원분산분석 (one-way ANOVA)------------------------------------------------
# 집단을 구분하는 독립변수가 한 개일 때 
# 귀무가설 : 집단 간 평균은 모두 동일 하다.


View(InsectSprays)
str(InsectSprays)

summary(InsectSprays)

describeBy(InsectSprays$count, InsectSprays$spray, mat = TRUE)

?tapply
# tapply(X, INDEX, FUN = NULL, ..., default = NA, simplify = TRUE)

tapply(InsectSprays$count, InsectSprays$spray, length)
tapply(InsectSprays$count, InsectSprays$spray, mean)
tapply(InsectSprays$count, InsectSprays$spray, sd)

#install.packages("gplots")
library(gplots)

# 집단별 평균값 그래프 그리기
?plotmeans
# plotmeans(formula, data=NULL, subset, na.action,
#           bars=TRUE, p=0.95, minsd=0, minbar, maxbar,
#           xlab=names(mf)[2], ylab=names(mf)[1], mean.labels=FALSE,
#           ci.label=FALSE, n.label=TRUE, text.n.label="n=",
#           digits=getOption("digits"), col="black", barwidth=1,
#           barcol="blue", connect=TRUE, ccol=
#             col, legends=names(means), xaxt, use.t=TRUE,
#           lwd=par("lwd"), ...)

plotmeans(count ~ spray, data = InsectSprays)

plotmeans(count ~ spray, data = InsectSprays,
          barcol = "tomato",
          barwidth = 3,
          col = "cornflowerblue",
          lwd = 2,
          xlab = "Type of Sprays",
          ylab = "Insect Count",
          main = "Performance of Insect Sprays")

?boxplot
## S3 method for class 'formula'
# boxplot(formula, data = NULL, ..., subset, na.action = NULL,
#         xlab = mklab(y_var = horizontal),
#         ylab = mklab(y_var =!horizontal),
#         add = FALSE, ann = !add, horizontal = FALSE,
#         drop = FALSE, sep = ".", lex.order = FALSE)
# 
# ## Default S3 method:
# boxplot(x, ..., range = 1.5, width = NULL, varwidth = FALSE,
#         notch = FALSE, outline = TRUE, names, plot = TRUE,
#         border = par("fg"), col = "lightgray", log = "",
#         pars = list(boxwex = 0.8, staplewex = 0.5, outwex = 0.5),
#         ann = !add, horizontal = FALSE, add = FALSE, at = NULL)

boxplot(count ~ spray, data = InsectSprays)

boxplot(count ~ spray, data = InsectSprays,
        barcol = "tomato",
        barwidth = 3,
        col = "cornflowerblue",
        lwd = 2,
        xlab = "Type of Sprays",
        ylab = "Insect Count",
        main = "Performance of Insect Sprays")


# 집단 간 평균 차이는 0 이다 (모두 동일) - 귀무가설

spray_aov = aov(count ~ spray, data = InsectSprays)
summary(spray_aov)
str(spray_aov)
#             Df Sum Sq Mean Sq F value Pr(>F)    
# spray        5   2669   533.8    34.7 <2e-16 *** (귀무가설 기각)
# Residuals   66   1015    15.4     


# 각 집단간의 비교
?model.tables
# ## S3 method for class 'aov'
# model.tables(x, type = "effects", se = FALSE, cterms, ...)
# 
# ## S3 method for class 'aovlist'
# model.tables(x, type = "effects", se = FALSE, ...)

model.tables(spray_aov, type = "mean")
model.tables(spray_aov, type = "effects") # 평균 차이


# 사후 분석 (그룹간 차이 확인 하기)
?TukeyHSD

# TukeyHSD(x, which, ordered = FALSE, conf.level = 0.95, ...)

spray_compare = TukeyHSD(spray_aov)

str(spray_compare)
spray_compare$spray["D-C", ]


# 그래프 확인 하기
plot(spray_compare)

plot(spray_compare, 
     col="blue",
     las = 1)

#install.packages("multcomp")
library(multcomp)
tuk_hsd = glht(model = spray_aov, linfct = mcp(spray="Tukey"))
summary(tuk_hsd)

cld(tuk_hsd, level=0.5)

plot(cld(tuk_hsd, level=0.5),
     col = "orange",
     las = 1)


# 정규성 확인 하기
library(car)
?qqPlot
# qqPlot(x, distribution="norm", groups, layout,
#        ylim=range(x, na.rm=TRUE), ylab=deparse(substitute(x)),
#        xlab=paste(distribution, "quantiles"), glab=deparse(substitute(groups)),
#        main=NULL, las=par("las"),
#        envelope=.95, col=carPalette()[1], col.lines=carPalette()[2],
#        lwd=2, pch=1, cex=par("cex"),
#        line=c("quartiles", "robust", "none"), id=TRUE, grid=TRUE, ...)

qqPlot(InsectSprays$count)

qqPlot(InsectSprays$count[InsectSprays$spray == "A"])
qqPlot(InsectSprays$count[InsectSprays$spray == "B"])
qqPlot(InsectSprays$count[InsectSprays$spray == "C"])
qqPlot(InsectSprays$count[InsectSprays$spray == "D"])
qqPlot(InsectSprays$count[InsectSprays$spray == "F"])

qqPlot(InsectSprays$count, 
       id = FALSE,
       pch=20,
       col="deepskyblue",
       xlab = "Theoretical Quantiles",
       ylab  = "Empirical Quantiles",
       main = "Q-Q plot")


# 정규성 검정 하기
?shapiro.test
# shapiro.test(rnorm(100, mean = 5, sd = 3))
# shapiro.test(runif(100, min = 2, max = 4))

shapiro.test(InsectSprays$count)
# W = 0.9216, p-value = 0.0002525 (귀무가설 기각) - 정규성이 아니다

shapiro.test(InsectSprays$count[InsectSprays$spray == "A"])
shapiro.test(InsectSprays$count[InsectSprays$spray == "B"])
shapiro.test(InsectSprays$count[InsectSprays$spray == "C"])
shapiro.test(InsectSprays$count[InsectSprays$spray == "D"])
shapiro.test(InsectSprays$count[InsectSprays$spray == "F"])



# 이상치 확인 
outlierTest(spray_aov)


# 집단간 분포 동일성 확인 하기

?leveneTest
# leveneTest(y, ...)
# ## S3 method for class 'formula'
# leveneTest(y, data, ...)
# ## S3 method for class 'lm'
# leveneTest(y, ...)
# ## Default S3 method:
# leveneTest(y, group, center=median, ...)

leveneTest(count ~ spray, data = InsectSprays)

?bartlett.test
# bartlett.test(x, ...)
# 
# ## Default S3 method:
# bartlett.test(x, g, ...)
# 
# ## S3 method for class 'formula'
# bartlett.test(formula, data, subset, na.action, ...)

bartlett.test(count ~ spray, data = InsectSprays)
# Bartlett's K-squared = 25.96, df = 5, p-value = 9.085e-05 (귀무가설 기각) - 분산 동일성이 없다


oneway.test(count ~ spray, data = InsectSprays)

# One-way analysis of means (not assuming equal variances)
# 
# data:  count and spray
# F = 36.065, num df = 5.000, denom df = 30.043, p-value = 7.999e-12 ( 귀무가설 기각) 

oneway.test(count ~ spray, data = InsectSprays,
            var.equal = TRUE)
# 등분산이 동일하는 가정하게 검정 하기 = aov와 같은 결과를 보여줌 ★★★
# One-way analysis of means
# 
# data:  count and spray
# F = 34.702, num df = 5, denom df = 66, p-value < 2.2e-16


