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

# . 이원분산분석 (two-way ANOVA)------------------------------------------------
# 집단을 구분하는 독립변수가 두 개일 때
# 주효과(main effect) 검정 : 각 독립변수에 의해 만들어지는 집단간 평균의 차리에 대한 검정
# 상호작용효과(interaction effect) 검정 : 두 독립변수의 조합에 의해 만들어지는 집단 간 평균의 차이에 대한 검정

View(ToothGrowth)
str(ToothGrowth)

# 전처리
ToothGrowth$dose = factor(ToothGrowth$dose,
                          levels = c(0.5, 1.0, 2.0),
                          labels = c("low", "med", "high"))
# 기술통계
library(psych)
summary(ToothGrowth)
describe(ToothGrowth)

?with
# with(data, expr, ...)
# within(data, expr, ...)
# ## S3 method for class 'list'
# within(data, expr, keepAttrs = TRUE, ...)

with(ToothGrowth, tapply(len, list(supp, dose), length))
with(ToothGrowth, tapply(len, list(supp, dose), mean))
with(ToothGrowth, tapply(len, list(supp, dose), sd))

describeBy(ToothGrowth$len, ToothGrowth$dose, mat = TRUE)


# 이원분산분석 실행
ToothGrowth_aov = aov(len ~ supp * dose, data = ToothGrowth)

ToothGrowth_aov = aov(len ~ supp + dose + supp:dose, data = ToothGrowth)

# aov(formula = len ~ supp * dose, data = ToothGrowth)
# Terms:
#                     supp     dose supp:dose Residuals
# Sum of Squares   205.350 2426.434   108.319   712.106
# Deg. of Freedom        1        2         2        54

summary(ToothGrowth_aov)
#             Df Sum Sq Mean Sq F value   Pr(>F)    
# supp         1  205.4   205.4  15.572 0.000231 ***  (귀무가설 기각)
# dose         2 2426.4  1213.2  92.000  < 2e-16 ***
# supp:dose    2  108.3    54.2   4.107 0.021860 *  
# Residuals   54  712.1    13.2     

?model.tables() # Compute Tables of Results from an Aov Model Fit
 
# model.tables(x, ...)
# 
# ## S3 method for class 'aov'
# model.tables(x, type = "effects", se = FALSE, cterms, ...)
# 
# ## S3 method for class 'aovlist'
# model.tables(x, type = "effects", se = FALSE, ...)

model.tables(ToothGrowth_aov, type = "means")

model.tables(ToothGrowth_aov, type = "effects")




# 그래프로 확인 하기
boxplot(len ~ supp * dose, data = ToothGrowth)

boxplot(len ~ supp * dose, data = ToothGrowth,
        col = c("deeppink","yellowgreen"),
        las = 1,
        xlab = "Vitamin C Type",
        ylab = "Tooth Growth",
        main = "Effects of Vitamin C on Tooth Grouth of Guinea Pigs")


?interaction.plot() # Two-way Interaction Plot
# interaction.plot(x.factor, trace.factor, response, fun = mean,
#                  type = c("l", "p", "b", "o", "c"), legend = TRUE,
#                  trace.label = deparse1(substitute(trace.factor)),
#                  fixed = FALSE,
#                  xlab = deparse1(substitute(x.factor)),
#                  ylab = ylabel,
#                  ylim = range(cells, na.rm = TRUE),
#                  lty = nc:1, col = 1, pch = c(1:9, 0, letters),
#                  xpd = NULL, leg.bg = par("bg"), leg.bty = "n",
#                  xtick = FALSE, xaxt = par("xaxt"), axes = TRUE,
#                  ...)

interaction.plot(x.factor = ToothGrowth$dose,
                 trace.factor = ToothGrowth$supp,
                 response = ToothGrowth$len,
                 trace.label = "Supplement",
                 las = 1,
                 type = "b",
                 pch = c(1, 19),
                 col = c("blue", "red"),
                 xlab = "Dose Level",
                 ylab = "Tooth Lengt",
                 main = "Interaction Plot for Tooth Growth of Guinea Pigs")

library(gplots)

?plotmeans()  # Plot Group Means and Confidence Intervals
# plotmeans(formula, data=NULL, subset, na.action,
#           bars=TRUE, p=0.95, minsd=0, minbar, maxbar,
#           xlab=names(mf)[2], ylab=names(mf)[1], mean.labels=FALSE,
#           ci.label=FALSE, n.label=TRUE, text.n.label="n=",
#           digits=getOption("digits"), col="black", barwidth=1,
#           barcol="blue", connect=TRUE, ccol=
#             col, legends=names(means), xaxt, use.t=TRUE,
#           lwd=par("lwd"), ...)

plotmeans(len ~ interaction(supp, dose, sep="-"), data = ToothGrowth)

plotmeans(len ~ interaction(supp, dose, sep="-"), data = ToothGrowth,
          col = c("red","green3"),
          connect = list(c(1,3,5), c(2,4,6)))

?coplot()  # Conditioning Plots
# coplot(formula, data, given.values, panel = points, rows, columns,
#        show.given = TRUE, col = par("fg"), pch = par("pch"),
#        bar.bg = c(num = gray(0.8), fac = gray(0.95)),
#        xlab = c(x.name, paste("Given :", a.name)),
#        ylab = c(y.name, paste("Given :", b.name)),
#        subscripts = FALSE,
#        axlabels = function(f) abbreviate(levels(f)),
#        number = 6, overlap = 0.5, xlim, ylim, ...)
# co.intervals(x, number = 6, overlap = 0.5)

coplot(len ~ dose | supp, data = ToothGrowth)

coplot(len ~ dose | supp, data = ToothGrowth,
       col = "steelblue",
       pch=19)

#install.packages("HH")
library(HH)


?interaction2wt() # Plot all main effects and twoway interactions in a multifactor

# interaction2wt(x, ...)
# 
# ## S3 method for class 'formula'
# interaction2wt(x, data=NULL, responselab, ...)
# 
# ## Default S3 method:
# interaction2wt(x,
#                response.var,
#                responselab = deparse(substitute(response.var)),
#                ...

interaction2wt(len ~ supp * dose, data = ToothGrowth)




# 사후 분석
?TukeyHSD() # Compute Tukey Honest Significant Differences
#TukeyHSD(x, which, ordered = FALSE, conf.level = 0.95, ...)

TukeyHSD(ToothGrowth_aov)

TukeyHSD(ToothGrowth_aov, which = c("dose"), conf.level = 0.99)





