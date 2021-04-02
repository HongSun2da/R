## X2검점(chi-square test)               #######################################
# 교차표를 이용하면 범주형으로 수집된 두 변수의 범주 조합에 따른 조합별 빈도를
# 살펴볼 수 있으면, 이를 통해 두 변수 간의 관계를 파악할 수 있음

# 범주형 변수 간 관련성이 모집단에서 조재하는지 검정
# 관측빈도 : 교차표상의 실제빈도
# 기대빈도 : 변수 간 서로 관련성이 없을 때 기대할 수 잇는 예상 빈도
# X2검정 결과가 통계적으로 유의하면 변수 간 관련성이 존재

# 전체 비율이 각각의 비율과 비슷(같을)것이다고 가정
# X2 값이 크면 오른쪽으로 멀리 있다.



# 데이터 수집
survivors = matrix(c(1443,151,47,1781,312,135),
                   ncol=2)


dimnames(survivors) = list(status = c("minor injury", "major injury", "dead"),
                           Seatbelt = c("With seatbelt","Without seatbelt"))
survivors



# 기술 통계
View(survivors)
class(survivors)
str(survivors)
summary(survivors)

library(psych)
describe(survivors)



# 전처리
?addmargins # Puts Arbitrary Margins on Multidimensional Tables or Arrays

# addmargins(A, margin = seq_along(dim(A)), FUN = sum, quiet = FALSE)


addmargins(survivors)

#                 Seatbelt
# status         With seatbelt Without seatbelt  Sum
# minor injury          1443             1781     3224
# major injury           151              312     463
# dead                    47              135     182
# Sum                   1641             2228     3869

addmargins(survivors, 1)
addmargins(survivors, 2)

?prop.table() # Express Table Entries as Fraction of Marginal Table

# proportions(x, margin = NULL)
# prop.table(x, margin = NULL)

prop.table(addmargins(survivors, 1), 1)
prop.table(addmargins(survivors, 2), 2)

addmargins(prop.table(addmargins(survivors, 2), 2), 1)



# 데이터 분석
?barplot()

# ## Default S3 method:
# barplot(height, width = 1, space = NULL,
#         names.arg = NULL, legend.text = NULL, beside = FALSE,
#         horiz = FALSE, density = NULL, angle = 45,
#         col = NULL, border = par("fg"),
#         main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
#         xlim = NULL, ylim = NULL, xpd = TRUE, log = "",
#         axes = TRUE, axisnames = TRUE,
#         cex.axis = par("cex.axis"), cex.names = par("cex.axis"),
#         inside = TRUE, plot = TRUE, axis.lty = 0, offset = 0,
#         add = FALSE, ann = !add && par("ann"), args.legend = NULL, ...)
# 
# ## S3 method for class 'formula'
# barplot(formula, data, subset, na.action,
#         horiz = FALSE, xlab = NULL, ylab = NULL, ...)

barplot(survivors,
        ylim = c(0, 2500),
        las = 1,
        col = c("yellowgreen", "lightsalmon", "orangered"),
        ylab = "Frequency",
        main = "Frequency of Survivor")
legend(0.2, 2500, 
       rownames(survivors),
       fill = c("yellowgreen", "lightsalmon", "orangered"))

?legend()
# legend(x, y = NULL, legend, fill = NULL, col = par("col"),
#        border = "black", lty, lwd, pch,
#        angle = 45, density = NULL, bty = "o", bg = par("bg"),
#        box.lwd = par("lwd"), box.lty = par("lty"), box.col = par("fg"),
#        pt.bg = NA, cex = 1, pt.cex = cex, pt.lwd = lwd,
#        xjust = 0, yjust = 1, x.intersp = 1, y.intersp = 1,
#        adj = c(0, 0.5), text.width = NULL, text.col = par("col"),
#        text.font = NULL, merge = do.lines && has.pch, trace = FALSE,
#        plot = TRUE, ncol = 1, horiz = FALSE, title = NULL,
#        inset = 0, xpd, title.col = text.col, title.adj = 0.5,
#        seg.len = 2)



survivors_prop = prop.table(survivors, 2)

barplot(survivors_prop,
        las = 1,
        col = c("yellowgreen", "lightsalmon", "orangered"),
        ylab = "Frequency",
        main = "Frequency of Survivor")


barplot(survivors_prop * 100,
        las = 1,
        col = c("yellowgreen", "lightsalmon", "orangered"),
        ylab = "Frequency",
        main = "Frequency of Survivor")




# X2 검정
survivors

chisq.test(survivors)

# Pearson's Chi-squared test
# X-squared = 45.969, df = 2, p-value = 1.042e-10 (귀무가설 기각) => 안전벨트 유무는 환자의 상태 차이가 0이 아니다.

?pchisq()

pchisq(45.969, df =(3-1) * (2-1), lower.tail = FALSE) # 1.042218e-10

qchisq(0.05, df = 2 ) # 0.05%에 있는 X2 (왼쪽)값은 ? 0.1025866 

qchisq(0.05, df = 2, lower.tail = FALSE ) # 0.05%에 있는 X2 (오른쪽)값은 ? 5.991465







