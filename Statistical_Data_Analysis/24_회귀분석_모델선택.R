## 회귀분석(regression analysis)                  ##############################

# R 포뮬러 심볼
# : > y ~ x + w + x:w
# * > y ~ x * w * z           -> x + w + z + x:w + x:z + w:z + x:w:z
# ^ > y ~ (x + w + x)^2       -> x + w + z + x:w + x:z + w:z
# . > y ~ .                   -> x + w + x
# - > y ~ (x + w + x)^2 - w:z -> x + w + z + x:w + x:z
# I > y ~ x + I((w + z)^2)    -> x + (w,z 합 제곱값)
#   
# - 1. 선형회귀(linear regression equation)
# - 2. 단순회귀분석(simple regression analysis)      - 직선
# - 3. 다항회귀분석(polynomial regression analysis)  - 곡선
# - 4. 다중회귀분석(multiple regression analysis)

#  회귀분석 가정
# - 선형성 
# - 정규성
# - 등분산성
# - 독립성

# 다중공선성(multicollinearity) 확인
# - VIF(variance inflation factor) > 4  (다중공선성 존재 확인 하기)
#                                  > 10 (다중공선성 존재 가능성이 높다)

# 회귀모델 검정
# anova()                  : 분산분석표
# coefficients() / coef()  : 회귀계수
# confint()                : 회귀계수에 대한 신뢰구간
# fitted()                 : 회귀식에 의한 예측값
# residuals() / resid()    : 잔차
# summary()                : 주요 분석 정보(잔차, 회귀계수, R2, F값 등)

################################################################################

# 회귀모델 선택

# 데이터 수집                                    -------------------------------

# 데이터 전처리                                  -------------------------------

# 기술통계                                       -------------------------------

# 데이터 분석                                    -------------------------------

mtcars_lm1 = lm(mpg ~ hp + wt, data=mtcars)
mtcars_lm2 = lm(mpg ~ hp + wt + disp + drat, data=mtcars)

summary(mtcars_lm1)
summary(mtcars_lm2)

# 분산으로 모델 비교                             -------------------------------
anova(mtcars_lm1, mtcars_lm2)

# Model 1: mpg ~ hp + wt
# Model 2: mpg ~ hp + wt + disp + drat
#   Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1     29 195.05                           
# 2     27 182.84  2     12.21 0.9016 0.4178


# AIC 모델 비교
AIC(mtcars_lm1, mtcars_lm2) # AIC 작은 값이 좋음

#            df      AIC
# mtcars_lm1  4 156.6523
# mtcars_lm2  6 158.5837



# 변수 선택                                      -------------------------------

mtcars_lm = lm(mpg ~ hp + wt + disp + drat, data=mtcars)

step(mtcars_lm)

step(mtcars_lm, direction="backward")


?step # {stats} Choose a model by AIC in a Stepwise Algorithm

# step(object, scope, scale = 0,
#      direction = c("both", "backward", "forward"),
#      trace = 1, keep = NULL, steps = 1000, k = 2, ...)


library(leaps)
mtcars_regsubset = leaps::regsubsets(x=mpg ~ hp+wt+disp+drat, data=mtcars,
                                    nbest=4)
?leaps::regsubsets # {leaps} functions for model selection

# regsubsets(x=, ...)
# 
# ## S3 method for class 'formula'
# regsubsets(x=, data=, weights=NULL, nbest=1, nvmax=8,
#            force.in=NULL, force.out=NULL, intercept=TRUE,
#            method=c("exhaustive", "backward", "forward", "seqrep"),
#            really.big=FALSE,
#            nested=(nbest==1),...)
# 
# ## Default S3 method:
# regsubsets(x=, y=, weights=rep(1, length(y)), nbest=1, nvmax=8,
#            force.in=NULL, force.out=NULL, intercept=TRUE,
#            method=c("exhaustive","backward", "forward", "seqrep"),
#            really.big=FALSE,nested=(nbest==1),...)
# 
# ## S3 method for class 'biglm'
# regsubsets(x,nbest=1,nvmax=8,force.in=NULL,
#            method=c("exhaustive","backward", "forward", "seqrep"),
#            really.big=FALSE,nested=(nbest==1),...)
# 
# ## S3 method for class 'regsubsets'
# summary(object,all.best=TRUE,matrix=TRUE,matrix.logical=FALSE,df=NULL,...)
# 
# ## S3 method for class 'regsubsets'
# coef(object,id,vcov=FALSE,...)
# ## S3 method for class 'regsubsets'
# vcov(object,id,...)



str(summary(mtcars_regsubset))


plot(mtcars_regsubset)

plot(mtcars_regsubset, 
     scale="adjr2",
     col=brewer.pal(9, "Pastel1"))


?plot.regsubsets # {leaps} Graphical table of best subsets

# ## S3 method for class 'regsubsets'
# plot(x, labels=obj$xnames, main=NULL, scale=c("bic", "Cp", "adjr2", "r2"),
#      col=gray(seq(0, 0.9, length = 10)),...)




# 최적화 모델
summary(lm(mpg ~ hp + wt + drat, data=mtcars))  # adjr2 = 0.82













