################################################################################
# 통계분석[Statistical Analysis]
#         차이검정
#                 t-검정   - 2개
#                 (t-test)      
#                               일표본 t-검정   - 1개             [One Sample t-test]
#                               독립표본 t-검정 - 2개             [Independent Sample t-test]
#                               대응표본 t-검정 - 반복(2)         [Paired Sample t-test]
#                 분산분석 - 3개
#                 (ANOVA - Analysis of Variance)       
#                               일원 분산분석     - 3개           [One Way ANOVA]
#                               반복측정 분산분석 - 반복(3)       [Repeated Measures ANOVA]
#                               이원 분산분석     - 요인 + 요인   [Two Way ANOVA] - 상호작용 확인
#                               이원 반복측정 분산- 반복 + 요인   [Two Way Repeated Measures ANOVA]
#         관계검정
#                 교차분석 - (명목)
#                 (Chi Square)
#                 상관분석 - (1:1)
#                 (Correlation)
#                 회귀분석 - (1:N)
#                 (Regression)  
#                               단순회귀분석      - 1:1종속(연속) [Linear Regression]
#                               다중회귀분석      - 1:N종속(연속) [Multiple Linear Regression]
#                               로지스틱 회귀분석 - 1:N종속(명목) [Logistic Regression]
################################################################################

#-------------------------------------------------------------------------------
# 통계분석 > 차이검정 > t-검정 > 일표본 t-검정   - 1개                  [One Sample t-test]
#-------------------------------------------------------------------------------
# 두 집단의 평균을 비교
# alternative :	"two.sided", "greater", "less"
test = t.test(data,
              alternative = c("two.sided"),
              mu=320,
              conf.level = 0.95)


#-------------------------------------------------------------------------------
# 통계분석 > 차이검정 > t-검정 > 독립표본 t-검정 - 2개                  [Independent Sample t-test]
#-------------------------------------------------------------------------------
# 등분산 인지 이분산 인지 확인 작업
# var.equal = TRUE 등분산 확인 후 설정
test = t.test(data$t_time ~ data$t_group,
              data = data,
              alternative = c("two.sided"),
              paired=TRUE,
              conf.level = 0.95)


#-------------------------------------------------------------------------------
# 통계분석 > 차이검정 > t-검정 > 대응표본 t-검정 - 반복(2)              [Paired Sample t-test]
#-------------------------------------------------------------------------------
# 두 집단의 차이(differences)로 비교
test = t.test(data$pre, data$post,
              alternative = c("two.sided"),
              paired=TRUE,
              conf.level = 0.95)


#-------------------------------------------------------------------------------
# 통계분석 > 차이검정 > 분산분석(ANOVA) > 일원 분산분석     - 3개       [One Way ANOVA]
#-------------------------------------------------------------------------------
# t test 3번 이상 비교하면 오류(1종)율이 증가(3 * 0.05 = 15%)
# 분산을 비교 -> 등분산 검정 -> 차이가 있다면 사후 검정
test = aov(score ~ group, 
           data = adata)


#-------------------------------------------------------------------------------
# 통계분석 > 차이검정 > 분산분석(ANOVA) > 반복측정 분산분석 - 반복(3)   [Repeated Measures ANOVA]
#-------------------------------------------------------------------------------
# 3집단 이상 사전, 사후 분산 비교
# 구형성(Test of Sphericity) 검정 - 각 반복측정치들 간의 상관관계가 모두 동일함을 의미
#   (만족)-> 일변량분석, Tests of Within-Subjects Effects
# (비만족)-> 다변량분석, 자유도 수정(그린하우스-가이서(Greenhouse-geisser), 훈펠트(Huynh-feldt) 수정계수)
model = aov(score ~ time + Error(id/time),
            data = train)


#-------------------------------------------------------------------------------
# 통계분석 > 차이검정 > 분산분석(ANOVA) > 이원 분산분석     - 요인 + 요인   [Two Way ANOVA] - 상호작용 확인
#-------------------------------------------------------------------------------
model = aov(taste ~ meth * temp, 
            data=train)

model = aov(taste ~ meth + temp + meth:temp, 
            data=train)


#-------------------------------------------------------------------------------
# 통계분석 > 차이검정 > 분산분석(ANOVA) > 이원 반복측정 분산- 반복 + 요인   [Two Way Repeated Measures ANOVA]
#-------------------------------------------------------------------------------
model = aov(painscore ~ time*group + Error(id), 
            data = train)

model = aov(painscore ~ time + group + time:group, 
            data = train)
















