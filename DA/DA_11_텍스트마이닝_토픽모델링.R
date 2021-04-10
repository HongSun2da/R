## 텍스트 마이닝    ############################################################
# 1. 빈도분석 (term frequency, tf)
#  - tf가 큰 단어 (문서에서 중요하게 사용되는 단어는 대체로 반복적으로 등장)
#  - tf가 작은 단어 (일단적으로 중요한 단어가 아닐 가능성이 높음)
#  - 역문서빈도(inverse document frequency, idf)
#    -> 어떤 단어가 여러 문서에서 공통적으로 많이 포함되면 idf는 작아 진다.(반대는 idf는 커짐) 
#    -> idf가 커다면 해당 문서는 다름 문서와 구별되는 중요한 단어를 포함 하고 있다
#  - tf-idf(term frequency-inverse document frequency) -> 단위의 중요도 측도 방법

# 2. 감성분석(sentiment analysis) or 오피니언마이닝(opinionmining)
#  - 감성어휘사전(sentiment lexicon dictionary)을 이용하여 감성상태(긍정/부정) 분류(빈도, 감성점수)
#    -> Bind(긍정,부정) 
#    -> AFINN(-5점 ~ 5점) 
#    -> NRC(10개의 감성상태) - positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, trust 
#    -> Loughran(6개의 감성상태) -  positive, negative, constrainging, litigious, superfluous, uncertainty

# 3. 분류분석
#  - 문서분류 (카테고리 분류 예측)

# 4. 토픽모델링(topic modeling)
#  - 대량의 문서 집합 속에서 토픽(분류)을 찾는 분석
#  - 문서 토픽은 혼합체(mixture of topics)로 비율을 계산
#    -> {태통령, 정부, 국회, 예산}   -> Topic1 : 0.56% (정치)
#    -> {환율, 이자율, 부동산, 예산} -> Topic2 : 0.25% (경제)
#  - LDA(Latent irichlet Allocation)는 토픽모델에 의한 문서생성의 역순으로 문서에 내재되어 있는 토픽을 발굴
#    -> 토픽의 개수 K 결정(분석자)
#    -> 문서 D에 포함된 각 단어 W를 무작위로 K개 토픽 가운데 하나에 할당
#    -> 문서 D에 포함된 각 단어 W를 대해 P(토픽T|문서D)XP(단어W|토픽T)의 확률로 소속 토픽T 업데이트 {반복수행}
#    -> 종료:안정화된 최종 토픽 할당

################################################################################
# - 1. 코퍼스(corpus) - 문서의 집합 ["VCorpus" "Corpus"]
#   - metadata [작성자, 작서일시, 제목, 언어]
#   - content  [내용, 문장]    
#     -> 소문자(영문) 처리
#     -> 불용어 제거 [stopwords] - 기타 제거문자(숫자, 빈문자, http, ftp, 특수문자..)
#     -> 어간 축출 작업

# - 2. 타이디-텍스트(tidy-text) - 토큰화(tokenization)
#     -> dplyr::tibble (형변환)
#     -> 데이터 정리 작업
#     -> tidytext::unnest_tokens (토큰화)
#     -> 불용어 제거 [stop_words] - 기타 제거문자(숫자, 빈문자, http, ftp, 특수문자..)
#     -> 어간 축출 작업
#     or
#     -> tm::VCorpus (corpus 형 변환)
#     -> tidytext::tidy (tidy 형 변환)
#     -> tidytext::unnest_tokens (토큰화)

# - 3. 문서-용어행렬(document-term matrix) - 단어주머니(bag-of-words)
#     -> tm::VCorpus (corpus 형 변환) 
#     -> 소문자(영문) 처리
#     -> 불용어 제거 [stopwords] - 기타 제거문자(숫자, 빈문자, http, ftp, 특수문자..)
#     -> 어간 축출 작업
#     -> tm::DocumentTermMatrix 만들기
#     or
#     -> dplyr::tibble (형 변화)
#     -> tidytext::unnest_tokens (토큰화)
#     -> tidytext::cast_dtm (용어행렬 만들기)
################################################################################

# 4. 토픽모델링(topic modeling)
#  - 대량의 문서 집합 속에서 토픽(분류)을 찾는 분석
#  - 문서 토픽은 혼합체(mixture of topics)로 비율을 계산
#    -> {태통령, 정부, 국회, 예산}   -> Topic1 : 0.56% (정치)
#    -> {환율, 이자율, 부동산, 예산} -> Topic2 : 0.25% (경제)
#  - LDA(Latent irichlet Allocation)는 토픽모델에 의한 문서생성의 역순으로 문서에 내재되어 있는 토픽을 발굴
#    -> 토픽의 개수 K 결정(분석자)
#    -> 문서 D에 포함된 각 단어 W를 무작위로 K개 토픽 가운데 하나에 할당
#    -> 문서 D에 포함된 각 단어 W를 대해 P(토픽T|문서D)XP(단어W|토픽T)의 확률로 소속 토픽T 업데이트 {반복수행}
#    -> 종료:안정화된 최종 토픽 할당


# 사용자 정의 함수   ===========================================================
# 문자 제거 함수
StrRemove = tm::content_transformer(function(x, pattern)
{
  return(gsub(pattern, "", x))
})

# 빈공간 문자 만들기
ToSpace = tm::content_transformer(function(x, pattern)
{
  return(gsub(pattern, " ", x))
})


# 불용어 제정의
Strstopwords = c(tm::stopwords("english"),
               c("first", "second", "one", "two", "three", "four", "another", 
                 "last", "least", "just", "will", "week", "weeks","quot", 
                 "ago", "day", "days", "night", "nights", "month","months", 
                 "years", "year", "next", "now", "today", "yesterday", 
                 "may", "new", "york", "according", "back", "say","says", 
                 "said", "can", "make","made", "reuters", "monday", "tuesday", 
                 "wednesday", "thursday", "friday", "saturday", "sunday"))




# 데이터 수집  =================================================================

library(textdata)

# 전체
news = textdata::dataset_ag_news()  # A tibble: 120,000 x 3

# Test 용
news = textdata::dataset_ag_news(split="test") # A tibble: 7,600 x 3

str(news)   # spec_tbl_df[,3] [7,600 x 3] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
class(news) # "spec_tbl_df" "tbl_df"      "tbl"         "data.frame" 

table(news$class)
# Business Sci/Tech   Sports    World 
#     1900     1900     1900     1900 


# 데이터 전처리  =================================================================

# news -> document-term matrix 변환 하기
library(tm)

docs = tm::VCorpus(VectorSource(news$description))

docs
# <<VCorpus>>
# Metadata:  corpus specific: 0, document level (indexed): 0
# Content:  documents: 7600

class(docs) # "VCorpus" "Corpus" 

# 내용 확인
lapply(docs, NLP::content)[1:5]



# 문자열 처리 하기    ----------------------------------------------------------

# 소문자 변화
docs = tm::tm_map(docs, tm::content_transformer(tolower))

# [http://wap.] 제거
docs = tm::tm_map(docs, StrRemove, "(f|ht)tp\\S+\\s*")

# [www.dbuk.net] 제거
docs = tm::tm_map(docs, StrRemove, "www\\.+\\S+")

# [불용어] 제거
docs = tm::tm_map(docs, tm::removeWords, Strstopwords)

# [: ; / 문자열] 제거 = try:wales > try wales
docs = tm::tm_map(docs, ToSpace, ":")
docs = tm::tm_map(docs, ToSpace, ";")
docs = tm::tm_map(docs, ToSpace, "/")
docs = tm::tm_map(docs, ToSpace, "\\.")
docs = tm::tm_map(docs, ToSpace, "\\\\")


#[! & , > . ? -] 부호 제거
docs = tm::tm_map(docs, tm::removePunctuation)

#[1-0] 숫자 제거
docs = tm::tm_map(docs, tm::removeNumbers)

#[ ] 문자 연결 공백 제거
docs = tm::tm_map(docs, tm::stripWhitespace)

#[ ] 문장 앞뒷 공백 제거
docs = tm::tm_map(docs, tm::content_transformer(trimws))

#[ ] 어간 제거 = trywales > trywal , national > nation 
docs = tm::tm_map(docs, tm::stemDocument)

lapply(docs, NLP::content)[1:5]




# document-term matrix 만들기  -------------------------------------------------
docs_dtm = tm::DocumentTermMatrix(docs)

docs_dtm
# <<DocumentTermMatrix (documents: 7600, terms: 14521)>>
#   Non-/sparse entries: 125168/110234432
# Sparsity           : 100%
# Maximal term length: 29
# Weighting          : term frequency (tf)

class(docs_dtm) # "DocumentTermMatrix"    "simple_triplet_matrix"

tm::inspect(docs_dtm)

# rowname 재설정
rownames(docs_dtm) = paste0(rownames(docs_dtm), "-", news$class)




# LDA model 만들기  ------------------------------------------------------------

#install.packages("topicmodels")
library(topicmodels)

#-------------------------------------------------------------------------------
?topicmodels::LDA # {topicmodels} Latent Dirichlet Allocation

# LDA(x, k, method = "VEM", control = NULL, model = NULL, ...) 
# 
# method	
# The method to be used for fitting; currently method = "VEM" or method= "Gibbs" are supported.
#-------------------------------------------------------------------------------

model = topicmodels::LDA(docs_dtm,
                         k = 4,
                         method = "Gibbs",
                         control = list(seed = 123, burnin = 1000, iter = 1000, thin = 100))

class(model)
# [1] "LDA_Gibbs"
# attr(,"package")
# [1] "topicmodels"

summary(model)
# Length     Class      Mode 
#      1 LDA_Gibbs        S4 

topicmodels::topics(model)[1:10]

table(topicmodels::topics(model))
#    1    2    3    4 
# 1932 2029 1775 1864 

# topic별 포함된 단어 확인
topicmodels::terms(model, 10)




# 속성값 확인(beta, gamma) ★★★★★----------------

str(model@beta) # num [1:4, 1:14521] -12.7 -10.3 -12.7 -12.7 -8.8 ...

# Topic(1:4)별 단어(1:14521)와 관련값
model@beta[, 1:10]

# Topic(1:4)별 단어(1:14521)와 확율값
exp(model@beta[, 1:10])



str(model@gamma) # num [1:7600, 1:4] 0.218 0.285 0.25 0.23 0.231 ...

# 각문서(1:7600)별 Topic(1:4)과 확율값
model@gamma[1:10, ]




# beta ######
# 타이디-텍스트(tidy-text) 만들기  ---------------------------------------------
library(tidytext)

# Topic(1:4)별 단어(1:14521)와 확율값 == exp(model@beta[, 1:10])
docs_tidy = tidytext::tidy(model,
                           matrix = "beta")

docs_tidy

class(docs_tidy) # "tbl_df"     "tbl"        "data.frame"
str(docs_tidy)   # tibble[,3] [58,084 x 3] (S3: tbl_df/tbl/data.frame)

# topic 별 상위 10
library(dplyr)

docs_tidy_top = docs_tidy %>%
                  dplyr::group_by(topic) %>%
                  dplyr::top_n(5, beta) %>%
                  dplyr::ungroup() %>%
                  dplyr::arrange(topic, -beta)

docs_tidy_top # A tibble: 20 x 3

class(docs_tidy_top) #  "tbl_df"     "tbl"        "data.frame"
str(docs_tidy_top)   # tibble[,3] [20 x 3] (S3: tbl_df/tbl/data.frame)




# 그래프  만들기  --------------------------------------------------------------
library(ggplot2)

ggplot2::ggplot(docs_tidy_top,
                aes(x = reorder(term, beta), y = beta, fill = factor(topic))) + 
  ggplot2::geom_col(show.legend = FALSE) + 
  ggplot2::facet_wrap(~ paste("Topic", topic),
                      scales = "free") +
  ggplot2::labs(x = NULL, 
                y = "Word-Topic Probability (Beta)") +
  ggplot2::coord_flip()
  





# gamma ######
# 타이디-텍스트(tidy-text) 만들기  ---------------------------------------------
library(tidytext)

# 각문서(1:7600)별 Topic(1:4)과 확율값
docs_tidy = tidytext::tidy(model,
                           matrix = "gamma")

docs_tidy

class(docs_tidy) # "tbl_df"     "tbl"        "data.frame"
str(docs_tidy)   # tibble[,3] [58,084 x 3] (S3: tbl_df/tbl/data.frame)




# 그래프  만들기  --------------------------------------------------------------
library(tidyr)

docs_tidy_topic = docs_tidy %>%
                    tidyr::separate(document,
                                    c("id", "category"),
                                    sep="-")
docs_tidy_topic

class(docs_tidy_topic) # "tbl_df"     "tbl"        "data.frame"
str(docs_tidy_topic)   # tibble[,4] [30,400 x 4] (S3: tbl_df/tbl/data.frame)



library(ggplot2)
ggplot2::ggplot(docs_tidy_topic,
                aes(x = factor(topic), y = gamma, fill = category)) + 
  ggplot2::geom_boxplot(color = "gray50",
                        show.legend = FALSE) +
  ggplot2::facet_wrap(~ category,
                      scales = "free") +
  ggplot2::labs(x = "Topic", 
                y = "Document-Topic Probability (Gamma)") 

