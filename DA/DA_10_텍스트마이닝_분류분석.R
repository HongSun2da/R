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

# 4. 토픽모델링

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

# - 3. 문서-용어행렬(document-term matrix) - 단어부머니(bag-of-words)
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

# 3. 분류분석
#  - 문서분류 (카테고리 분류 예측)

# 데이터 수집  =================================================================

# SMS Spam Collection Data Set
# https://archive.ics.uci.edu/ml/machine-learning-databases/00228/smsspamcollection.zip

url = "https://archive.ics.uci.edu/ml/machine-learning-databases/00228/smsspamcollection.zip"

local_copy = tempfile()

download.file(url,
              local_copy,
              mode = "wb")

#-------------------------------------------------------------------------------
?download.file # {utils} Download File from the Internet

# download.file(url, destfile, method, quiet = FALSE, mode = "w",
#               cacheOK = TRUE,
#               extra = getOption("download.file.extra"),
#               headers = NULL, ...)
#-------------------------------------------------------------------------------

library(readr)

sms = readr::read_delim(unzip(zipfile = local_copy, files = "SMSSpamCollection"),
                        delim = "\t",
                        quote = "",
                        col_types = cols("f", "c"),
                        col_names = c("type", "text"))
unlink(local_copy)

#-------------------------------------------------------------------------------
?readr::read_delim # {readr} Read a delimited file (including csv & tsv) into a tibble

# read_delim(
#   file,
#   delim,
#   quote = "\"",
#   escape_backslash = FALSE,
#   escape_double = TRUE,
#   col_names = TRUE,
#   col_types = NULL,
#   locale = default_locale(),
#   na = c("", "NA"),
#   quoted_na = TRUE,
#   comment = "",
#   trim_ws = FALSE,
#   skip = 0,
#   n_max = Inf,
#   guess_max = min(1000, n_max),
#   progress = show_progress(),
#   skip_empty_rows = TRUE
# )
#-------------------------------------------------------------------------------

sms # A tibble: 5,574 x 2
class(sms) # "spec_tbl_df" "tbl_df"      "tbl"         "data.frame" 

table(sms$type)
#  ham spam 
# 4827  747 

prop.table(table(sms$type))
#       ham      spam 
# 0.8659849 0.1340151 



# 데이터 전처리  ===============================================================

library(dplyr)
library(tibble)

# doc_id text etc... 컬럼 만들기 
sms = sms %>%
  dplyr::select(text, type) %>%
  tibble::add_column(doc_id = 1:nrow(.),
                     .before = 1) %>%
  # text -> ascii 변환
  dplyr::mutate(text=iconv(text, to = "ascii", sub = ""))

sms  # A tibble: 5,574 x 3
class(sms) # "tbl_df"     "tbl"        "data.frame"

library(tm)

docs = tm::VCorpus(tm::DataframeSource(sms))

docs
# Metadata:  corpus specific: 0, document level (indexed): 1
# Content:  documents: 5574

class(docs) # "VCorpus" "Corpus" 

lapply(docs, content)[c(13, 16, 20)]

meta(docs)$type[c(13, 16, 20)]

meta(docs[[1]])


# 소문자 변화
docs = tm::tm_map(docs, tm::content_transformer(tolower))

# 문자 제거 함수
StrRemove = tm::content_transformer(function(x, pattern)
  {
  return(gsub(pattern, "", x))
})

# [http://wap.] 제거
docs = tm::tm_map(docs, StrRemove, "(f|ht)tp\\S+\\s*")

# [www.dbuk.net] 제거
docs = tm::tm_map(docs, StrRemove, "www\\.+\\S+")

# 불용어 제정의
Strstopwords = c(tm::stopwords("english"),
                 c("can","cant","don","dont","get","got","just","one","will"))

# [불용어] 제거
docs = tm::tm_map(docs, tm::removeWords, Strstopwords)


# 빈공간 문자 만들기
ToSpace = tm::content_transformer(function(x, pattern)
{
  return(gsub(pattern, " ", x))
})

# [: ; / 문자열] 제거 = try:wales > try wales
docs = tm::tm_map(docs, StrRemove, ":")
docs = tm::tm_map(docs, StrRemove, ";")
docs = tm::tm_map(docs, StrRemove, "/")

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


lapply(docs, content)[c(13, 16, 20)]



# 데이터 분석(전체)  ===========================================================

# - 3. 문서-용어행렬(document-term matrix) - 단어부머니(bag-of-words)

docs_dtm = tm::DocumentTermMatrix(docs)

docs_dtm
# <<DocumentTermMatrix (documents: 5574, terms: 6478)>>
#   Non-/sparse entries: 40474/36067898
# Sparsity           : 100%
# Maximal term length: 38
# Weighting          : term frequency (tf)

class(docs_dtm) # "DocumentTermMatrix"    "simple_triplet_matrix"

tm::inspect(docs_dtm)


# # 단어 줄이기 [ !작동 안됨 ]
# docs_dtm = tm::DocumentTermMatrix(docs,
#                                   control = list(wordLengths = c(4, 1), bounds = list(global = c(5, 5300))))
# 
# 
#                                   
# docs_dtm
# # <<DocumentTermMatrix (documents: 5574, terms: 0)>>
# #   Non-/sparse entries: 0/0
# # Sparsity           : 100%
# # Maximal term length: 0
# # Weighting          : term frequency (tf)
# 
# class(docs_dtm) # "DocumentTermMatrix"    "simple_triplet_matrix"
# 
# tm::inspect(docs_dtm)



docs_tf = colSums(as.matrix(docs_dtm))

docs_tf
class(docs_tf) # "numeric"
head(docs_tf)

# top down count 확인
docs_tf[head(order(docs_tf, decreasing = TRUE))]
docs_tf[tail(order(docs_tf, decreasing = TRUE))]

# 200 개 이상 문자
tm::findFreqTerms(docs_dtm, lowfreq=200)

# 상관관계 가 있는 단어 찾기 ★★★★
tm::findAssocs(docs_dtm, c("call", "free"), c(0.20, 0.25))


# 그래프 그리기   --------------------------------------------------------------

library(wordcloud)
library(RColorBrewer)

set.seed(123)
wordcloud::wordcloud(words = names(docs_tf),
                     freq = docs_tf,
                     scale = c(4, 0.5),
                     min.freq = 30,
                     max.words = 200,
                     rot.per = 0,
                     random.order = FALSE,
                     random.color = FALSE,
                     colors = brewer.pal(6, "Set2"))


# 그래프 그리기(spam 구분)   ---------------------------------------------------

as.matrix(docs_dtm)[1:5, 1:5]

docs_span = as.matrix(docs_dtm)

rownames(docs_span) = sms$type

docs_span[1:5, 1:5]

docs_span = rowsum(docs_span, group = rownames(docs_span))

docs_span[, 1:5]

t(docs_span[, 1:5])

set.seed(123)
wordcloud::comparison.cloud(t(docs_span),
                            colors = c("cornflowerblue","tomato"),
                            title.size = 2,
                            title.colors = c("blue","red"),
                            title.bg.colors = "wheat",
                            rot.per = 0,
                            scale = c(5, 0.4),
                            max.words = 200)




# 데이터 분석(예측) 나이브 베이즈  =============================================

tm::inspect(docs_dtm) # <<DocumentTermMatrix (documents: 5574, terms: 6478)>>
sms$type # [ reached getOption("max.print") -- omitted 5574 entries ]


set.seed(123)
data_idx = sample(nrow(sms), nrow(sms)*0.7)

data_train = sms[data_idx,]$type
data_test = sms[-data_idx,]$type

table(data_train)
#  ham spam 
# 3376  525 
prop.table(table(data_train))
#       ham      spam 
# 0.8654191 0.1345809 

table(data_test)
#  ham spam 
# 1451  222 
prop.table(table(data_test))
#       ham      spam 
# 0.8673042 0.1326958 

str(data_train)


toFactor = function(x){
  x = ifelse(x > 0, 1, 0)
  x = factor(x, 
             levels = c(0, 1), 
             labels = c("no", "yes"))
  return(x)
}


sms_dtm = apply(docs_dtm, 
                MARGIN = 2,
                toFactor)

class(sms_dtm) # "matrix" "array" 
                
sms_dtm[1:7, 1:10]


x_train = sms_dtm[data_idx,]
x_test = sms_dtm[-data_idx,]

x_train[1:7, 1:10]
x_test[1:7, 1:10]

# 실행 오류

# library(e1071)
# 
# model = e1071::naiveBayes(x=x_train, y=data_train)
# 
# model_pred = predict(model, 
#                      newdata = x_test)
# 
# head(model_pred)





