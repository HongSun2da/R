## 텍스트 마이닝    ############################################################
# 1. 빈도분석 (term frequency, tf)
# 2. 감성분석
# 3. 분류분석
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

# 데이터 수집  =================================================================

text <- c("Crash dieting is not the best way to lose weight. http://bbc.in/1G0J4Agg",
          "A vegetarian diet excludes all animal flesh (meat, poultry, seafood).",
          "Economists surveyed by Refinitiv expect the economy added 160,000 jobs.")


library(tm)

docs = tm::VCorpus(VectorSource(text))

docs = tm::tm_map(docs, content_transformer(tolower))
docs = tm::tm_map(docs, removeWords, tm::stopwords("english"))

myRemove = content_transformer(function(x, pattern)
{
  return(gsub(pattern, "", x)) 
})

docs = tm::tm_map(docs, myRemove, "(f|ht)tp\\S+\\s*")
docs = tm::tm_map(docs, removePunctuation)
docs = tm::tm_map(docs, removeNumbers)
docs = tm::tm_map(docs, stripWhitespace)
docs = tm::tm_map(docs, content_transformer(trimws))
docs = tm::tm_map(docs, stemDocument)
docs = tm::tm_map(docs, content_transformer(gsub),
                  pattern="economist", replacement="economi")
lapply(docs, content)

#-------------------------------------------------------------------------------
?tm::DocumentTermMatrix # {tm} Term-Document Matrix

# TermDocumentMatrix(x, control = list())
# DocumentTermMatrix(x, control = list())
# as.TermDocumentMatrix(x, ...)
# as.DocumentTermMatrix(x, ...)
#-------------------------------------------------------------------------------

docs_dtm = tm::DocumentTermMatrix(docs,
                       control = list(wordLengths=c(2, Inf)))

docs_dtm # <<DocumentTermMatrix (documents: 3, terms: 19)>>
class(docs_dtm) # "DocumentTermMatrix"    "simple_triplet_matrix"

tm::nTerms(docs_dtm)
tm::Terms(docs_dtm)

tm::nDocs(docs_dtm)
tm::Docs(docs_dtm)

rownames(docs_dtm)
rownames(docs_dtm) = sour = c("BBC","CNN", "FOX")

tm::inspect(docs_dtm)

# Terms
# Docs  ad anim best crash diet economi exclud expect flesh job
# BBC    0    0    1     1    1       0      0      0     0   0
# CNN    0    1    0     0    1       0      1      0     1   0
# FOX    1    0    0     0    0       2      0      1     0   1

tm::inspect(docs_dtm[1:2, 10:15])


# DocumentTermMatrix -> tidy
library(tidytext)
tidy(docs_dtm)




# tidy -> DocumentTermMatrix
text <- c("Crash dieting is not the best way to lose weight. http://bbc.in/1G0J4Agg",
          "A vegetarian diet excludes all animal flesh (meat, poultry, seafood).",
          "Economists surveyed by Refinitiv expect the economy added 160,000 jobs.")
sour = c("BBC","CNN", "FOX")

library(dplyr)
library(tidytext)
library(SnowballC)

df = dplyr::tibble(source=sour,
                   text=text)
class(df) # "tbl_df"     "tbl"        "data.frame

token = df %>%
  tidytext::unnest_tokens(output=word,
                          input=text)

token
class(token) # "tbl_df"     "tbl"        "data.frame"

token %>%
  count(source, word)

token_dtm = token %>%
  count(source, word) %>%
  tidytext::cast_dtm(document = source, term = word, value = n)

class(token_dtm) #"DocumentTermMatrix"    "simple_triplet_matrix"


tm::inspect(token_dtm)

# Terms
# Docs  1g0j4agg bbc.in best crash dieting http is lose not the
# BBC          1      1    1     1       1    1  1    1   1   1
# CNN          0      0    0     0       0    0  0    0   0   0
# FOX          0      0    0     0       0    0  0    0   0   1





