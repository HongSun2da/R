## 텍스트 마이닝    ############################################################
# 1. 문서
# 2. 빈도분석
# 3. 감성분석
# 4. 분류분석
# 5. 토픽모델링


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
#     -> 
#     -> 
#     -> 
#     -> 
################################################################################

# 데이터 수집  =================================================================

text <- c("Crash dieting is not the best way to lose weight. http://bbc.in/1G0J4Agg",
          "A vegetarian diet excludes all animal flesh (meat, poultry, seafood).",
          "Economists surveyed by Refinitiv expect the economy added 160,000 jobs.")
sour = c("BBC","CNN", "FOX")

library(dplyr)

?dplyr::tibble

df = dplyr::tibble(source=sour,
                    text=text)
class(df) # "tbl_df"     "tbl"        "data.frame"

View(df)
str(df) # tibble[,2] [3 x 2] (S3: tbl_df/tbl/data.frame)


#install.packages("tidytext")

library(tidytext)

#-------------------------------------------------------------------------------
?tidytext::unnest_tokens #  {tidytext} Split a column into tokens

# unnest_tokens(
#   tbl,
#   output,
#   input,
#   token = "words",
#   format = c("text", "man", "latex", "html", "xml"),
#   to_lower = TRUE,
#   drop = TRUE,
#   collapse = NULL,
#   ...
# )
#-------------------------------------------------------------------------------


token = tidytext::unnest_tokens(tbl=df,
                        output=word,
                        input=text)
class(token) # "tbl_df"     "tbl"        "data.frame"

# %>% 연산자 사용
token = df %>%
  tidytext::unnest_tokens(output=word,
                          input=text)
token # A tibble: 33 x 2
print(token, n=Inf)

token %>%
  count(source) %>%
  arrange(desc(n))

#-------------------------------------------------------------------------------
?dplyr::anti_join # {dplyr} Filtering joins

# semi_join(x, y, by = NULL, copy = FALSE, ...)
# 
# ## S3 method for class 'data.frame'
# semi_join(x, y, by = NULL, copy = FALSE, ..., na_matches = c("na", "never"))
# 
# anti_join(x, y, by = NULL, copy = FALSE, ...)
# 
# ## S3 method for class 'data.frame'
# anti_join(x, y, by = NULL, copy = FALSE, ..., na_matches = c("na", "never"))
#-------------------------------------------------------------------------------

word_removed = dplyr::tibble(word=c("http","bbc.in","1g0j4agg"))
word_removed

token = token %>%
  dplyr::anti_join(word_removed, by="word")

print(token, n=Inf)

# 숫자 제거
token$word

token = token[-grep("\\d+", token$word),]

# 불용어 제거 작업
stop_words

token = token %>%
  dplyr::anti_join(stop_words, by="word")
print(token, n=Inf)

token$word = gsub("\\s+", "", token$word)
print(token, n=Inf)

# 어간 축출 
library(SnowballC)

token = token %>%
  dplyr::mutate(word=SnowballC::wordStem(token$word))
print(token, n=Inf)


token$word = gsub("economist", "economi", token$word)
print(token, n=Inf)




# tm 사용을 변환 하기
library(tm)

text <- c("Crash dieting is not the best way to lose weight. http://bbc.in/1G0J4Agg",
          "A vegetarian diet excludes all animal flesh (meat, poultry, seafood).",
          "Economists surveyed by Refinitiv expect the economy added 160,000 jobs.")
sour = c("BBC","CNN", "FOX")

docs = tm::VCorpus(tm::VectorSource(text))
class(docs) # "VCorpus" "Corpus" 

lapply(docs, content)

meta(docs, tag="author", type="local") = sour

meta(docs[[1]])

#-------------------------------------------------------------------------------
? tidy # {tidytext} Tidy a Corpus object from the tm package

# ## S3 method for class 'Corpus'
# tidy(x, collapse = "\n", ...)
#-------------------------------------------------------------------------------

token  = tidytext::tidy(docs) %>%
  tidytext::unnest_tokens(word,text) %>%
  select(soruce=author, word)

token  




