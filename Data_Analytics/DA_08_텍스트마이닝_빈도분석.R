## 텍스트 마이닝    ############################################################
# 1. 빈도분석 (term frequency, tf)
#  - tf가 큰 단어 (문서에서 중요하게 사용되는 단어는 대체로 반복적으로 등장)
#  - tf가 작은 단어 (일단적으로 중요한 단어가 아닐 가능성이 높음)
#  - 역문서빈도(inverse document frequency, idf)
#    -> 어떤 단어가 여러 문서에서 공통적으로 많이 포함되면 idf는 작아 진다.(반대는 idf는 커짐) 
#    -> idf가 커다면 해당 문서는 다름 문서와 구별되는 중요한 단어를 포함 하고 있다
#  - tf-idf(term frequency-inverse document frequency) -> 단위의 중요도 측도 방법

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

# 1. 빈도분석 (term frequency, tf)
#  - tf가 큰 단어 (문서에서 중요하게 사용되는 단어는 대체로 반복적으로 등장)
#  - tf가 작은 단어 (일단적으로 중요한 단어가 아닐 가능성이 높음)
#  - 역문서빈도(inverse document frequency, idf)
#    -> 어떤 단어가 여러 문서에서 공통적으로 많이 포함되면 idf는 작아 진다.(반대는 idf는 커짐) 
#    -> idf가 커다면 해당 문서는 다름 문서와 구별되는 중요한 단어를 포함 하고 있다
#  - tf-idf(term frequency-inverse document frequency) -> 단위의 중요도 측도 방법

# 데이터 수집  =================================================================

#install.packages("quanteda")
library(quanteda)

summary(data_corpus_inaugural) # 미국 역대 대통령 연설문

str(data_corpus_inaugural) # 'corpus' Named chr [1:58] 
class(data_corpus_inaugural) # "corpus"    "character"

View(data_corpus_inaugural)




# 데이터 전처리  ===============================================================

library(tidytext)
library(tibble)
library(dplyr)

docs = tidytext::tidy(data_corpus_inaugural) %>%
  dplyr::filter(Year > 1990) %>%
  dplyr::group_by(President, FirstName) %>%
  dplyr::summarise_all(list(~trimws(paste(., collapse = " ")))) %>%
  arrange(Year) %>%
  ungroup()

docs

class(docs) # "tbl_df"     "tbl"        "data.frame"
str(docs)

library(tm)

docs = docs %>%
  dplyr::select(text, everything()) %>%
  tibble::add_column(doc_id = 1:nrow(.), .before = 1)

docs_corpus = tm::VCorpus(tm::DataframeSource(docs))
docs_corpus

class(docs_corpus) # "VCorpus" "Corpus" 
View(docs_corpus)

# 내용 확인
lapply(docs_corpus[1], content)

# 소문자 변환
docs_corpus = tm::tm_map(docs_corpus, 
                         tm::content_transformer(tolower))

# 불용어 제거
mystopword = c(tm::stopwords("english"), c("can", "must", "will")) # character

docs_corpus = tm::tm_map(docs_corpus, 
                         tm::removeWords, mystopword)

# 문장부호 제거
docs_corpus = tm::tm_map(docs_corpus, 
                         tm::removePunctuation)

# 숫자 제거
docs_corpus = tm::tm_map(docs_corpus, 
                         tm::removeNumbers)

# 공백 제거
docs_corpus = tm::tm_map(docs_corpus, 
                         tm::stripWhitespace)

# 양쪽 여백 제거
docs_corpus = tm::tm_map(docs_corpus, 
                         tm::content_transformer(trimws))

lapply(docs_corpus[1], content)

# 추가 전처리
docs_corpus = tm::tm_map(docs_corpus, 
                         tm::content_transformer(gsub), 
                         pattern="america|americas|american|americans",
                         replacement="america")



# 문서-용어행렬(document-term matrix) 만들기   ---------------------------------
# 대통령 전체

docs_dtm = tm::DocumentTermMatrix(docs_corpus)

class(docs_dtm) # "DocumentTermMatrix"    "simple_triplet_matrix"

docs_dtm
# <<DocumentTermMatrix (documents: 4, terms: 2315)>>
#   Non-/sparse entries: 3561/5699
# Sparsity           : 62%
# Maximal term length: 16
# Weighting          : term frequency (tf)

tm::inspect(docs_dtm)

# 단어 확인
docs_dtm$dimnames


# 빈도 확인 하기
term_freq = colSums(as.matrix(docs_dtm))
str(term_freq)
class(term_freq) # numeric


term_freq[head(order(term_freq, decreasing = TRUE))]
term_freq[tail(order(term_freq, decreasing = TRUE))]

tm::findFreqTerms(docs_dtm, lowfreq = 40)
tm::findFreqTerms(docs_dtm, lowfreq = 50, highfreq = 100)


# 그래프 표현
library(ggplot2)

term_freq_df = data.frame(word=names(term_freq), frequency=term_freq)
head(term_freq_df)

# 기본 그래프
ggplot2::ggplot(subset(term_freq_df, frequency > 40),
                aes(x=word, y=frequency, fill=word)) +
  geom_col(color="dimgray") + 
  labs(x=NULL, y="Term Frequency (count)")

# 개선 그래프
ggplot2::ggplot(subset(term_freq_df, frequency > 40),
                aes(x=reorder(word, frequency), y=frequency, fill=word)) +
  ggplot2::geom_col(color="dimgray", 
                    width=0.6, 
                    show.legend = FALSE) + 
  ggplot2::geom_text(aes(label=frequency),
                     size=3.5,
                     color="black",
                     hjust=-0.3) +
  ggplot2::labs(x=NULL, 
                y="Term Frequency (count)") + 
  ggplot2::coord_flip()


#install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)

?RColorBrewer

head(term_freq)

wordcloud::wordcloud(words=names(term_freq), 
                     freq=term_freq,
                     scal=c(4, 0.5),
                     min.freq=5,
                     rot.per=0.1,
                     random.order=FALSE,
                     colors=RColorBrewer::brewer.pal(6, "Dark2"),
                     random.color = FALSE)


# 문서-용어행렬(document-term matrix) 만들기   ---------------------------------
# 대통령 별로

tm::inspect(docs_dtm)

class(docs_dtm) # "DocumentTermMatrix"    "simple_triplet_matrix"

rownames(docs_dtm) = c("Clinton","Bush", "Obama", "Trump")

tm::Docs(docs_dtm)

docs_token = tidytext::tidy(docs_dtm)
docs_token
class(docs_token) # "tbl_df"     "tbl"        "data.frame"

docs_token = docs_token %>%
  dplyr::mutate(document=factor(document, levels=c("Clinton","Bush", "Obama", "Trump"))) %>%
  dplyr::arrange(desc(count)) %>%
  dplyr::group_by(document) %>%
  dplyr::top_n(n=10, wt=count) %>%
  ungroup()

str(docs_token) # tibble[,3] [50 x 3] (S3: tbl_df/tbl/data.frame)
class(docs_token) # "tbl_df"     "tbl"        "data.frame"


# 기본 그래프
ggplot2::ggplot(docs_token,
                aes(x=term, y=count, fill=document)) +
  ggplot2::geom_col(show.legend=FALSE) +
  ggplot2::facet_wrap(~document, ncol=2, scales="free") +
  ggplot2::labs(x=NULL, y="Term frequency (count)") +
  ggplot2::coord_flip()

# 개선 그래프
ggplot2::ggplot(docs_token,
                aes(reorder_within(x=term, by=count, within=document),  y=count, fill=document)) +
  ggplot2::geom_col(show.legend=FALSE) +
  ggplot2::facet_wrap(~document, ncol=2, scales="free") +
  tidytext::scale_x_reordered() +
  ggplot2::labs(x=NULL, y="Term frequency (count)") +
  ggplot2::coord_flip()



# 1. 빈도분석 (term frequency, tf)      ----------------------------------------
# tf-idf(term frequency-inverse document frequency) - 대통령별 유니크한 단어 검색

docs_dtm2 = tm::DocumentTermMatrix(docs_corpus,
                                   control = list(weighting=tm::weightTfIdf))
docs_dtm2
# <<DocumentTermMatrix (documents: 4, terms: 2315)>>
#   Non-/sparse entries: 3137/6123
# Sparsity           : 66%
# Maximal term length: 16
# Weighting          : term frequency - inverse document frequency (normalized) (tf-idf)

class(docs_dtm2) # "DocumentTermMatrix"    "simple_triplet_matrix"
str(docs_dtm2)

rownames(docs_dtm2) = c("Clinton","Bush", "Obama", "Trump")

tm::inspect(docs_dtm2)

# 소스변경 처리함

docs_tfidf = tidytext::tidy(docs_dtm2) %>%
  mutate(tf_idf=count, count=NULL)

docs_tfidf = docs_tfidf %>%
  dplyr::mutate(document=factor(document, levels=c("Clinton","Bush", "Obama", "Trump"))) %>%
  dplyr::arrange(desc(tf_idf)) %>%
  dplyr::group_by(document) %>%
  dplyr::top_n(n=10, wt=tf_idf) %>%
  ungroup()

docs_tfidf

ggplot2::ggplot(docs_tfidf,
                aes(reorder_within(x=term, by=tf_idf, within=document),  y=tf_idf, fill=document)) +
  ggplot2::geom_col(show.legend=FALSE) +
  ggplot2::facet_wrap(~document, 
                      ncol=2, 
                      scales="free") +
  tidytext::scale_x_reordered() +
  ggplot2::labs(x=NULL, 
                y="Term frequency - Inverse Document Frequence") +
  ggplot2::geom_text(aes(label=round(tf_idf, 5)),
                     size=3.5,
                     color="black",
                     hjust=1.3) +
  ggplot2::coord_flip()





# 1. 빈도분석 (term frequency, tf)      ----------------------------------------
# tf(term frequency) - 


docs_address = docs %>%
  tidytext::unnest_tokens(word, text)

docs_address =docs_address %>%
  dplyr::anti_join(stop_words, by="word") %>%
  dplyr::filter(!grepl(pattern="\\d+", word)) %>%
  dplyr::mutate(word=gsub(pattern="'", replacement="", word)) %>%
  dplyr::mutate(word=gsub(pattern="america|americas|american|americans", replacement="america", word)) %>%
  count(President, word, sort=TRUE, name="count") %>%
  ungroup()

docs_address %>%
  dplyr::group_by(word) %>%
  dplyr::summarise(count=sum(count)) %>%
  dplyr::arrange(desc(count)) %>%
  dplyr::top_n(n=10, wt=count) %>%
  ggplot2::ggplot(aes(reorder(word, -count), count)) +
  geom_col(color="dimgray", fill="salmon", width=0.6) +
  theme(axis.text.x = element_text(angle=45, hjust=1))
  geom_text(aes(label=count), size=3.5, color="black") +
  labs(x=NULL, y="Term frequency (count)")




# 1. 빈도분석 (term frequency, tf)      ----------------------------------------
# tf-idf(inverse document frequency) -> 단위의 중요도 측도 방법

docs_address = docs_address %>%
    bind





