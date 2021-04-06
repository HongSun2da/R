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

# 2. 감성분석(sentiment analysis) or 오피니언마이닝(opinionmining)
#  - 감성어휘사전(sentiment lexicon dictionary)을 이용하여 감성상태(긍정/부정) 분류(빈도, 감성점수)
#    -> Bind(긍정,부정) 
#    -> AFINN(-5점 ~ 5점) 
#    -> NRC(10개의 감성상태) - positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, trust 
#    -> Loughran(6개의 감성상태) -  positive, negative, constrainging, litigious, superfluous, uncertainty

# 데이터 수집  =================================================================

library(tidytext)

#install.packages("textdata")
library(textdata)

str(sentiments) # tibble[,2] [6,786 x 2] (S3: tbl_df/tbl/data.frame)
class(sentiments) #  "tbl_df"     "tbl"        "data.frame"
View(sentiments) # negative positive
table(sentiments)

get_sentiments(lexicon = "bing")
table(get_sentiments(lexicon = "bing")$sentiment)
# negative positive 
# 4781     2005 

get_sentiments(lexicon = "afinn")
table(get_sentiments(lexicon = "afinn")$value)
# -5  -4  -3  -2  -1   0   1   2   3   4   5 
# 16  43 264 966 309   1 208 448 172  45   5 

get_sentiments(lexicon = "nrc")
table(get_sentiments(lexicon = "nrc")$sentiment)
# anger anticipation      disgust         fear          joy     negative     positive      sadness     surprise        trust 
#  1247          839         1058         1476          689         3324         2312         1191          534         1231 

get_sentiments(lexicon = "loughran")
table(get_sentiments(lexicon = "loughran")$sentiment)
# constraining    litigious     negative     positive  superfluous  uncertainty 
#          184          904         2355          354           56          297 



# 데이터 전처리  =================================================================

library(dplyr)
library(tibble)
library(purrr)
library(readr)
#install.packages("lubridate")
library(lubridate)


url = "https://archive.ics.uci.edu/ml/machine-learning-databases/00438/Health-News-Tweets.zip"

local_copy = tempfile() # 임시 파일
download.file(url, local_copy, mode="wb")  

Sys.setlocale("LC_TIME", "English")


files = unzip(zipfile = local_copy,
              files = c("Health-Tweets/bbchealth.txt",
                        "Health-Tweets/cnnhealth.txt",
                        "Health-Tweets/foxnewshealth.txt",
                        "Health-Tweets/NBChealth.txt"))

class(files) # "character"


health.twitter = purrr::map(files, 
                            readr::read_delim, 
                            delim="|",
                            quote="",
                            col_types=list(col_character(), col_character(), col_character()),
                            col_names=c("id", "datatime", "tweet")) %>% 
  purrr::map2(c("bbc", "cnn", "foxnews", "nbc"),
              ~cbind(.x, source=.y)) %>%
  purrr::reduce(dplyr::bind_rows) %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(datatime=lubridate::ymd_hms(strptime(datatime, "%a %b %d %H:%M:%S +0000 %Y")))

unlink(local_copy)
Sys.setlocale()


health.twitter # A tibble: 14,205 x 4

print(health.twitter, n=100)

# 데이터 분석  =================================================================
health.twitter %>%
  dplyr::count(source)

# 1 bbc      3929
# 2 cnn      4061
# 3 foxnews  2000
# 4 nbc      4215

library(stringr)

token = health.twitter %>%
  select(-id) %>%
  dplyr::mutate(tweet=str_replace_all(tweet, pattern="(f|ht)tp\\S+s*", replacement="")) %>%
  dplyr::mutate(tweet=str_replace_all(tweet, pattern="\\d", replacement="")) %>%
  dplyr::mutate(tweet=str_replace_all(tweet, pattern="\\bRT", replacement="")) %>%
  dplyr::mutate(tweet=str_replace_all(tweet, pattern="@\\S+", replacement="")) %>%
  dplyr::mutate(tweet=str_replace_all(tweet, pattern="&amp", replacement="")) %>%
  tidytext::unnest_tokens(word, tweet)

class(token) # "tbl_df"     "tbl"        "data.frame"

token # A tibble: 124,535 x 3


#  - 감성어휘사전(sentiment lexicon dictionary)을 이용하여 감성상태(긍정/부정) 분류(빈도, 감성점수)

?dplyr::inner_join # {dplyr} Mutating joins

# inner_join(): includes all rows in x and y.
# left_join(): includes all rows in x.
# right_join(): includes all rows in y.
# full_join(): includes all rows in x or y.

token %>%
  dplyr::inner_join(tidytext::get_sentiments("bing"), by="word") # A tibble: 12,510 x 4

token %>%
  dplyr::inner_join(tidytext::get_sentiments("bing"), by="word") %>%
  dplyr::count(word, sentiment, sort=TRUE) %>%
  dplyr::group_by(sentiment) %>%
  dplyr::top_n(10, n) %>%
  ungroup()

data_token = token %>%
  dplyr::inner_join(tidytext::get_sentiments("bing"), by="word") %>%
  dplyr::count(word, sentiment, sort=TRUE) %>%
  dplyr::group_by(sentiment) %>%
  dplyr::top_n(10, n) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(nsign=ifelse(sentiment=="negative", -n, n))

class(data_token) # "tbl_df"     "tbl"        "data.frame"
data_token # A tibble: 20 x 4


library(ggplot2)
library(scales)

ggplot2::ggplot(data_token,
                aes(x=reorder(word, nsign), y=nsign, fill=factor(sentiment, levels=c("positive", "negative")))) + 
  ggplot2::geom_col(color="lightslategray",
                    width=0.8) +
  ggplot2::geom_text(aes(label=n),
                     size = 3,
                     color = "black",
                     hjust = ifelse(data_token$nsign < 0, 1.3, -0.3)) +
  ggplot2::scale_fill_manual(values = c("cornflowerblue", "tomato")) +
  ggplot2::scale_y_continuous(breaks=pretty(data_token$nsign),
                              labels=abs(pretty(data_token$nsign))) +
  ggplot2::labs(x=NULL, y="Count") +
  ggplot2::theme(legend.position = "bottom",
                 legend.title = element_blank()) +
  ggplot2::coord_flip()
  

# patient  cancer   virus    문자 제외 하기
data_token = token %>%
  dplyr::inner_join(tidytext::get_sentiments("bing"), by="word") %>%
  dplyr::filter(!(word == "patient" | word == "cancer" | word == "virus")) %>%
  dplyr::count(word, sentiment, sort=TRUE) %>%
  dplyr::group_by(sentiment) %>%
  dplyr::top_n(10, n) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(nsign=ifelse(sentiment=="negative", -n, n))

class(data_token) # "tbl_df"     "tbl"        "data.frame"
data_token # A tibble: 20 x 4


ggplot2::ggplot(data_token,
                aes(x=reorder(word, n), y=n, fill=factor(sentiment, levels=c("positive", "negative")))) + 
  ggplot2::geom_col(color="lightslategray",
                    width=0.8,
                    show.legend = FALSE) +
  ggplot2::geom_text(aes(label=n),
                     size = 3,
                     color = "black",
                     hjust = 1.3) +
  ggplot2::scale_fill_manual(values = c("lightsteelblue", "lightsalmon")) +
  ggplot2::facet_wrap(~ factor(sentiment, levels=c("positive", "negative")),
                      ncol = 2,
                      scales = "free") +
  ggplot2::labs(x=NULL, y="Count") +
  ggplot2::coord_flip()




