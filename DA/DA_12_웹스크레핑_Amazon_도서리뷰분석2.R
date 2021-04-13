
load("amazon_review_cheese01.rda")
ls()

str(amazon_review) # 'data.frame':	3670 obs. of  8 variables:

View(amazon_review)

install.packages("tidyverse")
library(tidyverse)




# 평점(rating) 확인   ----------------------------------------------------------
summary(amazon_review$rating)

library(ggplot2)

# 색 참조
colors()

ggplot2::ggplot(amazon_review,
                aes(x = rating)) +
  geom_bar(color = "darkblue",
           fill = "cadetblue",
           width = 0.6)

# 그래프 그리기
ggplot2::ggplot(amazon_review,
                aes(x = rating)) +
  ggplot2::geom_bar(color = "darkblue",
                     fill = "cadetblue",
                     width = 0.6) +
  ggplot2::scale_x_continuous(breaks = c(1, 2, 3, 4, 5),
                              labels = c("1 star", "2 star", "3 star", "4 star", "5 star")) +
  ggplot2::labs(x = "Rating",
                y = "Frequency",
                title = "Review of 'who moved my cheese'",
                subtitle = "Rating distribution",
                caption = "Source: amazon") +
  ggplot2::theme(plot.title = element_text(face = "bold"),
                 axis.text = element_text(face = "bold"))




# 리뷰(review) 확인   ----------------------------------------------------------
# 단어수
summary(nchar(amazon_review$review))


# 그래프 그리기
ggplot2::ggplot(amazon_review,
                aes(x = nchar(review))) +
  ggplot2::geom_histogram(color = "brown",
                          fill = "coral") +
  ggplot2::labs(x = "Review Length (Number of Review Characters)",
                y = "Frequency",
                title = "Review of 'who moved my cheese'",
                subtitle = "Rating distribution",
                caption = "Source: amazon") +
  ggplot2::theme(plot.title = element_text(face = "bold"),
                 axis.text = element_text(face = "bold"))




# 평점(rating) - 리뷰(review) 관계   -------------------------------------------

# 3000 단어가 넘어 가는 리뷰 # 44
nrow(amazon_review[nchar(amazon_review$review) > 3000, ])
nrow(amazon_review[nchar(amazon_review$review) > 8000, ])


ggplot2::ggplot(amazon_review[nchar(amazon_review$review) < 3000, ],
                aes(x = as.factor(rating), y = nchar(review))) +
  ggplot2::geom_boxplot(color = "dimgray",
                          fill = "goldenrod") +
  ggplot2::scale_x_discrete(breaks = c("1", "2", "3", "4", "5"),
                              labels = c("1 star", "2 star", "3 star", "4 star", "5 star")) +
  ggplot2::labs(x = "Rating",
                y = "Review Length",
                title = "Review of 'who moved my cheese'",
                subtitle = "Distribution of review length by rating",
                caption = "Source: amazon") +
  ggplot2::theme(plot.title = element_text(face = "bold"),
                 axis.text = element_text(face = "bold"))




# 리뷰(review) 감성 분석    ----------------------------------------------------
# 2. 감성분석(sentiment analysis) or 오피니언마이닝(opinionmining)
#  - 감성어휘사전(sentiment lexicon dictionary)을 이용하여 감성상태(긍정/부정) 분류(빈도, 감성점수)
#    -> Bind(긍정,부정) 
#    -> AFINN(-5점 ~ 5점) 
#    -> NRC(10개의 감성상태) - positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, trust 
#    -> Loughran(6개의 감성상태) -  positive, negative, constrainging, litigious, superfluous, uncertainty

library(tidytext)
library(textdata)

afinn = tidytext::get_sentiments(lexicon = "afinn")
afinn # A tibble: 2,477 x 2

summary(afinn)

amazon_review_words = amazon_review %>% 
                          # 토큰화 # 269513 rows
                          tidytext::unnest_tokens(output = word,
                                                  input = review,
                                                  token = "words") %>% 
                          # 불용어 제거 # 94347 rows
                          dplyr::anti_join(stop_words, by = "word") %>% 
                          # 분석할 컬럼 조회 # 94139 rows
                          dplyr::select(c(id, rating, word)) %>% 
                          # 타이디-텍스트 # A tibble: 94,472 x 3
                          dplyr::as_tibble()

amazon_review_words # A tibble: 94,472 x 3

amazon_sent_review = amazon_review_words %>% 
                        # 감성 용어 사전 Join # A tibble: 11,580 x 4
                        dplyr::inner_join(tidytext::get_sentiments("afinn"),
                                          by = "word") %>% 
                        # 그룹
                        dplyr::group_by(id, rating) %>% 
                        # summary # A tibble: 2,789 x 3
                        dplyr::summarise(score_avg = mean(value, na.rm = TRUE)) %>% 
                        dplyr::ungroup()

amazon_sent_review # A tibble: 2,789 x 3

# 그래프 그리기
ggplot2::ggplot(amazon_sent_review,
                aes(x = as.factor(rating), y = score_avg)) +
  ggplot2::geom_boxplot(color = "black",
                        fill = "lavenderblush") +
  ggplot2::scale_x_discrete(breaks = c(1, 2, 3, 4, 5),
                            labels = c("1 star", "2 star", "3 star", "4 star", "5 star")) +
  ggplot2::labs(x = "Rating",
                y = "Average Sentiment Score of Words",
                title = "Review of 'who moved my cheese'",
                subtitle = "Distribution of average sentiment score",
                caption = "Source: amazon") +
  ggplot2::theme(plot.title = element_text(face = "bold"),
                 axis.text = element_text(face = "bold"))

#-------------------------------------------------------------------------------
# 리뷰 평점이 올라 갈수록 감성 점수도 올라 간다. (관련이 있다고 본다.)
#-------------------------------------------------------------------------------



# 리뷰(review) 감성 분석2    ---------------------------------------------------

amazon_review_words # A tibble: 94,472 x 3

# 데이터 전처리 # A tibble: 94,472 x 3
amazon_sent_word = amazon_review_words %>% 
  
                      # group by count # A tibble: 78,440 x 4
                      dplyr::count(id, rating, word) %>% 
                      
                      # group by init # A tibble: 78,440 x 4
                      dplyr::group_by(word) %>% 
                      
                      # 전체 word 별 count, sum, avg # A tibble: 13,049 x 4
                      dplyr::summarise(in_review = n(),
                                       in_use = sum(n),
                                       rating_avg = mean(rating)) %>% 
                      
                      # review 에서 3번 이상나온 단어 # A tibble: 3,817 x 4
                      dplyr::filter(in_review >= 3) %>% 
                      
                      # order by rating_avg # A tibble: 3,817 x 4
                      dplyr::arrange(rating_avg) %>% 
                      
                      # 감성사전 inner join # A tibble: 630 x 5
                      dplyr::inner_join(tidytext::get_sentiments("afinn"), by = "word")
  
amazon_sent_word # A tibble: 94,472 x 3 => # A tibble: 630 x 5
                   
View(amazon_sent_word)

# 그래프 그리기 (rating_avg - value)
ggplot2::ggplot(amazon_sent_word,
                aes(x = as.factor(value), y = rating_avg)) +
  ggplot2::geom_boxplot()


ggplot2::ggplot(amazon_sent_word,
                  aes(x = as.factor(value), y = rating_avg)) +
  ggplot2::geom_boxplot(color = "black",
                      fill = "aquamarine") +
  ggplot2::scale_y_continuous(breaks = c(1, 2, 3, 4, 5),
                              labels = c("1 star", "2 star", "3 star", "4 star", "5 star")) +
  ggplot2::labs(x = "Sentiments",
                y = "Average Rating of Reviews",
                title = "Review of 'who moved my cheese'",
                subtitle = "Distribution of average sentiment rating",
                caption = "Source: amazon") +
  ggplot2::theme(plot.title = element_text(face = "bold"),
                 axis.text = element_text(face = "bold"))

#-------------------------------------------------------------------------------
# 리뷰 평점이 좋을수록 긍정적인 단어들을 사용함(단어들이 평점 하고 관련이 있어 보인다)
#-------------------------------------------------------------------------------




# 머신 런닝 분석     -----------------------------------------------------------
# RTextTools::create_matrix()    - 문서 용어 행열
# RTextTools::create_container() - train, test 나누기
# RTextTools::train_models()     - model 생성(예측)
# RTextTools::classify_models()  - test 분류 작업

#install.packages("RTextTools")
library(RTextTools)

#-------------------------------------------------------------------------------
?RTextTools::create_matrix # {RTextTools} creates a document-term matrix to be passed into create_container().

# create_matrix(textColumns, language="english", minDocFreq=1, maxDocFreq=Inf, 
#               minWordLength=3, maxWordLength=Inf, ngramLength=1, originalMatrix=NULL, 
#               removeNumbers=FALSE, removePunctuation=TRUE, removeSparseTerms=0, 
#               removeStopwords=TRUE,  stemWords=FALSE, stripWhitespace=TRUE, toLower=TRUE, 
#               weighting=weightTf)
#-------------------------------------------------------------------------------

amazon_review

amazon_dtm = RTextTools::create_matrix(textColumns = amazon_review$review,
                                        language = "english",
                                        removeNumbers = TRUE,
                                        removePunctuation = TRUE,
                                        removeStopwords = FALSE,
                                        stemWords = FALSE,
                                        stripWhitespace = TRUE,
                                        toLower = TRUE,
                                        removeSparseTerms = 0.99)
amazon_dtm
# <<DocumentTermMatrix (documents: 3670, terms: 631)>>
# Non-/sparse entries: 104077/2211693
# Sparsity           : 96%
# Maximal term length: 13
# Weighting          : term frequency (tf)


# tibble 행렬 만들기

amazon_dtm = dplyr::as_tibble(as.matrix(amazon_dtm), 
                              row.names=amazon_review$id)
amazon_dtm # A tibble: 3,670 x 631
class(amazon_dtm) # "tbl_df"     "tbl"        "data.frame"



#-------------------------------------------------------------------------------
?RTextTools::create_container() # {RTextTools} creates a container for training, classifying, and analyzing documents.

create_container(matrix, labels, trainSize=NULL, testSize=NULL, virgin)
#-------------------------------------------------------------------------------

# train, test 나누기
train_end = round(nrow(amazon_dtm)*0.7, 0)
test_end = nrow(amazon_dtm)

my_container = RTextTools::create_container(matrix = amazon_dtm,
                                             labels = amazon_review$rating,
                                             trainSize = 1:train_end,
                                             testSize = (train_end+1):test_end,
                                             virgin = FALSE)
my_container


#-------------------------------------------------------------------------------
?RTextTools::train_models() # {RTextTools} makes a model object using the specified algorithms.

# train_models(container, algorithms, ...)
#-------------------------------------------------------------------------------

set.seed(123)
my_classifier = RTextTools::train_models(container = my_container,
                                         algorithms = c("RF", "SVM"))
my_classifier
class(my_classifier) # "list"

# 분석 가능 알고리즘
RTextTools::print_algorithms()
# "BAGGING"  "BOOSTING" "GLMNET"   "NNET"     "RF"       "SLDA"     "SVM"      "TREE"   



#-------------------------------------------------------------------------------
?RTextTools::train_models() # {RTextTools} makes a model object using the specified algorithms.

# train_models(container, algorithms, ...)
#-------------------------------------------------------------------------------

my_predction = RTextTools::classify_models(container = my_container,
                                            models = my_classifier)
my_predction

class(my_predction) # "data.frame"

head(my_predction)




#-------------------------------------------------------------------------------
# 결과 확인

# "RF"-randomForest 오분류 
table(amazon_review$rating[(train_end+1):test_end],
      my_predction$FORESTS_LABEL,
      dnn = c("Actual", "Predctied"))
#         Predctied
# Actual   1   2   4   5
#      1  41   0  11 227
#      2  12   1   1  62
#      3   6   0   3  84
#      4   1   0  14 179
#      5   8   0  15 436

# "RF"-randomForest 정확도 0.4468665
mean(amazon_review$rating[(train_end+1):test_end] == my_predction$FORESTS_LABEL)




# "SVM" 오분류 
table(amazon_review$rating[(train_end+1):test_end],
      my_predction$SVM_LABEL,
      dnn = c("Actual", "Predctied"))
#         Predctied
# Actual   1   2   3   4   5
#      1 105   4   5  12 153
#      2  22   0   7   6  41
#      3   9   1   8  10  65
#      4  11   5   4  15 159
#      5  20   1   4  11 423

# "SVM" 정확도 0.5004541
mean(amazon_review$rating[(train_end+1):test_end] == my_predction$SVM_LABEL)













