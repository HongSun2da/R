load("tripadv_review01.rda")
ls()

str(tripadv_review) # 'data.frame':	1823 obs. of  8 variables:

View(tripadv_review)



# 데이터 전처리   ==============================================================
# 2. 감성분석(sentiment analysis) or 오피니언마이닝(opinionmining)
#  - 감성어휘사전(sentiment lexicon dictionary)을 이용하여 감성상태(긍정/부정) 분류(빈도, 감성점수)
#    -> Bind(긍정,부정) 
#    -> AFINN(-5점 ~ 5점) 
#    -> NRC(10개의 감성상태) - positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, trust 
#    -> Loughran(6개의 감성상태) -  positive, negative, constrainging, litigious, superfluous, uncertainty


#install.packages("tidyverse")
library(tidyverse)
library(tidytext)

tripadv_review_word = tripadv_review %>% 
                        # 토큰화 # 69739 rows
                        tidytext::unnest_tokens(output = word,
                                                input = review,
                                                token = "words") %>% 
                        
                        # 불용어 제거 # 23983 rows 
                        dplyr::anti_join(stop_words, by = "word") %>% 
                        
                        # 분석할 컬럼 조회 # 23858 rows 
                        dplyr::select(c(id, date, rating, word)) %>% 
                        
                        # 타이디-텍스트 # A tibble: 24,108 x 4
                        dplyr::as_tibble()

tripadv_review_word # A tibble: 24,108 x 4
  

tripadv_sent_review = tripadv_review_word %>% 
  
                        # 감성 용어 사전 Join # A tibble: 1,968 x 5
                        dplyr::inner_join(tidytext::get_sentiments("bing"),
                                          by = "word") %>% 
                        
                        # group by count# A tibble: 1,148 x 5
                        dplyr::count(id, date, rating, sentiment) %>% 
                        
                        # sentiment 구분열 만들기 # A tibble: 984 x 5
                        tidyr::spread(sentiment, n, fill = 0) %>% 
                        
                        # 긍정-부정 = 리뷰감정가 # A tibble: 984 x 6
                        dplyr::mutate(sentiment=positive - negative) %>% 
                        
                        # group by 해지 # A tibble: 984 x 6
                        dplyr::ungroup()

tripadv_sent_review # A tibble: 984 x 6

View(tripadv_sent_review)


# 데이터 분석   ================================================================

summary(tripadv_sent_review$sentiment)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -2.0000 -1.0000  0.0000  0.3333  2.0000  3.0000 


library(ggplot2)

ggplot2::ggplot(tripadv_sent_review,
                aes(x = as.factor(rating), y = sentiment)) +
  ggplot2::geom_boxplot()


# 그래프 그리기
ggplot2::ggplot(tripadv_sent_review,
                aes(x = as.factor(rating), y = sentiment)) +
  ggplot2::geom_boxplot(color = "black",
                        fill = "darkcyan") +
  ggplot2::scale_x_discrete(breaks = c("1", "2", "3", "4", "5"),
                              labels = c("1 star", "2 star", "3 star", "4 star", "5 star")) +
  ggplot2::scale_y_continuous(breaks = seq(-6, 6, by=2)) +
  ggplot2::labs(x = "Rating",
                y = "Sentiment Score (positive - negative)",
                title = "Review of Planet Hollywood",
                subtitle = "Distribution of sentiment score by review rating",
                caption = "Source: Tripadvisor") +
  ggplot2::theme(plot.title = element_text(face = "bold"),
                 axis.text = element_text(face = "bold")) +
  ggplot2::coord_flip()


#-------------------------------------------------------------------------------
# 리뷰 평점이 좋을수록 긍정적인 단어를 더 많이 사용함(단어 - 평점 관련이 있어 보임)
#-------------------------------------------------------------------------------






# 데이터 전처리   ==============================================================

tripadv_review_word # A tibble: 26,185 x 4

tripadv_sent_word = tripadv_review_word %>% 
  
                      # 감성 용어 사전 Join # A tibble: 4,226 x 5
                      dplyr::inner_join(tidytext::get_sentiments("bing"),
                                        by = "word") %>% 
                      
                      # group by count # A tibble: 637 x 3
                      dplyr::count(sentiment, word) %>% 
                      
                      # group by 해지 # A tibble: 637 x 3
                      dplyr::ungroup() %>% 
                      
                      # 단어 수가 50개 이상 단어 # A tibble: 21 x 3
                      dplyr::filter(n >= 50) %>% 
                    
                      # 부정 부분을 음수 처리 # A tibble: 21 x 4
                      dplyr::mutate(nsign=ifelse(sentiment == "negative", -n, n))

tripadv_sent_word # A tibble: 21 x 4
print(tripadv_sent_word, n = Inf)
  

# 데이터 분석   ================================================================

ggplot2::ggplot(tripadv_sent_word,
                aes(x = word ,  y = nsign, fill = sentiment )) +
  ggplot2::geom_bar(stat = "identity")


library(scales)

# 그래프 그리기
ggplot2::ggplot(tripadv_sent_word,
                aes(x = reorder(word, nsign) , 
                    y = nsign, 
                    fill = factor(sentiment, levels = c("positive", "negative")) )) +
  ggplot2::geom_bar(stat = "identity",
                    color = "lightslategray",
                    width = 0.8) +
  ggplot2::geom_text(aes(label = n), 
                     size = 3,
                     color = "black",
                     hjust = ifelse(tripadv_sent_word$nsign < 0 , 1.1, -0.1)) +
  ggplot2::theme(legend.position = "bottom",
                 legend.title = element_blank(),
                 plot.title = element_text(face = "bold"),
                 axis.text = element_text(face = "bold", size = 10)) +
  ggplot2::labs(x = NULL,
                y = "Count",
                title = "Review of Planet Hollywood",
                subtitle = "Top words contributing to sentiment",
                caption = "Source : Tripadvisor") +
  ggplot2::scale_fill_manual(values = c("cornflowerblue", "tomato")) +
  ggplot2::scale_y_continuous(breaks = pretty(tripadv_sent_word$nsign),
                              labels = abs(pretty(tripadv_sent_word$nsign))) +
  ggplot2::coord_flip()
  
#-------------------------------------------------------------------------------
# 전체 리뷰에서 긍정/부정 단어 분포(상위 단어)를 확인 함
#-------------------------------------------------------------------------------





# 데이터 전처리   ==============================================================
library(lubridate)

head(tripadv_review$date)

# 월단위로 나누기
tripadv_review$ym = lubridate::floor_date(x = tripadv_review$date,
                                          unit = "month")

tripadv_review # 1712 rows


tripadv_agg = tripadv_review %>% 
                
                # group by 월단위
                dplyr::group_by(ym) %>% 
                
                # 월단위로 평점 평균 구하기 # A tibble: 152 x 2
                dplyr::summarise(rating = mean(rating, na.rm = TRUE), n = n()) %>% 
                
                # group by 해지 # A tibble: 152 x 3
                dplyr::ungroup()

tripadv_agg # A tibble: 152 x 3



# 데이터 분석   ================================================================

ggplot2::ggplot(tripadv_agg,
                aes(x = ym ,  y = rating)) +
  ggplot2::geom_line() +
  ggplot2::geom_smooth()


# 그래프 그리기 (시간에 따른 평점)
ggplot2::ggplot(tripadv_agg,
                aes(x = ym ,  y = rating)) +
  ggplot2::geom_line(color = "khaki4",
                     size = 1) +
  ggplot2::geom_smooth(color = "dodgerblue",
                       size = 1) +
  ggplot2::scale_x_date(date_labels = "%Y",
                        date_breaks = "2 years") +
  ggplot2::scale_y_continuous(breaks = c(1,2,3,4,5),
                              labels = c("1 star", "2 star", "3 star", "4 star", "5 star")) +
  ggplot2::labs(x = NULL,
                y = "Rating",
                title = "Review of Planet Hollywood",
                subtitle = "Rating scores over time",
                caption = "Source : Tripadvisor") +
  ggplot2::theme(plot.title = element_text(face = "bold"),
                 text = element_text(family = "sans"))


# 그래프 그리기 (시간에 따른 리뷰 수)
ggplot2::ggplot(tripadv_agg,
                aes(x = ym ,  y = n)) +
  ggplot2::geom_line(color = "khaki4",
                     size = 1) +
  ggplot2::geom_smooth(color = "dodgerblue",
                       size = 1) +
  ggplot2::scale_x_date(date_labels = "%Y",
                        date_breaks = "2 years") +
  ggplot2::labs(x = NULL,
                y = "Number of review",
                title = "Review of Planet Hollywood",
                subtitle = "Number of review scores over time",
                caption = "Source : Tripadvisor") +
  ggplot2::theme(plot.title = element_text(face = "bold"),
                 text = element_text(family = "sans"))




















































