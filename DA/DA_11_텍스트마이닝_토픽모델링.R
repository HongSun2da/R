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