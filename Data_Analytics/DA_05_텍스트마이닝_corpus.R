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
#     -> 
# - 타이디-텍스트(tidy-text)
# - 문서-용어행렬(document-term matrix)

################################################################################

# 데이터 수집  =================================================================
# install.packages("tm")
library(tm)

data(crude)

View(crude)

crude
# <<VCorpus>>
# Metadata:  corpus specific: 0, document level (indexed): 0
# Content:  documents: 20

class(crude) # "VCorpus" "Corpus"

crude[[1]]$meta
crude[[1]]$content

crude[[3]]$meta
crude[[3]]$content


# corpus 만들기 ----------------------------------------------------------------
text = c("Diamond Shamrock Corp said that effective today it had cut its contract prices for crude oil by 1.50 dlrs a barrel.",
          "The reduction brings its posted price for West Texas Intermediate to 16.00 dlrs a barrel, the copany said.",
          "The price reduction today was made in the light of falling oil product prices and a weak crude oil market",
          "Texaco Canada said it lowered the contract price it will pay for crude oil 64 Canadian cts a barrel",
          "effective today. The decrease brings the company's posted price for the benchmark grade",
          "Edmonton/Swann Hills Light Sweet, to 22.26 Canadian dlrs a bbl.",
          "Texaco Canada last changed its crude oil postings on Feb 19.Reuter")


tm::getSources() 
# "DataframeSource" "DirSource"       "URISource"       "VectorSource"    "XMLSource"       "ZipSource" 

docs = tm::VCorpus(tm::VectorSource(text))

class(docs) # "VCorpus" "Corpus" 
View(docs)

# content 정보 확인

tm::inspect(docs[1])
tm::inspect(docs[[1]])

as.character(docs[[1]])

lapply(docs, as.character)

str(docs[[1]])

docs[[1]]$content
docs[[1]]$meta$id

paste(as.vector(unlist(lapply(docs, content))), collapse = " ")

# 메타정보 확인
meta(docs[[1]])

meta(docs[[1]], tag="id")
meta(docs[[1]], tag="datetimestamp")

meta(docs[[1]], tag="description")

# set
meta(docs[[1]], tag="author", type="local") = "BBC"
meta(docs[[1]])

meta(docs, tag="author", type="local") = c("BBC","CNN","Fox")
lapply(docs, meta, tag="author")

meta(docs, tag="category", type="local") = "default"
lapply(docs, meta, tag="category")

# 삭제
meta(docs, tag="sort", type="local") = NULL
lapply(docs, meta, tag="sort")

# Filter 검색
docs_filt = tm_filter(docs,
                      FUN=function(x) any(grep("price|effective", content(x))))
lapply(docs_filt, content)

# 파일 만들기
tm::writeCorpus(docs)

list.files(pattern=".txt")



# 문자열 변경하기  -------------------------------------------------------------

tm::getTransformations()
# "removeNumbers"     "removePunctuation" "removeWords"       "stemDocument"      "stripWhitespace"  

# 소문자 변경하기
docs = tm::tm_map(docs, content_transformer(tolower))
lapply(docs, content)

# 불용어 제거
docs = tm::tm_map(docs, removeWords, tm::stopwords("english"))
lapply(docs, content)

# 사용자 정의 함수
myRemove = content_transformer(function(x, pattern)
  {
    return(gsub(pattern, "", x)) 
})

docs = tm::tm_map(docs, myRemove, "(f|ht)tp\\S+\\s*")
lapply(docs, content)

docs = tm::tm_map(docs, removePunctuation)
docs = tm::tm_map(docs, removeNumbers)
lapply(docs, content)

docs = tm::tm_map(docs, stripWhitespace)
lapply(docs, content)

docs = tm::tm_map(docs, content_transformer(trimws))
lapply(docs, content)

# 어간 축출 작업
docs = tm::tm_map(docs, stemDocument)
lapply(docs, content)








