################################################################################
# 웹 스크레이핑과 데이터분석/시각화 - XPath - 사례: 도서 리뷰 수집 @amazon
################################################################################


# 데이터 수집   ================================================================

# url 확인
# https://www.amazon.com/Who-Moved-My-Cheese-Amazing/product-reviews/0399144463/ref=cm_cr_getr_d_paging_btm_prev_1?ie=UTF8&reviewerType=all_reviews&pageNumber=1
# https://www.amazon.com/Who-Moved-My-Cheese-Amazing/product-reviews/0399144463/ref=cm_cr_getr_d_paging_btm_next_2?ie=UTF8&reviewerType=all_reviews&pageNumber=2
# https://www.amazon.com/Who-Moved-My-Cheese-Amazing/product-reviews/0399144463/ref=cm_cr_getr_d_paging_btm_next_3?ie=UTF8&reviewerType=all_reviews&pageNumber=3

#install.packages("XML")
#install.packages("httr")

library(XML)
library(xml2)
library(httr)

# 데이터 1 페이지 확인 작업   --------------------------------------------------

url = "https://www.amazon.com/Who-Moved-My-Cheese-Amazing/product-reviews/0399144463/ref=cm_cr_getr_d_paging_btm_prev_368?ie=UTF8&reviewerType=all_reviews&pageNumber=368"

# 본문을 다 못 읽어 옴
html = xml2::read_html(url)

html
class(html) # "xml_document" "xml_node" 
str(html)
# List of 2
# $ node:<externalptr> 
#   $ doc :<externalptr> 
#   - attr(*, "class")= chr [1:2] "xml_document" "xml_node"


# 본문을 다 읽어 옴
web = httr::GET(url)

web
class(web) # "response"
str(web)


# html 확인 하기  
html_parsed = XML::htmlParse(web)

html_parsed
class(html_parsed) # "HTMLInternalDocument" "HTMLInternalDocument" "XMLInternalDocument"  "XMLAbstractDocument" 
str(html_parsed)   # Classes 'HTMLInternalDocument', 'HTMLInternalDocument', 'XMLInternalDocument', 'XMLAbstractDocument' <externalptr> 


#-------------------------------------------------------------------------------
?XML::xpathSApply # {XML} Find matching nodes in an internal XML tree/DOM

# getNodeSet(doc, path, namespaces = xmlNamespaceDefinitions(doc, simplify = TRUE), 
#            fun = NULL, sessionEncoding = CE_NATIVE, addFinalizer = NA, ...)
# xpathApply(doc, path, fun, ... ,
#            namespaces =  xmlNamespaceDefinitions(doc, simplify = TRUE),
#            resolveNamespaces = TRUE, addFinalizer = NA, xpathFuns = list())
# xpathSApply(doc, path, fun = NULL, ... ,
#             namespaces = xmlNamespaceDefinitions(doc, simplify = TRUE),
#             resolveNamespaces = TRUE, simplify = TRUE,
#             addFinalizer = NA)
# matchNamespaces(doc, namespaces,
#                 nsDefs = xmlNamespaceDefinitions(doc, recursive = TRUE, simplify = FALSE),
#                 defaultNs = getDefaultNamespace(doc, simplify = TRUE))
#-------------------------------------------------------------------------------


# [product] 추출  ------------
product = XML::xpathSApply(html_parsed,
                           "//h1/a[@data-hook='product-link']",
                           xmlValue)
product


# [author] 추출   ------------
author = XML::xpathSApply(html_parsed,
                           "//div[@class='a-section review aok-relative']//span[@class='a-profile-name']",
                           xmlValue)
author


# [date] 추출     ------------
date = XML::xpathSApply(html_parsed,
                         "//span[@data-hook='review-date']",
                         xmlValue)
date = gsub("Reviewed.*on ", "", date)

# date 날짜형 변환
Sys.setlocale("LC_TIME","English")
date = as.Date(date, format = "%B %d, %Y")
Sys.setlocale()
date


# [quote] 추출    ------------
quote = XML::xpathSApply(html_parsed,
                        "//a[@data-hook='review-title']//span[0]",
                        xmlValue)
quote


# [review] 추출   ------------
review = XML::xpathSApply(html_parsed,
                         "//span[@data-hook='review-body']",
                         xmlValue)
review


# [rating] 추출   ------------
rating = XML::xpathSApply(html_parsed,
                          "//i[@data-hook='review-star-rating']//span",
                          xmlValue)
rating

# 숫자형 변화
rating = as.numeric(substr(rating, 1, 3))
rating


# [helpful] 추출   ------------
helpful = XML::xpathSApply(html_parsed,
                           "//span[@data-hook='review-voting-widget']",
                           function(x){
                             # 추천수가 없는 경우 
                             val = unlist(XML::xpathSApply(x,
                                                           ".//span[@data-hook='helpful-vote-statement']",
                                                           xmlValue))
                             if(is.null(val)) val = NA
                             else val
                           })
helpful

# 숫자형 변화 <= One, 2, 3, ... 1,000, 1,500
library(dplyr)
library(stringr)

helpful = helpful %>% 
  stringr::str_replace("One", "1") %>% 
  stringr::str_replace(",", "") %>% 
  stringr::str_extract("\\d+") %>% 
  as.numeric()
helpful  


# Data frame 만들기  -----------------------------------------------------------
data_review = data.frame(product = product,
                         author = author,
                         date = date,
                         quote = quote,
                         review = review,
                         rating = rating,
                         helpful = helpful,
                         stringsAsFactors = FALSE)
str(data_review) # 'data.frame':	10 obs. of  7 variables:
View(data_review)




# 데이터 전체 페이지(500 page) 확인 작업   -------------------------------------

library(XML)
library(xml2)
library(httr)
library(dplyr)
library(stringr)

Sys.setlocale("LC_TIME","English")
amazon_review = data.frame()
for (i in c(1:500)) {
  print(paste(as.character(i), "-------------------------------------")) # debugging
  
  url = paste0("https://www.amazon.com/Who-Moved-My-Cheese-Amazing/product-reviews/0399144463/ref=cm_cr_getr_d_paging_btm_prev_",
               as.character(i),
               "?ie=UTF8&reviewerType=all_reviews&pageNumber=",
               as.character(i))
  
  print(url) # debugging
  
  # 본문 읽기
  html = httr::GET(url)
  
  # html 확인 하기  
  html_parsed = XML::htmlParse(html)

  # [product] 추출  ------------
  product = XML::xpathSApply(html_parsed,
                             "//h1/a[@data-hook='product-link']",
                             xmlValue)
  #print(product)  # debugging
  
  # [author] 추출   ------------
  author = XML::xpathSApply(html_parsed,
                            "//div[@class='a-section review aok-relative']//span[@class='a-profile-name']",
                            xmlValue)
  #print(author)  # debugging
  
  # [date] 추출     ------------
  date = XML::xpathSApply(html_parsed,
                          "//span[@data-hook='review-date']",
                          xmlValue)
  date = gsub("Reviewed.*on ", "", date)
  date = as.Date(date, format = "%B %d, %Y")

  #print(date)  # debugging
  
  # [quote] 추출    ------------
  quote = XML::xpathSApply(html_parsed,
                           "//a[@data-hook='review-title']//span[1]",
                           xmlValue)
  #print(quote) # debugging
  
  # [review] 추출   ------------
  review = XML::xpathSApply(html_parsed,
                            "//span[@data-hook='review-body']",
                            xmlValue)
  # [rating] 추출   ------------
  rating = XML::xpathSApply(html_parsed,
                            "//i[@data-hook='review-star-rating']//span",
                            xmlValue)
  rating = as.numeric(substr(rating, 1, 3))
  
  #print(rating)  # debugging

  # [helpful] 추출   ------------
  helpful = XML::xpathSApply(html_parsed,
                             "//span[@data-hook='review-voting-widget']",
                             function(x){
                               # 추천수가 없는 경우 
                               val = unlist(XML::xpathSApply(x,
                                                             ".//span[@data-hook='helpful-vote-statement']",
                                                             xmlValue))
                               if(is.null(val)) val = NA
                               else val
                             })
  # 숫자형 변화 <= One, 2, 3, ... 1,000, 1,500
  helpful = helpful %>% 
    stringr::str_replace("One", "1") %>% 
    stringr::str_replace(",", "") %>% 
    stringr::str_extract("\\d+") %>% 
    as.numeric()
  
  #print(helpful)  # debugging
  
  # 데이터 병합
  if(length(date) > 0)
  {
    amazon_df = data.frame(product = product,
                             author = author,
                             date = date,
                             quote = quote,
                             review = review,
                             rating = rating,
                             helpful = helpful,
                             stringsAsFactors = FALSE)        
    amazon_review = rbind(amazon_review, amazon_df)
  }
  else break
  Sys.sleep(sample(10, 1)*0.1)

}

Sys.setlocale()

# row id 삽입 하기
amazon_review = cbind(id=1:nrow(amazon_review), amazon_review)

View(amazon_review)

# 데이터 저장 하기
save(amazon_review, file="amazon_review_cheese01.rda")

load("amazon_review_cheese01.rda")



















































































