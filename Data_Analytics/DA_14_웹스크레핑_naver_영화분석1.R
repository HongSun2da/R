################################################################################
# 웹 스크레이핑과 데이터분석/시각화 - XPath - 사례: 영화 리뷰 수집 @네이버
################################################################################

# 데이터 수집   ================================================================

# url 확인
# https://movie.naver.com/movie/point/af/list.nhn?st=mcode&sword=143435&target=&page=1
# https://movie.naver.com/movie/point/af/list.nhn?st=mcode&sword=143435&target=&page=2
# https://movie.naver.com/movie/point/af/list.nhn?st=mcode&sword=143435&target=&page=3
# ...
# https://movie.naver.com/movie/point/af/list.nhn?st=mcode&sword=143435&target=&page=999
# https://movie.naver.com/movie/point/af/list.nhn?st=mcode&sword=143435&target=&page=1000


url = "https://movie.naver.com/movie/point/af/list.nhn?st=mcode&sword=143435&target=&page=1"

#install.packages("RCurl")
library(RCurl)

web = RCurl::getURL(url)
web

# 한글 깨짐 현상      ----------------------------------------------------------
# 웹 encoding 확인 1
library(stringr)
stringr::str_extract_all(web, "<meta.+?>")

# "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=euc-kr\">"   


# 웹 encoding 확인 2
library(readr)
readr::guess_encoding(file = url)
# # A tibble: 4 x 2
# encoding   confidence
# <chr>           <dbl>
# 1 EUC-KR          1    
# 2 GB18030         0.62 
# 3 EUC-JP          0.48 
# 4 ISO-8859-1      0.290

# encoding 값
web_enc = as.character(readr::guess_encoding(file = url)[1,1])

web = RCurl::getURL(url,
                    .encoding = web_enc)
web


# PC(local) encoding 확인
localeToCharset() # "CP949"
Encoding(web) #"unknown"

#-------------------------------------------------------------------------------
?Encoding # {base} Read or Set the Declared Encodings for a Character Vector

# - "latin1" or "UTF-8" or "bytes" 만 사용
# Encoding(x)
# Encoding(x) <- value
# enc2native(x)
# enc2utf8(x)
#-------------------------------------------------------------------------------

library(XML)

html = XML::htmlParse(web,
                      encoding = localeToCharset())
html



# <td class="ac num">17467640</td>
# writeno
writeno = XML::xpathSApply(html,
                          "//td[@class='ac num']",
                          xmlValue)
writeno

# 숫자형 변환
writeno = as.numeric(writeno)
writeno



# <a href="javascript:find_list('nickname','17467640', 'after');" class="author">ekle****</a>
# author
author = XML::xpathSApply(html,
                          "//a[@class='author']",
                          xmlValue)
author
  
# <td class="num"><a href="javascript:find_list('nickname','17467640', 'after');" class="author">ekle****</a><br>21.04.10</td>
# date
date = XML::xpathSApply(html,
                          "//td[@class='num']/text()",
                          xmlValue)
date

# Date형 변화
date = as.Date(date, format = "%y.%m.%d")
date

# <a href="/movie/bi/mi/basic.nhn?code=143435" class="movie color_b">옥자</a>
# title
title = XML::xpathSApply(html,
                          "//a[@class='movie color_b']",
                          xmlValue)
title


library(stringi)

# encoding 문자 확인 # "native"
stringi::stri_enc_mark(title) # "native"

# utf8 가능 여부 확인 # TRUE
stringi::stri_enc_isutf8(title)

# encoding 변환
title = str_conv(string = title,
                 encoding = "UTF-8")
title

# <td class="title">
#   <a href="/movie/bi/mi/basic.nhn?code=143435" class="movie color_b">옥자</a>
#   <div class="list_netizen_score">
#   <span class="st_off"><span class="st_on" style="width:100%">별점 - 총 10점 중</span></span><em>10</em>
#   </div>
#   <br>이런 영화 만들 멘탈들이 안되는 나머지 조폭영화 한국감독들 
#   <a href="#" onclick="report('krgl****', '8YRFBSUpZOqX4PNuWuWS7nE4MlKSKirvbKMeaBIohUQ=', '이런 영화 만들 멘탈들이 안되는 나머지 조폭영화 한국감독들', '17459829', 'point_after');" class="report" style="color:#8F8F8F" title="새 창">신고</a>
# </td>
# review
review = XML::xpathSApply(html,
                         "//td[@class='title']",
                         function(x){
                           val = unlist(xpathSApply(x, "./text()[normalize-space()]", xmlValue))
                           if (is.null(val)) val = NA
                           else val
                         }) %>% 
  str_conv(encoding = "UTF-8") %>%
  stringr::str_trim()

review

# <div class="list_netizen_score">
#   <span class="st_off"><span class="st_on" style="width:100%">별점 - 총 10점 중</span></span><em>10</em>
# </div>
# rating
rating = XML::xpathSApply(html,
                        "//div[@class='list_netizen_score']/em",
                        xmlValue)
rating

# 숫자형 변환
rating = as.numeric(rating)
rating




# 데이터 수집(전체)   ==========================================================
library(RCurl)
library(XML)
library(stringi)

naver_movie = data.frame()

for (i in c(1:1000)) {
  print(i) # debugging
  
  url = paste0("https://movie.naver.com/movie/point/af/list.nhn?st=mcode&sword=143435&target=&page=",i)
  
  print(url) # debugging

  # encoding 값
  web_enc = as.character(readr::guess_encoding(file = url)[1,1])
  web = RCurl::getURL(url,
                      .encoding = web_enc)

    html = XML::htmlParse(web,
                        encoding = localeToCharset())

  writeno = XML::xpathSApply(html,
                             "//td[@class='ac num']",
                             xmlValue)
  writeno = as.numeric(writeno)

  author = XML::xpathSApply(html,
                            "//a[@class='author']",
                            xmlValue)

  date = XML::xpathSApply(html,
                          "//td[@class='num']/text()",
                          xmlValue)
  date = as.Date(date, format = "%y.%m.%d")

  print(date) # debugging
  
  title = XML::xpathSApply(html,
                           "//a[@class='movie color_b']",
                           xmlValue)
  title = str_conv(string = title,
                   encoding = "UTF-8")

  review = XML::xpathSApply(html,
                            "//td[@class='title']",
                            function(x){
                              val = unlist(xpathSApply(x, "./text()[normalize-space()]", xmlValue))
                              if (is.null(val)) val = NA
                              else val
                            }) %>% 
    str_conv(encoding = "UTF-8") %>%
    stringr::str_trim()

  rating = XML::xpathSApply(html,
                            "//div[@class='list_netizen_score']/em",
                            xmlValue)
  rating = as.numeric(rating)
  rating

  if(length(writeno) > 0)
  {
    naver_movie_df = data.frame(writeno = writeno,
                            author = author,
                            date = date,
                            title = title,
                            review = review,
                            rating = rating,
                            stringsAsFactors = FALSE)
    naver_movie = rbind(naver_movie, naver_movie_df)
  }
  else break
  Sys.sleep(sample(10, 1)*0.1)
}


naver_movie


# row id 삽입 하기
naver_review = cbind(id=1:nrow(naver_movie), naver_movie)

View(naver_movie)

# 데이터 저장 하기
save(naver_movie, file="naver_movie01.rda")

load("naver_movie01.rda")









































