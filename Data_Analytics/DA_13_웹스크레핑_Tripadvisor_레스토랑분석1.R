################################################################################
# 웹 스크레이핑과 데이터분석/시각화 - XPath - 사례: 레스토랑 리뷰 수집 @Tripadvisor
################################################################################

# 데이터 수집   ================================================================

# url 확인
# https://www.tripadvisor.com/Restaurant_Review-g60763-d477604-Reviews-Planet_Hollywood-New_York_City_New_York.html
# https://www.tripadvisor.com/Restaurant_Review-g60763-d477604-Reviews-or10-Planet_Hollywood-New_York_City_New_York.html
# https://www.tripadvisor.com/Restaurant_Review-g60763-d477604-Reviews-or20-Planet_Hollywood-New_York_City_New_York.html
# ...
# https://www.tripadvisor.com/Restaurant_Review-g60763-d477604-Reviews-or720-Planet_Hollywood-New_York_City_New_York.html

library(xml2)
library(XML)

urlString = "https://www.tripadvisor.com/Restaurant_Review-g60763-d477604-Reviews-or10-Planet_Hollywood-New_York_City_New_York.html"

html = xml2::read_html(urlString)
html_parsed = XML::htmlParse(html)

html_parsed
class(html_parsed) # "HTMLInternalDocument" "HTMLInternalDocument" "XMLInternalDocument"  "XMLAbstractDocument" 



# <h1 data-test-target="top-info-header" class="_3a1XQ88S">Planet Hollywood</h1>
# name 
name = XML::xpathSApply(html_parsed,
                        "//h1[@data-test-target='top-info-header']",
                        xmlValue)
name

# <a href="#MAPVIEW" class="_15QfMZ2L">1540 Broadway Frnt 1, New York City, NY 10036-4039</a>
# location 
location = XML::xpathSApply(html_parsed,
                            "//a[@href='#MAPVIEW']",
                            xmlValue)
location

# <div class="info_text pointer_cursor" onclick="widgetEvCall('handlers.usernameClick', event, this);"><div>tomthurlow</div></div>
# <div class="info_text " onclick="widgetEvCall('handlers.usernameClick', event, this);"><div>A Tripadvisor reviewer on Facebook</div></div>
# author 
author = XML::xpathSApply(html_parsed,
                            "//div[@class='info_text pointer_cursor']//div | //div[@class='info_text ']//div",
                            xmlValue)
author

# <span class="ratingDate" title="July 29, 2020">Reviewed July 29, 2020 </span>
# date
date = XML::xpathSApply(html_parsed,
                          "//span[@class='ratingDate']",
                          xmlGetAttr, "title")
date

# Date 형 변화
Sys.setlocale("LC_TIME", "English")
date = as.Date(date, format = "%B %d, %Y")
Sys.setlocale()

date

# <span class="noQuotes">Fantastic service, average food</span>
# quote
quote = XML::xpathSApply(html_parsed,
                        "//span[@class='noQuotes']",
                        xmlValue)
quote

# <p class="partial_entry">We visited Planet Hollywood with our two small children. The restaurant was busy and the atmosphere was really good. I had the burger which was tasty and my little boy had the tacos which looked cool. The waitress was great she told us all about...<span class="postSnippet">the menu, drinks, and took the time to interact with us which was nice. We had a problem whilst waiting for a plate of food but other than this there was absolutely nothing wrong with the Planet Hollywood experience.</span><span class="taLnk ulBlueLinks" onclick="widgetEvCall('handlers.clickExpand',event,this);">More</span></p>
# review
review = XML::xpathSApply(html_parsed,
                           "//div[@class='ui_column is-9']/div[@class='prw_rup prw_reviews_text_summary_hsx']//p[@class='partial_entry']",
                           xmlValue)
review

# <span class="ui_bubble_rating bubble_30"></span>
# rating
rating = XML::xpathSApply(html_parsed,
                          "//div[@class='reviewSelector']//div[@class='ui_column is-9']/span[1]",
                          xmlGetAttr, "class")
rating

library(stringr)
# 숫자형 변환
rating = as.numeric(str_sub(rating, start = -2))/10
rating



# 전체 페이지 자동 데이터 수집   ===============================================


# <a data-page-number="183" data-offset="1820" class="pageNum last " onclick="widgetEvCall('handlers.paginate', event, this); widgetEvCall('handlers.trackClick', event, this, 'last', '183');" href="/Restaurant_Review-g60763-d477604-Reviews-or1820-Planet_Hollywood-New_York_City_New_York.html">183</a>
# total_page 
total_page = XML::xpathSApply(html_parsed,
                          "//div[@class='unified ui_pagination ']//div[@class='pageNumbers']/a[@class='pageNum last ']",
                          xmlValue)
total_page
# 숫자형 변환
total_page = as.numeric(gsub(",","", total_page))
total_page
  


library(xml2)
library(XML)
library(stringr)

baseurl = "https://www.tripadvisor.com/Restaurant_Review-g60763-d477604-Reviews-Planet_Hollywood-New_York_City_New_York.html"

tripadv_review = data.frame()
Sys.setlocale("LC_TIME", "English")
for (i in c(1:183)) {
  print(i) # debugging
  
  if(i == 1) {url = baseurl}
  else {
    url = paste0("https://www.tripadvisor.com/Restaurant_Review-g60763-d477604-Reviews-or",
                 (i-1)*10,
                 "-Planet_Hollywood-New_York_City_New_York.html")
  }

  print(url) # debugging

  html = xml2::read_html(url)
  html_parsed = XML::htmlParse(html)

  name = XML::xpathSApply(html_parsed,
                          "//h1[@data-test-target='top-info-header']",
                          xmlValue)

  location = XML::xpathSApply(html_parsed,
                              "//a[@href='#MAPVIEW']",
                              xmlValue)

  author = XML::xpathSApply(html_parsed,
                            "//div[@class='info_text pointer_cursor']//div | //div[@class='info_text ']//div",
                            xmlValue)

  date = XML::xpathSApply(html_parsed,
                          "//span[@class='ratingDate']",
                          xmlGetAttr, "title")
  date = as.Date(date, format = "%B %d, %Y")

  quote = XML::xpathSApply(html_parsed,
                           "//span[@class='noQuotes']",
                           xmlValue)

  review = XML::xpathSApply(html_parsed,
                            "//div[@class='ui_column is-9']/div[@class='prw_rup prw_reviews_text_summary_hsx']//p[@class='partial_entry']",
                            xmlValue)

  rating = XML::xpathSApply(html_parsed,
                            "//div[@class='reviewSelector']//div[@class='ui_column is-9']/span[1]",
                            xmlGetAttr, "class")
  rating = as.numeric(str_sub(rating, start = -2))/10

  # 데이터 병합
  if(length(date) > 0)
  {
    tripadv_df = data.frame(name = name,
                            location = location,
                            author = author,
                            date = date,
                            quote = quote,
                            review = review,
                            rating = rating,
                            stringsAsFactors = FALSE)
    tripadv_review = rbind(tripadv_review, tripadv_df)
  }
  else break
  Sys.sleep(sample(10, 1)*0.1)
}

Sys.setlocale()

tripadv_review



# row id 삽입 하기
tripadv_review = cbind(id=1:nrow(tripadv_review), tripadv_review)

View(tripadv_review)

# 데이터 저장 하기
save(tripadv_review, file="tripadv_review01.rda")

load("tripadv_review01.rda")

















  

