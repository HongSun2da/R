################################################################################
# 웹 스크레이핑과 데이터분석/시각화 - XPath - 사례: 도서 리뷰 수집 @amazon
################################################################################

# url 확인
# https://www.amazon.com/Who-Moved-My-Cheese-Amazing/product-reviews/0399144463/ref=cm_cr_getr_d_paging_btm_prev_1?ie=UTF8&reviewerType=all_reviews&pageNumber=1
# https://www.amazon.com/Who-Moved-My-Cheese-Amazing/product-reviews/0399144463/ref=cm_cr_getr_d_paging_btm_next_2?ie=UTF8&reviewerType=all_reviews&pageNumber=2
# https://www.amazon.com/Who-Moved-My-Cheese-Amazing/product-reviews/0399144463/ref=cm_cr_getr_d_paging_btm_next_3?ie=UTF8&reviewerType=all_reviews&pageNumber=3

#install.packages("XML")

library(XML)
library(xml2)

url = "https://www.amazon.com/Who-Moved-My-Cheese-Amazing/product-reviews/0399144463/ref=cm_cr_getr_d_paging_btm_prev_1?ie=UTF8&reviewerType=all_reviews&pageNumber=1"
html = xml2::read_html(url)

html
class(html) # "xml_document" "xml_node" 
str(html)
# List of 2
# $ node:<externalptr> 
#   $ doc :<externalptr> 
#   - attr(*, "class")= chr [1:2] "xml_document" "xml_node"

html_parsed = XML::htmlParse(html)

html_parsed
class(html_parsed) # "HTMLInternalDocument" "HTMLInternalDocument" "XMLInternalDocument"  "XMLAbstractDocument" 
str(html_parsed)   # Classes 'HTMLInternalDocument', 'HTMLInternalDocument', 'XMLInternalDocument', 'XMLAbstractDocument' <externalptr> 




