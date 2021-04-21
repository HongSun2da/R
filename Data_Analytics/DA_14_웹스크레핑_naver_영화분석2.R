################################################################################
# 웹 스크레이핑과 데이터분석/시각화 - XPath - 사례: 영화 리뷰 수집 @네이버
################################################################################

load("naver_movie01.rda")
ls()

str(naver_movie) # 'data.frame':	10000 obs. of  6 variables:

View(naver_movie)


# 한글 전처리   ----------------------------------------------------------------
library(tidyverse)

okja_review = naver_movie$review[20:30]

okja_review %>% 
  # [ㅠㅠ, ㅋㅋㅋ, ㅎㅎㅎ] 단글 제어 
  stringr::str_replace_all("[ㄱ-ㅣ]", "") %>% 
  
  # [.,] 문장부호 제거
  stringr::str_replace_all("[[:punct:]]", "") %>% 

  # [] 숫자 제거
  stringr::str_replace_all("[[:digit:]]", "") %>% 

  # [] 문장 앞뒤 공백 제처
  stringr::str_trim()

okja_review


# java 설치 작업
# https://www.oracle.com/java/technologies/javase-downloads.html
#  - Oracle JDK (JRE Download) 설치

# KoNLP 설치 경로에 java 파일 확인
# C:\Users\Hongsun\Documents\R\win-library\4.0\KoNLP\java
# jhannanum.jar, junit-4.10.jar, ko.jar, scala-library-2.11.8.jar


# [KoNLP] 설치 문제   ----------------------------------------------------------
#install.packages("rJava")
library(rJava)

#install.packages("hash")
library(hash)

#install.packages("Sejong")
library(Sejong)

#install.packages("RSQLite")
library(RSQLite)

#install.packages("memoise")
library(memoise)

# ERROR: dependencies 'rJava', 'hash', 'Sejong', 'RSQLite' are not available for package 'KoNLP'
install.packages("https://cran.r-project.org/src/contrib/Archive/KoNLP/KoNLP_0.80.2.tar.gz",
                 repos = NULL,
                 type = "source",
                 INSTALL_opts = c("--no-lock"))

library(KoNLP)  


# 데이터 전처리   ==============================================================
library(KoNLP) 
library(rJava)

# 단어 형태소사전 생성
buildDictionary(ext_dic = c("Sejong", "woorimalsam","insighter")) 
# 983012 words dictionary was built.


#-------------------------------------------------------------------------------
?SimplePos09 # {KoNLP} POS tagging by using 9 KAIST tags

# SimplePos09(sentences, autoSpacing = FALSE)
# https://github.com/haven-jeon/KoNLP/blob/master/etcs/KoNLP-API.md 
#-------------------------------------------------------------------------------

okja_words = SimplePos09(okja_review, autoSpacing = FALSE)

okja_words





























