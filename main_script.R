library(tidyverse)
library(dplyr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)


headlines <- read.csv2("C:\\Users\\Louis GLEYO\\Downloads\\headlines.csv",sep = ",",encoding="UTF-8")
bad_words <-read.csv2("C:\\Users\\Louis GLEYO\\Downloads\\bad_words_en.csv",sep = ",",encoding="UTF-8")

list_bad_words <<- list()

contains_bad_word_global <-function(headline_column){
  RESULTS <- vector(mode="logical", length=length(headline_column))
  for(i in 1:length(RESULTS)){
    RESULTS[i] = contains_bad_word(headline_column[i])
  }
  return(RESULTS)
}

contains_bad_word <- function(headline_str){
  headline_words = strsplit(headline_str, "[[:punct:][:space:]]+")[[1]]
  for(bad_word in bad_words$bad_word){
    for(headline_word in headline_words){
      if(toupper(bad_word)==toupper(headline_word)){
        list_bad_words <<- c(list_bad_words,bad_word)
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

headlines_group   = headlines %>% 
  group_by(clickability_test_id, headline) %>% summarise(impressions = sum(impressions),clicks = sum(clicks))
headlines_group_2 = headlines_group %>% 
  mutate(click_rate = clicks/impressions, bad_word = contains_bad_word_global(headline))

LM = lm(click_rate~bad_word,data=headlines_group_2)
tab_model(LM,digits = 5, digits.p = 5,digits.rsq = 5)

count_bad_words = sort(table(unlist(list_bad_words)))
count_bad_words

headlines_group_3 = headlines_group_2 %>% group_by(clickability_test_id,bad_word)
length(headlines_group_2) - length(headlines_group_3)
