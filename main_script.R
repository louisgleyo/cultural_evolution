library(tidyverse)
library(dplyr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)

# [0] Data import and functions declaration
headlines <- read.csv2("C:\\Users\\Louis GLEYO\\Downloads\\headlines.csv",sep = ",",encoding="UTF-8")
bad_words <-read.csv2("C:\\Users\\Louis GLEYO\\Downloads\\bad_words_en.csv",sep = ",",encoding="UTF-8")

## Global variable that will store all bad words detected in the headlines
list_bad_words <<- list()

## Function that checks if a string contains a bad word
## Param : headline_str : a string
## Result : TRUE if it contains a bad word, FALSE else
contains_bad_word <- function(headline_str){
  # Splits the string into a list of words
  headline_words = strsplit(headline_str, "[[:punct:][:space:]]+")[[1]]
  # Check word by word if it is a swear word
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

## Apply the contain_bad_word function to a column in
## Param : headline_str : a column of string
## Result : A vector of booleans
contains_bad_word_global <-function(headline_column){
  RESULTS <- vector(mode="logical", length=length(headline_column))
  for(i in 1:length(RESULTS)){
    RESULTS[i] = contains_bad_word(headline_column[i])
  }
  return(RESULTS)
}

## Group rows by test_id and headline, add clicks and print
headlines_group   = headlines %>% 
  group_by(clickability_test_id, headline) %>% summarise(impressions = sum(impressions),clicks = sum(clicks))

## Mutation of headline_group, to which we add the
## calculation of the click_rate and the variable 
## bad_word which is TRUE if the title contains a swear word and FALSE otherwise
headlines_group_2 = headlines_group %>% 
  mutate(click_rate = clicks/impressions, bad_word = contains_bad_word_global(headline))

## Linear regression with click rate as dependent variable and 
## bad_word as explanatory variable
LM = lm(click_rate~bad_word,data=headlines_group_2)
tab_model(LM,digits = 5, digits.p = 5,digits.rsq = 5)

## [2] Counts the occurrence of each swear word found in the UpWorthy dataset
count_bad_words = sort(table(unlist(list_bad_words)))
count_bad_words

## [3] Count the number of experiences such that there are some titles 
## that contain swear words and others that do not
headlines_group_3 = headlines_group_2 %>% group_by(clickability_test_id,bad_word)
length(headlines_group_2) - length(headlines_group_3)
