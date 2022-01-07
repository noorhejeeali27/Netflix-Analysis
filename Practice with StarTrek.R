.libPaths("C:/Rlibs")

#Calling the package
#install.packages("tidytuesdayR")
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2021-08-17')
startrek <- tuesdata$computer

#Counting words
tidy_startrek %>%
  count(word, sort=TRUE)

library(dplyr)
library(stringr)

View(startrek[1:40,])
unique(startrek$char)
unique(startrek$line)

#Changing to text #Need to find location information for startrek
colnames(startrek)[2] <- "text"

#Tokenizing
library(tidytext)
tidy_startrek <- startrek %>%
  unnest_tokens(word, text)
print(tidy_startrek)

#removing stop words
data(stop_words)
startrek_no_stop <- tidy_startrek %>%
  anti_join(stop_words)
print(startrek_no_stop)
#printing the count frequencies for each token without stop words
startrek_no_stop %>%
  count(word, sort=TRUE)

#plotting the token frequencies:
#plotting the one without the stop words
#The minute you switch to ggplot you change from %>% to + 
library(ggplot2)
freq_hist <-startrek_no_stop %>%
  count(word, sort=TRUE) %>%
  filter(n>200) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist)


