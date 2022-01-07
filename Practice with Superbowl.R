.libPaths("C:/Rlibs")

#Calling the package
#install.packages("tidytuesdayR")
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2021-03-02')
superbowl <- tuesdata$youtube

#Counting words
tidy_superbowl %>%
  count(word, sort=TRUE)

library(dplyr)
library(stringr)

View(superbowl[1:40,])
unique(superbowl$title)
unique(superbowl$description)

#Changing to text #Need to find location information for startrek
colnames(superbowl)[22] <- "text"

#Tokenizing
library(tidytext)
tidy_superbowl <- superbowl %>%
  unnest_tokens(word, text)
print(tidy_superbowl)

#removing stop words
data(stop_words)
superbowl_no_stop <- tidy_superbowl %>%
  anti_join(stop_words)
print(superbowl_no_stop)
#printing the count frequencies for each token without stop words
superbowl_no_stop %>%
  count(word, sort=TRUE)

#plotting the token frequencies:
#plotting the one without the stop words
#The minute you switch to ggplot you change from %>% to + 
library(ggplot2)
freq_hist <-superbowl_no_stop %>%
  count(word, sort=TRUE) %>%
  filter(n>200) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist)
