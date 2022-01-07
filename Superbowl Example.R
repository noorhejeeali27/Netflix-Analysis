.libPaths("C:/Rlibs")

library(tidytext)
library(janeaustenr)
library(dplyr)
library(stringr)
library(tidyr)
library(tidytuesdayR)

#Calling the package
#install.packages("tidytuesdayR")
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2021-03-02')
superbowl <- tuesdata$youtube

#Use description which is column 22
colnames(superbowl)[22] <- "text"

superbowl_token <- superbowl %>%
  unnest_tokens(word, text)

### creating a tidy format for Budlight movies
#Brand is location information
budlight <- superbowl %>%
  filter(brand == "Bud Light")

tidy_budlight <- budlight %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_budlight)

### creating a tidy format for budweiser movies
budweiser <- superbowl %>%
  filter(brand == "Budweiser")

tidy_budweiser <- budweiser %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_budweiser)

### creating a tidy format for pepsi movies
pepsi <- superbowl %>%
  filter(brand == "Pepsi")

tidy_pepsi <- pepsi %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_pepsi)

#############################################
####We want to combine all the datasets and do frequencies 
#############################################
library(tidyr)
frequency <- bind_rows(mutate(tidy_budlight, author="Bud Light"),
                       mutate(tidy_budweiser, author= "Budweiser"),
                       mutate(tidy_pepsi, author="Pepsi")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Pepsi`, `Budweiser`)

#let's plot the correlograms:
#let's plot the correlograms:
#Words away from the diagonal are country specific
#Common words are in the center
library(scales)
ggplot(frequency, aes(x=proportion, y=`Bud Light`, 
                      color = abs(`Bud Light`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Bud Light", x=NULL)

#From this we can see that for budweiser: funn, ad, beer, bowl,spots, american
#Pepsi is ads, bowls, aired music, superbowl

##########################################
##doing the cor.test() ################
##########################################

cor.test(data=frequency[frequency$author == "Budweiser",],
         ~proportion + `Bud Light`)

cor.test(data=frequency[frequency$author == "Pepsi",],
         ~proportion + `Bud Light`)


#Going into sentiment analysis we can see that funny was used
nrcfunny <- get_sentiments("nrc") %>%
  filter(sentiment == "funny")


########################################################
##### Comparing different sentiment libraries on Netflix ####
########################################################

Budlight <- superbowl_token %>%
  filter(brand == "Bud Light")

afinn <- Budlight %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

#Combining both tokenized Budlight with bing and nrc
#Afinn is numeric and bing_and_nrc are characters
bing_and_nrc <- bind_rows(
  Budlight%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  Budlight %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

#See how interact with each other
#Plot in a bar chart how much sentiment it has
#Budlight movies for afinn is negative(Bing is more negative because positive or negative(1 or -1))
#If a token is -1 then bing will be negative
library(ggplot2)
bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

##############################################################
######## Most common positive and negative words #############
##############################################################

bing_counts <- Budlight %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts

bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()
