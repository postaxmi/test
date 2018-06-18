library(topicmodels)
library(readr)
library(tm)
library(tidyverse)
library(tidytext)
data<-read_csv("../../statistics/Texas Last Statement - CSV.csv",enc)

# column TDCJNumber can be used as indentifier for each observation
# column LastStatement has the text of the last statement

# there are some problems with some characters, let's convert to ascii
data$LastStatement<-iconv(data$LastStatement,from="UTF-8",to="ASCII//TRANSLIT")

# get document term matrix
docs <- data.frame(doc_id = data$TDCJNumber,
                   text = data$LastStatement,
                   stringsAsFactors = FALSE)
ds <- DataframeSource(docs)
x <- Corpus(ds)
dtm <- DocumentTermMatrix(x)


# get document term matrix (another way)
tokens<-unnest_tokens(data,word, LastStatement,token="ngrams",n=1)
words<-tokens %>% group_by(TDCJNumber,word) %>% summarise(count=n())
dtm2<-words %>%   cast_dtm(TDCJNumber, word, count)



rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ]           #remove all docs without words


ap_lda <- LDA(dtm.new, k = 5, control = list(seed = 1234))

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics


ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))
