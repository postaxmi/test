library(topicmodels)
library(readr)
library(tm)
library(tidyverse)
library(tidytext)
library(slam)

set.seed(123)

# read document of last statement
data<-read_csv("Texas Last Statement - CSV.csv")
good_indexes <- data$LastStatement != "None"

# column TDCJNumber can be used as indentifier for each observation
# column LastStatement has the text of the last statement

# there are some problems with some characters, let's convert to ascii
data$LastStatement <- iconv(data$LastStatement, from = "UTF-8", to = "ASCII//TRANSLIT")


docs_statement <- data.frame(doc_id = data$TDCJNumber[good_indexes],
                   text = data$LastStatement[good_indexes],
                   stringsAsFactors = FALSE)


## read documents of elections
folders<-c("debates","primaries","speeches")
docs_election<-data.frame(doc_id=character(),text=character())
for(folder in folders){
  file_list <- list.files(folder)
  for (file in file_list){
    fileName<-paste(folder,file,sep="/")
    text<-read_file(fileName)
    docs_election<-rbind(docs_election,data.frame(doc_id=fileName,text=text,stringsAsFactors = F))
  }
}

# choose between docs_statement and docs_election
docs<-docs_statement

### 
 # function to get document term matrix
 # documents: dataframe with columns doc_id and text
 # tfidf_threshold: (optional, default 0) minimum tf-idf value for each word to be kept in the dtm vocabulary
getDtm<-function(documents, tfidf_threshold=0){
  ds  <- DataframeSource(documents)
  x   <- Corpus(ds)
  # remove stpwords, use stemming, remove punctuation and numbers, to lower
  #x   <- tm_map(x, content_transformer(tolower))
  #x   <- tm_map(x, removePunctuation)
  #x   <- tm_map(x, removeWords, stopwords("english"))
  dtm <- DocumentTermMatrix(x, control = list(stemming = TRUE, stopwords = TRUE, minWordLength = 3, removeNumbers = TRUE, removePunctuation = TRUE))

  # reduce vocabulary of the dtm, consider only words with high values of tf-idf
  term_tfidf<-tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) *log2(nDocs(dtm)/col_sums(dtm > 0))
  print(paste("median tf-idf: ",median(term_tfidf)))

  dtm_reduced <- dtm[, term_tfidf >= tfidf_threshold]
  rowTotals <- apply(dtm_reduced , 1, sum) #Find the sum of words in each Document
  dtm_reduced   <- dtm[rowTotals> 0, ]           #remove all docs without words
  return(dtm_reduced)
}

###
 # function to get mean value of entropy for document_topics distribution
 # ap_lda: lda topic model
 # dtm: document term matrix
evaluate_doc_topics_distribution<-function(ap_lda, dtm){
  # get for each document the topic distribution
  doc_topics_distribution<-posterior(ap_lda,dtm)$topics
  # compute for each document the entropy of the topic distribution:
  #  documents with only 1 topic with very high probability will have small value of entropy
  #  documents with many topics with same probability (almost uniform distribution) will have high value of entropy
  doc_topics_distribution$entropy<- -rowSums(doc_topics_distribution*log2(doc_topics_distribution))
  doc_topics_entropy<-mean(doc_topics_distribution$entropy)
  return(doc_topics_entropy)
}

# get document term matrix (another way)
tokens <- unnest_tokens(data,word, LastStatement,token="ngrams",n=1)
words  <- tokens %>% anti_join(stop_words,by="word") %>% group_by(TDCJNumber,word) %>% summarise(count=n())
# Possible elaborations here...
dtm2   <- words  %>% cast_dtm(TDCJNumber, word, count)

dtm.new<- getDtm(docs,tfidf_threshold = 0.1)


ap_lda <- LDA(dtm.new, k = 5, control = list(seed = 1234))

evaluate_doc_topics_distribution(ap_lda,dtm.new)
perplexity(ap_lda,dtm.new)

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
  coord_flip()+
  labs(title="parole dei topic")


beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread %>% top_n(20,abs(log_ratio)) %>% mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term,log_ratio)) + 
  geom_col()+
  coord_flip()+
  labs(title="parole pi? diverse tra i due topic")

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

ap_documents %>% ggplot(aes(x=factor(topic),y=gamma))+
  geom_boxplot()+
  labs(title="probabilit? delle parole dei vari topic")


document_classification <- ap_documents %>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  ungroup()

document_classification %>% ggplot(aes(topic))+
  geom_bar()+
  labs(title="numero di documenti per ogni topic")


## evaluate different models given by varying the number of topics

n_topics<-2:20
folds<-seq_len(10)
folding <- sample(rep(folds, nrow(dtm.new))[seq_len(nrow(dtm.new))])
m<-matrix(nrow=length(n_topics)*length(folds),ncol=4)
i<-1
for (k in n_topics) {
  print(paste("topics: ",k))
  for (fold in folds) {
    m[i,1]<-fold
    m[i,2]<-k
    
    train_dtm<-dtm.new[folding!=fold,]
    test_dtm<-dtm.new[folding==fold,]
    ap_lda <- LDA(train_dtm, k = k, control = list(seed = 1234))
    
    m[i,3]<-perplexity(ap_lda,test_dtm)
    m[i,4]<-evaluate_doc_topics_distribution(ap_lda,test_dtm)
    
    i<-i+1
  }
}

# better rescale entropy based on number of topics
m[,4]<-m[,4]/log(m[,2])

df<-data.frame(m)
colnames(df)<-c("fold","topics","perplexity","entropy")
df<-df %>% gather(measure,value,perplexity,entropy)
ggplot(df,aes(x=factor(topics),y=value))+
  geom_boxplot()+
  geom_point()+
  facet_wrap(~measure,scales = "free")

write.csv(df,"topics_entropy-perplexity")
