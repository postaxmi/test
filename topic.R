library(topicmodels)
library(readr)
library(tm)
library(tidyverse)
library(tidytext)
library(slam)
library(LDAvis)
source('lda_tidiers.R')


## Set to 'docs_statement' or 'docs_election'
doc_name = as.name('docs_statement')
##


set.seed(123)

## read document of last statement
data_tls <- read_csv("Texas Last Statement - CSV.csv")
stopwords_sieved <- readLines('stopwords.dat')

# column TDCJNumber can be used as indentifier for each observation
# column LastStatement has the text of the last statement

# there are some problems with some characters, let's convert to ascii
data_tls$LastStatement <- iconv(data_tls$LastStatement, from = "UTF-8", to = "ASCII//TRANSLIT")

# remove entries with no statement, rename variables
docs_statement <- data_tls %>% filter(LastStatement != "None") %>% rename(doc_id=TDCJNumber, text=LastStatement) %>% mutate(doc_id=as.character(doc_id))

## read documents of elections
folders<-c("debates","primaries","speeches")
docs_election<-data.frame(doc_id=character(),text=character(),folder=character(),who=character())
for(folder in folders){
  file_list <- list.files(folder)
  for (file in file_list){
    fileName<-paste(folder,file,sep="/")
    text<-read_file(fileName)
    docs_election<-rbind(docs_election,data.frame(doc_id=fileName,text=text,folder=folder,who=tolower(substr(file,0,5)),stringsAsFactors = F))
  }
}
docs_election <- docs_election %>% mutate(folder=factor(folder),who=factor(who))


### 
 # function to get document term matrix
 # documents: dataframe with columns doc_id and text
 # tfidf_threshold: (optional, default 0) minimum tf-idf value for each word to be kept in the dtm vocabulary
getDtm <- function(documents, tfidf_threshold=0, stopwords_vector = stopwords('SMART')){
  ds  <- DataframeSource(data.frame(doc_id = documents$doc_id,
                                    text = documents$text,
                                    stringsAsFactors = FALSE))
  x   <- SimpleCorpus(ds)
  # to lower, remove stopwords, remove numbers and punctuation, use stemming
  x   <- tm_map(x, content_transformer(tolower))
  # x   <- tm_map(x, removePunctuation)
  x   <- tm_map(x, removeWords, stopwords_vector)
  dtm <- DocumentTermMatrix(x, control = list(minWordLength = 3, removeNumbers = TRUE, removePunctuation = TRUE, stemming = TRUE))

  # reduce vocabulary of the dtm, consider only words with high values of tf-idf
  term_tfidf<-tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) *log2(nDocs(dtm)/col_sums(dtm > 0))
  print(paste("median tf-idf:",median(term_tfidf)))

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

if (FALSE) { # skip, it's not actually needed...
  # another way to get document term matrix
  tokens <- unnest_tokens(data,word, LastStatement,token="ngrams",n=1)
  words  <- tokens %>% anti_join(stop_words,by="word") %>% group_by(TDCJNumber,word) %>% summarise(count=n())
  # Possible elaborations here...
  dtm2   <- words  %>% cast_dtm(TDCJNumber, word, count)
}

# get Document-Term Matrix; with docs_statement we prefer using a reduced stopwords list
docs <- eval(doc_name)
if (doc_name == 'docs_statement') {
  dtm.new <- getDtm(docs, tfidf_threshold = 0, stopwords_vector = stopwords_sieved)
} else {
  dtm.new <- getDtm(docs, tfidf_threshold = 0)
}
#

ap_lda <- LDA(dtm.new, k = 5, control = list(seed=1234,keep=1))
plot(ap_lda@logLiks)
ap_ctm <- CTM(dtm.new, k = 5, control = list(seed=1234,keep=1))
plot(ap_ctm@logLiks)
ap_ctm@Sigma
ap_lda_gibbs <- LDA(dtm.new, k = 5, method = "Gibbs", control = list(burnin = 200, iter = 1000, keep = 1) )
plot(ap_lda_gibbs@logLiks)
# check convergence of chain looking at loglikelihood vs iterations


evaluate_doc_topics_distribution(ap_lda,dtm.new)
perplexity(ap_lda,dtm.new)

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

ap_ctm_topics <- tidy(ap_ctm, matrix = "beta")
ap_ctm_topics

plot.ap_top_terms <- function(ap_topics) {
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
}
plot.ap_top_terms(ap_topics)
plot.ap_top_terms(ap_ctm_topics)

lambda <- 0.6
plot.ap_relevant_terms <- function(ap_topics) {
  ap_rel_terms <- ap_topics %>%
    # mutate(topic = paste0("topic", topic)) %>%
    spread(topic, beta)
  globalProb = rowSums(ap_rel_terms[-1]) / (ncol(ap_rel_terms)-1)
  ap_rel_terms[-1] <- log(ap_rel_terms[-1]) + (lambda - 1) * log(globalProb)
  
  ap_rel_terms %>%
    gather(topic, relevance, -term) %>% 
    group_by(topic) %>% 
    top_n(10, relevance) %>%
    ggplot(aes(term, relevance, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    labs(title="parole nei topic con alta relevance")
}
plot.ap_relevant_terms(ap_topics)
plot.ap_relevant_terms(ap_ctm_topics)

# plot.beta_spread <- function(ap_topics) {
#   beta_spread <- ap_topics %>%
#     mutate(topic = paste0("topic", topic)) %>%
#     spread(topic, beta) %>%
#     filter(topic1 > .001 | topic2 > .001) %>%
#     mutate(log_ratio = log2(topic2 / topic1))
#   
#   beta_spread %>% top_n(20,abs(log_ratio)) %>% mutate(term = reorder(term, log_ratio)) %>%
#     ggplot(aes(term,log_ratio)) + 
#     geom_col()+
#     coord_flip()+
#     labs(title="parole più diverse tra i due topic")
# }
# plot.beta_spread(ap_topics)
# plot.beta_spread(ap_ctm_topics)

## Dimensionality reduction and correlation using Jensen-Shannon divergence
## This function computes the divergence between each couple of topics,
## then projects on a plane using Principal Coordinate Analysis
PCA <- LDAvis::jsPCA(posterior(ap_lda)$terms)
symbols(PCA, circles = sqrt(colMeans(ap_lda@gamma)/60) , inches = FALSE)
text(PCA, col=2)

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

dd <- docs %>% inner_join(ap_documents,by=c("doc_id"="document"))

# distribution of topics
dd %>% group_by(topic) %>% summarise(gamma=sum(gamma)) %>% 
  ggplot(aes(x=factor(topic),y=gamma))+geom_col()

# plot distribution of documents w.r.t a feature (column groupByValue)
data_by_groups<-function(data){
  a<-data%>% group_by(groupByValue) %>% summarise(count=n()) 
  p<-ggplot(a,aes(x=groupByValue,y=count))+geom_col()
  return(list(plot=p,data=a))
}
# plot distribution of topics w.r.t documents feature (column groupByValue)
topic_by_groups<-function(data,total){
  data %>% group_by(groupByValue,topic) %>% summarise(gamma=sum(gamma)) %>% inner_join(total,by="groupByValue") %>% mutate(gamma=gamma/count) %>% 
    ggplot(aes(x=groupByValue,y=gamma,color=factor(topic)))+geom_point()+geom_smooth(se=F)
}

###############################
# for last statement docs
if (doc_name == 'docs_statement') {

# group data by age and plot distribution
dataByGroup <- dd %>% mutate(groupByValue=floor(Age/10)*10)
t<-data_by_groups(dataByGroup)
t$plot
topic_by_groups(dataByGroup,t$data)

# group data by MaleVictim and plot distribution
dataByGroup <- dd %>% mutate(groupByValue=MaleVictim)
t<-data_by_groups(dataByGroup)
t$plot
topic_by_groups(dataByGroup,t$data)

}
###############################
# for election docs
if (doc_name == 'docs_election') {

# group data by folder and plot distribution
dataByGroup<-dd %>% mutate(groupByValue=folder)
t<-data_by_groups(dataByGroup)
t$plot
topic_by_groups(dataByGroup,t$data)

# group data by who and plot distribution
dataByGroup<-dd %>% mutate(groupByValue=who)
t<-data_by_groups(dataByGroup)
t$plot
topic_by_groups(dataByGroup,t$data)

}


# ap_documents %>% ggplot(aes(x=factor(topic),y=gamma))+
#   geom_boxplot()+
#   labs(title="probabilità delle parole dei vari topic")

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
  print(paste("topics:",k))
  for (fold in folds) {
    print(fold)
    m[i,1]<-fold
    m[i,2]<-k
    
    train_dtm<-dtm.new[folding!=fold,]
    test_dtm<-dtm.new[folding==fold,]
    ap_lda <- LDA(train_dtm, k = k, control = list(seed = 1234))
    
    m[i,3]<-perplexity(ap_lda,train_dtm)
    m[i,4]<-evaluate_doc_topics_distribution(ap_lda,train_dtm)
    
    m[i,5]<-perplexity(ap_lda,test_dtm)
    m[i,6]<-evaluate_doc_topics_distribution(ap_lda,test_dtm)
    
    i<-i+1
  }
  df<-data.frame(m)
  write.csv(df,paste0("matrix_cross_validation",k,".csv"))
}

df<-data.frame(m)
write.csv(df,"matrix_cross_validation.csv")

# better rescale entropy based on number of topics
m[,4]<-m[,4]/log(m[,2])

df<-data.frame(m)
colnames(df)<-c("fold","topics","perplexity","entropy")
df <- df %>% mutate(entropy_train=entropy_train/log(topics),entropy_test=entropy_test/log(topics)) %>%
  gather(measure,value,perplexity_train,entropy_train,perplexity_test, entropy_test)

ggplot(df,aes(x=factor(topics),y=value,color=factor(fold)))+
  geom_point()+
  geom_line(aes(group=fold))+
  geom_point(data=df %>% group_by(topics,measure) %>% mutate(value=mean(value)),aes(x=factor(topics),y=value),color="red")+
  facet_wrap(~measure,scales = "free")

ggplot(df %>% group_by(topics,measure) %>% mutate(value=mean(value)),aes(x=factor(topics),y=value))+
  geom_point()+
  geom_line()+
  facet_wrap(~measure,scales = "free")

write.csv(df,"topics_entropy-perplexity")

# evaluate perplexity in a parallel way
## with lda you can specify method VEM or Gibbs (Gibbs seems faster but it gives different results)

library(doParallel)
DTM<-dtm.new
cluster <- makeCluster(detectCores(logical = TRUE) - 1) # leave one CPU spare...
registerDoParallel(cluster)

clusterEvalQ(cluster, {
  library(topicmodels)
})

folds <- 5
splitfolds <- sample(1:folds, 23, replace = TRUE)
candidate_k <- c(2:20) # candidates for how many topics
burnin = 100
iter = 1000
keep = 50

clusterExport(cluster, c("burnin", "iter", "keep", "splitfolds", "folds", "candidate_k"))

# we parallelize by the different number of topics.  A processor is allocated a value
# of k, and does the cross-validation serially.  This is because it is assumed there
# are more candidate values of k than there are cross-validation folds, hence it
# will be more efficient to parallelise
system.time({
  results <- foreach(j = 1:length(candidate_k), .combine = rbind) %dopar%{
    k <- candidate_k[j]
    results_1k <- matrix(0, nrow = folds, ncol = 2)
    colnames(results_1k) <- c("k", "perplexity")
    for(i in 1:folds){
      train_set <- DTM[splitfolds != i , ]
      valid_set <- DTM[splitfolds == i, ]
      
      fitted <- LDA(train_set, k = k, method = "Gibbs", control = list(burnin = burnin, iter = iter, keep = keep) )
      #fitted <- LDA(train_set, k = k)
      results_1k[i,] <- c(k, perplexity(fitted, newdata = valid_set))
    }
    return(results_1k)
  }
})

stopCluster(cluster)

results_df <- as.data.frame(results)

ggplot(results_df, aes(x = k, y = perplexity)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("5-fold cross-validation of topic modeling") +
  labs(x = "Candidate number of topics", y = "Perplexity on the hold-out set")



# better visualization

p<-posterior(ap_lda)

docl  <- apply(dtm.new, 1, sum) #Find the sum of words in each Document
wordf <- apply(dtm.new, 2, sum)
data<-list(phi=p$terms, theta=p$topics, vocab=colnames(p$terms), doc.length=docl, term.frequency=wordf)

json <- createJSON(phi = data$phi, 
                   theta = data$theta, 
                   doc.length = data$doc.length, 
                   vocab = data$vocab, 
                   term.frequency = data$term.frequency)
serVis(json, out.dir = 'vis', open.browser = T)
