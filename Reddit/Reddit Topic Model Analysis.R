library(quanteda)
library(tidyverse)
library(data.table)
library(topicmodels)
library(textmineR)
library(anytime)
library(tictoc)
library(flextable)
library(officer)

# Load Reddit pooled data
pooled_reddit <- read.csv(file = "pooled_reddit.csv", stringsAsFactors = F)

# Remove punctuation
pooled_reddit$text <- gsub("\\n|\\t|[[:punct:]]", " ", pooled_reddit$text)

# create random sample of 10% of Reddit data due to computational limitations
sample_pool <- pooled_reddit[sample(nrow(pooled_reddit), size = round(nrow(pooled_reddit) * .1),replace=FALSE), ]

# Create document-term matrix using textmineR
rdtm2 <- CreateDtm(doc_vec = sample_pool$text, # character vector of documents
                   doc_names = sample_pool$link_id, # document names
                   ngram_window = c(1, 2), # minimum and maximum n-gram length
                   stopword_vec = c(tm::stopwords("english"), # stopwords from tm
                                    tm::stopwords("smart")), # this is the default value
                   lower = TRUE, # lowercase - this is the default value
                   remove_punctuation = TRUE, # punctuation - this is the default
                   remove_numbers = T)

# Remove noisy features
tf_pool2 <- TermDocFreq(rdtm2)
rdtm2 <- rdtm2[ , ! stringr::str_detect(colnames(rdtm2),
                                        "(^http)|(_http)|(^https_)|
                                    (^www$)|(_www$)|(^www_)|
                                    (^ðÿ$)|(_ðÿ$)|(^ðÿ_)|
                                    (^aaa)|(_aa$)|(^aa_)|
                                    (^asktransgender$)|(_asktransgender$)|(^asktransgender_)|
                                    [ðãìàâ]") ]

rdtm2 <- rdtm2[ , colSums(rdtm2) > 10] # Only keep features that occur more than 10 times
rdtm2 <- t(rdtm2[ , tf_pool2$term ]) * tf_pool2$idf # weigh documents using tf-idf
rdtm2 <- t(rdtm2)
tf_pool2 <- TermDocFreq(rdtm2)
tf_bigrams2 <- tf_pool2[ stringr::str_detect(tf_pool2$term, "_") , ]

# Calculate the LDA model for Reddit
set.seed(666)
pool_model <- FitLdaModel(dtm = rdtm2,
                          k = 20, # A 20 topic model
                          iterations = 500,
                          burnin = 100,
                          alpha = 0.1,
                          beta = 0.05,
                          optimize_alpha = T,
                          calc_likelihood = T,
                          calc_coherence = T,
                          calc_r2 = T,
                          cpus = 5
)

# evaluate accuracy of model
pool_model$r2

plot(pool_model$log_likelihood, type = "l")

summary(pool_model$coherence)

# get top 10 terms of each topic
pool_model$top_terms <- GetTopTerms(phi = pool_model$phi, M = 10)
head(t(pool_model$top_terms))

# get prevalence of each topic
pool_model$prevalence <- colSums(pool_model$theta) / sum(pool_model$theta) * 100

# prevalence should be proportional to alpha
plot(pool_model$prevalence, pool_model$alpha, xlab = "prevalence", ylab = "alpha")

# naive topic labeling (guesses what the topic is about based on terms)
pool_model$labels <- LabelTopics(assignments = pool_model$theta > 0.05, dtm = rdtm2, M = 1)

head(pool_model$labels)

pool_model$summary <- data.frame(topic = rownames(pool_model$phi),
                                 label = pool_model$labels,
                                 coherence = round(pool_model$coherence, 3),
                                 prevalence = round(pool_model$prevalence,3),
                                 top_terms = apply(pool_model$top_terms, 2, function(x){
                                   paste(x, collapse = ", ")
                                 }),
                                 stringsAsFactors = FALSE)

topic_tables <- pool_model$summary[order(pool_model$summary$coherence, decreasing = T), ][1:20, ]

row.names(topic_tables) <- 1:nrow(topic_tables)

# evalute the highest probable topic for each document using theta
doc_topics <- data.frame(pool_model$theta)
doc_topics$top <- names(doc_topics)[apply(doc_topics, 1, which.max)]

# takes a topic model, n number of documents, and topic number and returns top n text in the topic
top_documents <- function(mod, n, top) {
  doc_top <- data.frame(mod$theta)
  top_doc <- doc_top[with(doc_top, order(-doc_top[,top])),]
  top_doc <- top_doc[1:n,] %>%
    mutate(link_id = rownames(.))
  top_doc <- merge(top_doc, sample_pool[,1:3], by = "link_id")
  top_doc <- top_doc[,c(1,22,23)]
  return(top_doc)
}

# the top 10 documents with the highest theta in topic 12
top_12 <- top_documents(pool_model, 10, 12)

# Create a clean table of all topics and top terms for analysis
reddit_table <- flextable(topic_tables) %>%
  autofit() %>%
  theme_vanilla() %>%
  set_header_labels(label_1 = "biterm label",
                    coherence = "topic coherence",
                    top_terms = "top terms") %>%
  autofit(part = "header")
