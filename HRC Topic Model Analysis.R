library(quanteda)
library(tidyverse)
library(data.table)
library(topicmodels)
library(textmineR)
library(anytime)
library(tictoc)
library(flextable)
library(officer)

# Load HRC preprocessed file
press_text <- read.csv(file = "press_text.csv", stringsAsFactors = F)

# Create the document-term matrix (dtm) for HRC
pdtm <- CreateDtm(doc_vec = during_press$text, # character vector of documents
                  doc_names = during_press$X, # document names
                  ngram_window = c(1, 2), # minimum and maximum n-gram length
                  stopword_vec = c(tm::stopwords("english"), # stopwords from tm
                                   tm::stopwords("smart")), # this is the default value
                  lower = TRUE, # lowercase - this is the default value
                  remove_punctuation = TRUE, # punctuation - this is the default
                  remove_numbers = T)

# Filer out noisy features and documents
pdtm <- pdtm[ , ! stringr::str_detect(colnames(pdtm), "[^a-z_]") ]
tf_press <- TermDocFreq(pdtm)
pdtm <- t(pdtm[ , tf_press$term ]) * tf_press$idf
pdtm <- t(pdtm)

pdtm <- pdtm[ , colSums(pdtm) > 5]

# Create a list of document terms and biterms from the press dtm
tf_press <- TermDocFreq(pdtm)
tf_bigrams_press <- tf_press[ stringr::str_detect(tf_press$term, "_") , ]

# Calculate the LDA model for HRC
set.seed(420)
press_model <- FitLdaModel(dtm = pdtm,
                           k = 20, # a topic model with 20 topics
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

# Evaluate accuracy of model using R2 and topic coherence
press_model$r2

plot(press_model$log_likelihood, type = "l") # plot likelihood

summary(press_model$coherence)

hist(press_model$coherence,
     col = "blue",
     main = "Histogram of probabilistic coherence")

# get top ten terms of each topic
press_model$top_terms <- GetTopTerms(phi = press_model$phi, M = 10)

# get prevalence of each topic
press_model$prevalence <- colSums(press_model$theta) / sum(press_model$theta) * 100

# prevalence should be proportional to alpha
plot(press_model$prevalence, press_model$alpha, xlab = "prevalence", ylab = "alpha")

# naive topic labeling (guesses what the topic is about based on terms)
press_model$labels <- LabelTopics(assignments = press_model$theta > 0.05, dtm = pdtm, M = 1)

press_model$summary <- data.frame(topic = rownames(press_model$phi),
                                  label = press_model$labels,
                                  coherence = round(press_model$coherence, 3),
                                  prevalence = round(press_model$prevalence,3),
                                  top_terms = apply(press_model$top_terms, 2, function(x){
                                    paste(x, collapse = ", ")
                                  }),
                                  stringsAsFactors = FALSE)

topic_tables_press <- press_model$summary[order(press_model$summary$coherence, decreasing = T), ][1:20, ]

# Create a clean table of all topics and top terms for analysis
HRC_table <- flextable(topic_tables_press) %>%
  autofit() %>%
  theme_vanilla() %>%
  set_header_labels(label_1 = "biterm label",
                    coherence = "topic coherence",
                    top_terms = "top terms") %>%
  autofit(part = "header")
HRC_table
