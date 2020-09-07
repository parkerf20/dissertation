tic("create sample dtm")
pdtm <- CreateDtm(doc_vec = during_press$text, # character vector of documents
                  doc_names = during_press$X, # document names
                  ngram_window = c(1, 2), # minimum and maximum n-gram length
                  stopword_vec = c(tm::stopwords("english"), # stopwords from tm
                                   tm::stopwords("smart")), # this is the default value
                  lower = TRUE, # lowercase - this is the default value
                  remove_punctuation = TRUE, # punctuation - this is the default
                  remove_numbers = T)

pdtm <- pdtm[ , ! stringr::str_detect(colnames(pdtm), "[^a-z_]") ]
tf_press <- TermDocFreq(pdtm)
pdtm <- t(pdtm[ , tf_press$term ]) * tf_press$idf
pdtm <- t(pdtm)

pdtm <- pdtm[ , colSums(pdtm) > 5]

tf_press <- TermDocFreq(pdtm)
tf_bigrams_press <- tf_press[ stringr::str_detect(tf_press$term, "_") , ]

toc(log = T)


tic("all models")
set.seed(420)
press_model <- FitLdaModel(dtm = pdtm,
                           k = 20, # should be 11
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
toc(log = T)

# evaluate model

press_model$r2

plot(press_model$log_likelihood, type = "l")

summary(press_model$coherence)

hist(press_model$coherence,
     col = "blue",
     main = "Histogram of probabilistic coherence")

# get top terms of each topic
press_model$top_terms <- GetTopTerms(phi = press_model$phi, M = 10)

# get prevalence of each topic
press_model$prevalence <- colSums(press_model$theta) / sum(press_model$theta) * 100

# prevalence should be proportional to alpha
plot(press_model$prevalence, press_model$alpha, xlab = "prevalence", ylab = "alpha")

# naive topic labeling
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

HRC_table <- flextable(topic_tables_press) %>%
  autofit() %>%
  theme_vanilla() %>%
  set_header_labels(label_1 = "biterm label",
                    coherence = "topic coherence",
                    top_terms = "top terms") %>%
  autofit(part = "header")
HRC_table


top_documents_press <- function(mod, n, top) {
  # takes a topic model, n number of documents, and topic number and returns top n text in the topic
  doc_top <- data.frame(mod$theta)
  top_doc <- doc_top[with(doc_top, order(-doc_top[,top])),]
  top_doc <- top_doc[1:n,] %>%
    mutate(link_id = rownames(.))
  top_doc <- merge(press_text, top_doc, by.x = "X", by.y = "link_id")
  return(top_doc[,1:4])
}

top_doc <- top_documents_press(press_model, 1, 4)
