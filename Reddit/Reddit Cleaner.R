
library(quanteda)
library(tidyverse)
library(data.table)

# Datasets created using Python PSAW reddit API wrapper. Split to accommodate computation cost
mtf <- read.csv(file = "mtf_1619.csv", stringsAsFactors = F)
ftm <- read.csv(file = "ftm_1619.csv", stringsAsFactors = F)
asktrans <- read.csv(file = "at_1619.csv", stringsAsFactors = F)
trans <- read.csv(file = "trans_1619.csv", stringsAsFactors = F)
nb <- read.csv(file = "nb_1619.csv", stringsAsFactors = F)

# Remove unnecessary columns
keep <- c("X", "author", "author_flair_css_class", "author_flair_text", "author_fullname", "body", "created_utc", "gildings", "id", "is_submitter", "link_id", "locked", "no_follow", "parent_id", "permalink", "stickied", "subreddit", "subreddit_id")

# Merge into one dataframe and write into a csv
merged_df <- lapply(list(mtf, ftm, asktrans, trans, nb), function(x) x[,keep]) %>%
  rbindlist()

# Remove all datapoints generated from deleted, bot, or automoderator users
reddit_text <- merged_df[merged_df$author!="[deleted]",] 
reddit_text <- reddit_text[reddit_text$author_fullname !="t2_6l4z3",]
reddit_text <- reddit_text[!(is.na(reddit_text$body) | reddit_text$body==""), ]

reddit_text$body <- gsub("™|€|â|¤|&gt;|ï|ðÿ|ã|©¸", "", reddit_text$body)
reddit_text$created_utc <- anydate(reddit_text$created_utc)
colnames(reddit_text)[6:7] <- c("text", "get_dates")
colnames(reddit_text)[15] <- "href"

# Combining the text from all documents in the thread for topic model robustness
pooled_reddit <- reddit_text %>%
  group_by(link_id) %>%
  summarise(text = paste(text, collapse = ""))
write.csv(pooled_reddit, file = "pooled_reddit.csv")
```
