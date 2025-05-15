# Load libraries
library(tm)
library(tidytext)
library(dplyr)
library(stringr)
library(Hmisc)
library(readr)

# Set folder path
folder_path <- "mgmt_cleaning_current"

# Read all text files
file_list <- list.files(folder_path, pattern = "\\.txt$", full.names = TRUE)
text_data <- lapply(file_list, readLines)
text_data <- lapply(text_data, paste, collapse = " ")

# Put into a data frame
docs <- data.frame(id = basename(file_list), text = unlist(text_data), stringsAsFactors = FALSE)

# Basic Cleaning
docs_clean <- docs %>%
  mutate(text = str_to_lower(text),                         # lowercase
         text = str_replace_all(text, "[^a-z\\s]", " "),     # remove symbols and numbers
         text = str_squish(text))                            # remove extra spaces

# Remove stopwords
docs_tokens <- docs_clean %>%
  unnest_tokens(word, text) 
#%>%
 # anti_join(stop_words, by = "word")

# Optional: Recombine if needed
docs_final <- docs_tokens %>%
  group_by(id) %>%
  summarise(text = paste(word, collapse = " "))

# Now you have cleaned text ready for sentiment analysis!

docs_clean <- docs_clean %>%
  mutate(
    cik = str_split_fixed(id, "_", 2)[,1],                         # first part before first "_"
    filing_date = str_split_fixed(id, "_", 3)[,3],                 # third part (the date)
    filing_year = str_sub(filing_date, 1, 4)                       # first 4 characters = year
  )

docs_clean <- docs_clean %>%
  select(-c(filing_date, id))    # removes 'filing_date' and 'ext' columns

docs_clean <- docs_clean %>%
  mutate(cik_year = paste(cik, filing_year, sep = ""))



# 2. 
positive_words <- tolower(readLines("positive.txt"))
negative_words <- tolower(readLines("negative.txt"))


# Define negation words
negation_words <- c("no", "not", "none", "neither", "never", "nobody", "without", "hardly", "barely", "rarely")

cybersecurity_words <- c("cyber", "cyberattack", "cyber-attack", "breach", "hacked", "hacking", "hack", "dos", "ddos", "attack", "security", "vulnerability", "compromise", 
                         "data breach", "data-breach", "databreach", "ransomware", "ransom", "phishing", "unauthorized", "virus", "vishing", "webcrawler", "bug", "malware")


# 3. Updated calculate_sentiment() function
calculate_sentiment <- function(words) {
  positive_count <- 0
  negative_count <- 0
  total_words <- length(words)
  
  for (i in seq_along(words)) {
    word <- words[i]
    
    if (word %in% positive_words) {
      window_start <- max(i - 3, 1)
      window_words <- words[window_start:(i-1)]
      
      if (any(window_words %in% negation_words)) {
        negative_count <- negative_count + 1  # flip positive to negative
      } else {
        positive_count <- positive_count + 1
      }
    }
    
    if (word %in% negative_words) {
      negative_count <- negative_count + 1
    }
  }
  
  positive_prop <- ifelse(total_words > 0, positive_count / total_words, 0)
  negative_prop <- ifelse(total_words > 0, negative_count / total_words, 0)
  
  if (positive_prop + negative_prop > 0) {
    net_positive_tone <- (positive_prop - negative_prop) / (positive_prop + negative_prop)
  } else {
    net_positive_tone <- 0
  }
  
  return(data.frame(
    positive_count = positive_count,
    negative_count = negative_count,
    positive_prop = positive_prop,
    negative_prop = negative_prop,
    net_positive_tone = net_positive_tone,
    total_words = total_words
  ))
}


calculate_cyber_mentions <- function(words) {
  sum(words %in% cybersecurity_words)
}

# 5. Tokenize text into words
docs_tokens <- docs_clean %>%
  unnest_tokens(word, text)

# 6. Group words per document
docs_list <- docs_tokens %>%
  group_by(cik_year) %>%
  summarise(words = list(word), .groups = "drop")

# 7. Apply the sentiment function
sentiment_scores <- docs_list %>%
  rowwise() %>%
  mutate(
    sentiment_result = list(calculate_sentiment(words)),
    positive_count = sentiment_result$positive_count,
    negative_count = sentiment_result$negative_count,
    positive_prop = sentiment_result$positive_prop,
    negative_prop = sentiment_result$negative_prop,
    net_positive_tone = sentiment_result$net_positive_tone,
    total_words = sentiment_result$total_words,
    cyber_mentions = calculate_cyber_mentions(words)
  ) %>%
  select(cik_year, positive_count, negative_count, positive_prop, negative_prop,
         net_positive_tone, total_words, cyber_mentions) %>%
  ungroup()



write_csv2(sentiment_scores, "sentiment_current.csv")
