library(readr)
library(dplyr)
library(edgar)

# Load your breach filing dataset
filing_list <- read_delim("id.csv", delim = ";") %>%
  mutate(
    cik = as.character(CIK),             
    filing_year = as.numeric(Filing_year)
  ) %>%
  filter(!is.na(cik), !is.na(filing_year)) %>%
  distinct(cik, filing_year)

# Define user agent
useragent <- "mipe20ag@student.cbs.dk"

# Define cyber-related keywords
cyber_words <- c(
  "cyber", "cyberattack", "cyber-attack", "breach", "hacked", "hacking", "hack", "dos", "ddos", "attack", "security", "vulnerability", "compromise", 
  "data breach", "data-breach", "databreach", "ransomware", "ransom", "phishing", "unauthorized", "virus", "vishing", "webcrawler", "bug", "malware"
)


# Prepare result containers
sentiment_results <- list()
keyword_results <- list()

# Loop over each row
for (i in 1:nrow(filing_list)) {
  cik <- filing_list$cik[i]
  year <- filing_list$filing_year[i]
  
  message(paste("🔍 Processing CIK:", cik, "Year:", year))
  
  # Sentiment
  tryCatch({
    sentiment <- getSentiment(
      cik.no = cik,
      form.type = "10-K",
      filing.year = year -1,
      useragent = useragent
    )
    sentiment_results[[length(sentiment_results) + 1]] <- sentiment
  }, error = function(e) {
    message(paste("❌ Sentiment error for", cik, "in", year))
  })
  
  # Keyword search: search the year BEFORE the filing year
  tryCatch({
    keywords <- searchFilings(
      cik.no = cik,
      form.type = "10-K",
      filing.year = year - 1,
      word.list = cyber_words,
      useragent = useragent
    )
    keyword_results[[length(keyword_results) + 1]] <- keywords
  }, error = function(e) {
    message(paste("❌ Keyword search error for", cik, "in", year - 1))
  })
  
  Sys.sleep(1)
}

sentiment_df_prev<- bind_rows(sentiment_results)
keyword_df_prev <- bind_rows(keyword_results)

write_csv(sentiment_df_prev, "sentiment_scores_prev3.csv")
write_csv(keyword_df_prev, "cybersecurity_keyword_hits_prev3.csv")
