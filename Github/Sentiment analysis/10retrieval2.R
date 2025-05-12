library(readr)
library(dplyr)
library(edgar)

useragent = "mikkel.rmp@gmail.com"


# Load your breach filing dataset
filing_list <- read_delim("id.csv", delim = ";") %>%
  mutate(
    cik = as.character(cik),              # CIK must be string (no padding)
    filing_year = as.numeric(filing_year)
  ) %>%
  filter(!is.na(cik), !is.na(filing_year)) %>%
  distinct(cik, filing_year)


# Loop over each row
for (i in 1:nrow(filing_list)) {
  cik <- filing_list$cik[i]
  year <- filing_list$filing_year[i]
  
  message(paste("🔍 Processing CIK:", cik, "Year:", year))
  
  # Try to download MD&A and save to file
  tryCatch({
    mdna_text <- getFilingsHTML(
      cik.no = cik,
      form.type = '10-K',
      filing.year = year,
      useragent = useragent
    )
    
  }, error = function(e) {
    message(paste("⚠️ Failed for CIK:", cik, "Year:", year, "Reason:", e$message))
  })
}


