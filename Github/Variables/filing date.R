library(readr)
library(dplyr)
library(lubridate)
library(edgar)


# Load your breach data
breaches <- read_delim("id.csv", delim = ";", locale = locale(date_format = "%d-%m-%Y"))
useragent <- "mipe20ag@student.cbs.dk"

# Extract filing year range
breaches <- breaches %>%
  mutate(fiscal_year = year(as.Date(fiscal_year_end)))

results <- list()

for (i in 1:nrow(breaches)) {
  cik <- as.character(breaches$cik[i])
  base_year <- breaches$fiscal_year[i]
  
  for (filing_year in base_year:(base_year + 1)) {
    tryCatch({
      header <- getFilingHeader(
        cik.no = cik,
        form.type = "10-K",
        filing.year = filing_year,
        useragent = useragent
      )
      
      results[[length(results) + 1]] <- tibble(
        cik = cik,
        original_fiscal_year_end = breaches$fiscal_year_end[i],
        relative_year = filing_year - base_year,
        filing_year = filing_year,
        period_of_report = header$period.of.report,
        filing_date = header$filing.date,
        form_type = header$form.type,
        filing_url = header$accession.url
      )
    }, error = function(e) {
      message(paste("❌ No 10-K for", cik, "in", filing_year))
    })
    
    Sys.sleep(0.2)
  }
}

# Combine and export
final_df <- bind_rows(results)
write_csv(final_df, "filings_relative_years_0_and_1.csv")

library(readr)
library(dplyr)
library(edgar)

# Set user agent
useragent <- "mipe20ag@student.cbs.dk"

# Load CSV 
filings_input <- read_delim("id.csv", delim = ";")

# Prepare list to store results
results <- list()

# Loop through each row
for (i in 1:nrow(filings_input)) {
  cik <- as.character(filings_input$cik[i])
  year <- filings_input$filing_year[i]
  
  tryCatch({
    header <- getFilingHeader(
      cik.no = cik,
      form.type = "10-K",
      filing.year = year,
      useragent = useragent
    )
    
    results[[length(results) + 1]] <- tibble(
      cik = cik,
      filing_year = year,
      filing_date = header$date.filed,
      accession_url = header$accession.url,
      form_type = header$form.type
    )
  }, error = function(e) {
    message(paste("❌ No 10-K for CIK", cik, "in", year))
  })
  
  Sys.sleep(1)
}

# Combine and save
final_df <- bind_rows(results)
write_csv(final_df, "10k_filing_dates_official.csv")

