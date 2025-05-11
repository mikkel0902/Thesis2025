library(quantmod)

# Read in tickers from CSV
ticker_list <- read.csv("id.csv", stringsAsFactors = FALSE)

# Make sure the column is named 'ticker'
tickers <- ticker_list$ticker

# Initialize empty dataframe to store results
ipo_data <- data.frame(Ticker = tickers, IPO_Date = as.Date(NA))

# Loop over each ticker and get IPO date (earliest available price date)
for (t in tickers) {
  cat("Getting data for:", t, "\n")
  tryCatch({
    stock_xts <- suppressWarnings(getSymbols(t, src = "yahoo", from = "1900-01-01", auto.assign = FALSE))
    first_date <- index(stock_xts)[1]
    ipo_data$IPO_Date[ipo_data$Ticker == t] <- first_date
  }, error = function(e) {
    message("Error for ", t, ": ", e$message)
  })
}

# Print or export
print(ipo_data)
write.csv(ipo_data, "ipo_dates_output.csv", row.names = FALSE)

