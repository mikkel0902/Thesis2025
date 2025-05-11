install.packages(c("quantmod", "dplyr", "readr", "lubridate"))
library(quantmod)
library(dplyr)
library(readr)
library(lubridate)

# Example: breach_data.csv with columns 'ticker' and 'report_date'
breaches <- read_delim("id.csv", delim = ";", 
                       col_types = cols(date = col_character()))

breaches$date <- as.Date(breaches$date, format = "%d-%m-%Y")


# Get trading calendar (change dates as needed)
getSymbols("SPY", src = "yahoo", from = "2000-01-01", to = Sys.Date(), auto.assign = TRUE)
trading_days <- index(SPY)

# Function to get next trading day if not already one
adjust_to_trading_day <- function(date, trading_days) {
  if (date %in% trading_days) {
    return(date)
  } else {
    return(min(trading_days[trading_days > date]))
  }
}

# Apply to your data
breaches <- breaches %>%
  mutate(adjusted_event_date = sapply(date, adjust_to_trading_day, trading_days = trading_days))

breaches$adjusted_event_date <- as.Date(breaches$adjusted_event_date, origin = "1970-01-01")


print(head(breaches))

write.csv(breaches, "adjusted_breach_dates2.csv", row.names = FALSE)
