library(readr)
library(dplyr)
library(lubridate)
library(quantmod)

# STEP 1: Load  data
main_data <- read_delim("id.csv", delim = ";", locale = locale(date_format = "%d-%m-%Y")) %>%
  mutate(
    id = as.character(id),
    report_trading_day = as.Date(report_trading_day, format = "%d-%m-%Y")
  )

marketcap_data <- read_delim("marketcap.csv", delim = ";", locale = locale(date_format = "%d-%m-%Y")) %>%
  mutate(
    id = as.character(id),
    Trade_Date = as.Date(`Data Date - Dividends`, format = "%d-%m-%Y")
  )

# STEP 2: Get official U.S. trading calendar
getSymbols("SPY", src = "yahoo", from = "2000-01-01", to = Sys.Date(), auto.assign = TRUE)
trading_days <- index(SPY)

# STEP 3: Adjust each report_trading_day to the most recent prior trading day
get_previous_trading_day <- function(date, trading_days) {
  trading_days_before <- trading_days[trading_days < date]
  if (length(trading_days_before) == 0) {
    return(NA)
  } else {
    return(max(trading_days_before))
  }
}

main_data <- main_data %>%
  rowwise() %>%
  mutate(previous_trading_day = get_previous_trading_day(report_trading_day, trading_days)) %>%
  ungroup()

marketcap_data <- marketcap_data %>%
  mutate(
    id = as.character(id),
    Trade_Date = as.Date(`Data Date - Dividends`, format = "%d-%m-%Y")
  ) %>%
  rename(previous_trading_day = Trade_Date)  # <-- key fix

matched_data <- left_join(
  main_data,
  marketcap_data,
  by = c("id", "previous_trading_day")
)

write_csv(matched_data, "marketcapprev.csv")



# Load  main breach/CAR dataset
main_data <- read_delim("id.csv", delim = ";", locale = locale(date_format = "%d-%m-%Y"))

# Ensure 'id' columns are both character type
main_data <- main_data %>%
  mutate(id = as.character(id))


# Merge on 'id' and select relevant columns from marketcap file
merged_data2 <- main_data %>%
  left_join(
    marketcap_data %>%
      select(
        id,
        Ticker = `Ticker Symbol`,
        Previous_date = `Data Date - Dividends`,
        Shares_Outstanding = `Shares Outstanding`,
        Price_close= `Price - Close - Daily`
      ),
    by = "id"
  )

glimpse(merged_data2)


write_csv(merged_data2, "marketcapfinal.csv")


