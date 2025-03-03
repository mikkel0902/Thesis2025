import pandas as pd
import yfinance as yf

# Load your list of organization names
df = pd.read_excel("Ticker 2018.xlsx")  # Assumes a column named "Company"

tickers = []
for company in df["Company"]:
    try:
        stock = yf.Ticker(company)
        ticker = stock.info.get("symbol", "Not Found")
        tickers.append(ticker)
    except:
        tickers.append("Not Found")

df["Ticker"] = tickers
df.to_excel("output_with_tickers.xlsx", index=False)