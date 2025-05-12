import yfinance as yf
import pandas as pd
from datetime import datetime

# List of all current Dow Jones tickers
tickers = ["UNH", "GS", "MSFT", "HD", "V", "SHW", "MCD", "CAT", "AMGN", "AXP", 
           "TRV", "CRM", "IBM", "JPM", "AAPL", "HON", "AMZN", "PG", "BA", "JNJ", 
           "CVX", "MMM", "NVDA", "WMT", "DIS", "MRK", "KO", "CSCO", "NKE", "VZ"]

# Set the date range
start_date = "2024-01-08"
end_date = "2024-12-31"

# Initialize an empty list to store dataframes
all_dfs = []

# Download data for each ticker
for ticker in tickers:
    print(f"Downloading data for {ticker}...")
    ticker_data = yf.download(ticker, start=start_date, end=end_date)
    
    # Reset index to make Date a column
    ticker_data = ticker_data.reset_index()
    
    # Handle multi-level columns if they exist
    if isinstance(ticker_data.columns, pd.MultiIndex):
        # Flatten column names by keeping only the first level
        ticker_data.columns = [col[0] if isinstance(col, tuple) else col for col in ticker_data.columns]
    
    # Select only needed columns and ensure proper column names
    ticker_data = ticker_data[['Date', 'Open', 'High', 'Low', 'Close']]
    
    # Add ticker column
    ticker_data['Ticker'] = ticker
    
    # Append to our list
    all_dfs.append(ticker_data)

# Combine all dataframes into one
combined_data = pd.concat(all_dfs, ignore_index=True)

# Display the first few rows and column structure
print("Column names in order:")
print(list(combined_data.columns))
print("\nFirst few rows:")
print(combined_data.head())

# Save to CSV
combined_data.to_csv("dow_jones_all_tickers_2024.csv", index=False)

print(f"Data saved to dow_jones_all_tickers_2024.csv")