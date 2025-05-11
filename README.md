# Portifolio_Optimization_Case

## Overview

This project implements portfolio optimization techniques. It finds the optimal asset allocations that maximize the Sharpe ratio (risk-adjusted return) by:

1. Loading stock data from CSV files
2. Generating portfolios with diverse asset allocations
3. Calculating returns, volatility, and Sharpe ratios
4. Identifying the portfolio with the highest risk-adjusted return

## Features

- **Data Loading**: Reads stock data from CSV files with proper date and price parsing
- **Portfolio Generation**: Creates portfolios with constraint-based weight allocations
  - 25 stocks selected from a universe of 30
  - Weights normalized to sum to 1.0
  - Individual stock weights capped at 20%
- **Risk Analysis**: Calculates the returns, covariance matrices and portfolio volatility
- **Performance Optimization**: Implements parallel computation for faster processing

## Installation

### Prerequisites

- GHC (Glasgow Haskell Compiler) 9.4+ 
- Cabal 3.4+

### Option 1: Using DevContainer (Recommended)

This project includes a DevContainer configuration that provides a ready-to-use development environment with all dependencies pre-installed.

1. Ensure you have [Docker](https://www.docker.com/products/docker-desktop) and [VS Code](https://code.visualstudio.com/) with the [Remote - Containers](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers) extension installed.

2. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/portifolio-optimization.git
   cd portifolio-optimization
   ```

3. Open the folder in VS Code and when prompted, click "Reopen in Container" or use the command palette (F1) and select "Remote-Containers: Open Folder in Container".

4. VS Code will build the container and set up the development environment with all dependencies.

### Option 2: Manual Installation

1. Ensure you have GHC and Cabal installed. You can use [GHCup](https://www.haskell.org/ghcup/) to install these tools:

   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
   ```

2. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/portifolio-optimization.git
   cd portifolio-optimization
   ```

3. Build the project using Cabal:
   ```bash
   cabal update
   cabal build
   ```

## Usage

### Preparing Data

The program requires a CSV file containing stock data, which is already available in the `data/` directory, with the following format:

```
Date,Open,High,Low,Close,Ticker
2024-01-01,100.0,105.0,98.0,103.0,AAPL
2024-01-01,200.0,210.0,195.0,205.0,MSFT
...
```

The default expected filename is `dow_jones_all_tickers_2024.csv`, but you can modify this in `Main.hs`.

### Running the Program

1. From the project directory, run:
   ```bash
   cabal run PortifolioOptCabal
   ```

2. For better performance, specify threading options:
   ```bash
   cabal run PortifolioOptCabal -- -- +RTS -N -RTS
   ```
   This uses all available CPU cores for parallel computation.

3. The program will display progress information and finally output the optimal portfolio details.

### Configuration Options

You can modify the following parameters in `Main.hs`:

- `dataPath`: Path to the stock data CSV file
- `walletsPerCombination`: Number of random portfolios to generate per combination
- `riskFreeRate`: The risk-free rate used for Sharpe ratio calculation

## Sample Output

```
Loading stock data...
Loaded 7500 records.
Found 30 unique tickers.
Pre-computing daily returns...
Generating all ticker combinations (25 out of 30)...
Total number of combinations: 142506
Calculating covariance matrix (this might take a while)...
Covariance matrix calculated.

Starting exhaustive evaluation of all combinations...
Processing 1000 random weight allocations per combination.
Progress: 0.00% - Best Sharpe: N/A
Progress: 0.01% - Best Sharpe: 1.2345
Progress: 0.02% - Best Sharpe: 1.3456
...
Progress: 100.00% - Best Sharpe: 1.9876

Exhaustive evaluation complete!

=== OPTIMAL PORTFOLIO BY SHARPE RATIO ===
Best Sharpe ratio: 1.9876
Wallet allocation (complete):
  AAPL: 20.0%
  MSFT: 18.5%
  JNJ: 12.3%
  AMZN: 10.1%
  ...  
  WMT: 0.5%
Total allocation: 100.0%
Number of stocks: 25
Annual return: 24.5%
Annual volatility: 18.2%
Sharpe ratio: 1.9876

The optimal portfolio uses the following tickers:
["AAPL","AMZN","AXP","BA","CAT","CRM","CSCO","CVX","DIS","GS","HD","HON","IBM","INTC","JNJ","JPM","KO","MCD","MMM","MSFT","NKE","PG","TRV","UNH","V"]

Analysis complete.
```

## Project Structure

- `src/DataLoader.hs`: Functions for loading and parsing stock data
- `src/Portfolio.hs`: Portfolio generation and metrics calculation
- `src/Returns.hs`: Return calculation functions
- `src/Optimization.hs`: Portfolio optimization algorithms
- `src/Utilities.hs`: Helper functions
- `app/Main.hs`: Main application entry point