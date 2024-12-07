import numpy as np
import pandas as pd


def get_macd(stock_data, fast=60, slow=90, signal=80, up_down=False):
    '''
    Args
    - up_down (bool): if true returns binary (1: up, 0: down) else provides a
        numeric value
    '''
    df = stock_data.copy()
    exp1 = df.Value.ewm(span=fast, adjust=False).mean()
    exp2 = df.Value.ewm(span=slow, adjust=False).mean()
    macd = exp1 - exp2
    exp3 = macd.ewm(span=signal, adjust=False).mean()
    histo = macd - exp3
    if up_down:
        hdiff = histo.diff(1)
        is_rising = 1 * (hdiff > 0)
        return is_rising
    return histo


def get_sharpe_ratio(returns, window=None):
    ANNUAL_MARKET_DAYS = 252
    returns = returns[returns.notna()]
    n = len(returns)
    if window is None:
        window = n
    if n > window:
        returns = returns[n - window:n]
    ratio = (np.sqrt(ANNUAL_MARKET_DAYS) * returns.mean()) / returns.std()
    return ratio


def get_daily_returns(x):
    '''
    If x is the series of values, returns the daily returns as pct change
    '''
    x = np.array(x)
    return pd.Series(x[1:] / x[:-1] - 1)


def sharpe_from_daily(x):
    returns = get_daily_returns(x)
    sharpe = get_sharpe_ratio(returns)
    return sharpe


def td2yf(df):
    'Convert twelvedata format to yfinance format'
    df.drop(columns=['volume'], inplace=True)
    df['Value'] = df.close.copy()
    symbols = sorted(list({i[0] for i in df.index}))
    dfs = [df.loc[sym] for sym in symbols]
    for sym, df in zip(symbols, dfs):
        df.columns = pd.MultiIndex.from_tuples(
            [(col, sym) for col in ['Open', 'High', 'Low', 'Close', 'Value']])
    return pd.concat(dfs, axis=1).astype(np.float64)


def silence_pandas(silence):
    if silence:
        # Block annoying panda's warning about trying to set a value on a slice
        pd.options.mode.chained_assignment = None
    else:
        # Turn warnings back on (default setting)
        pd.options.mode.chained_assignment = 'warn'
