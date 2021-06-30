import numpy as np
import pandas as pd


def silence_pandas(silence):
    if silence:
        # Block annoying panda's warning about trying to set a value on a slice
        pd.options.mode.chained_assignment = None
    else:
        # Turn warnings back on (default setting)
        pd.options.mode.chained_assignment = 'warn'


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
