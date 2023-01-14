from datetime import datetime, timedelta
import multiprocessing as mp
import os
import pickle
import sys

import numpy as np
import pandas as pd
import yfinance as yf


DATA = '../data'
TRIM = 0.02
RUN = {
    0: 'all',
    1: 0,  # specific batch
    2: 'random_batch'
}[0]
#MIN_OVERALL_SHARPE = 0.45
MIN_STOCK_PRICE = 1.00

TOMORROW = (datetime.now() + timedelta(1)).date()
YEARS_OF_DATA = 50
MIN_YEARS = 10
START = TOMORROW - timedelta(YEARS_OF_DATA * 365)
N_JOBS = 8


def main():
    symbols, states_df = load_data()
    STATES = sorted(states_df.state.unique())
    CURRENT_STATE = states_df.state.to_numpy()[-1]
    print('Current State:', CURRENT_STATE)
    batches = prepare_batches(symbols)
    run_batches = get_run_batches(batches)
    min_start = get_min_start()
    print('n batches:', len(run_batches))

    # multiprocessing
    manager = mp.Manager()
    dfs = manager.list()
    pool = mp.Pool(N_JOBS)
    for batch in run_batches:
        pool.apply_async(
            process_batch,
            args=(dfs, batch, min_start, STATES, states_df, TRIM))
    pool.close()
    pool.join()

    # recombine
    print('\nTotal DFs:', len(dfs))
    if len(dfs) > 1:
        df = pd.concat(dfs)
    else:
        df = dfs[0]
    outpath = f'{DATA}/dar_by_state.csv'
    df.to_csv(outpath, index=True)
    print(f'Complete. Data written to {outpath}')
        
        
def load_data():
    with open(f'{DATA}/all_symbols.pkl', 'rb') as f:
        symbols = pickle.load(f)
    states = pd.read_csv(f'{DATA}/hmm_exp_returns.csv')
    states = states[['Date', 'state']]
    states.index = pd.to_datetime(states.Date)
    states.drop(columns='Date', inplace=True)
    return symbols, states


def prepare_batches(symbols, batch_size=100):
    n = len(symbols)
    batches = []
    batch_start = 0
    batch_end = min(batch_start + batch_size, n)
    while batch_start < n:
        batches.append(symbols[batch_start:batch_end])
        batch_start = batch_end
        batch_end = min(batch_start + batch_size, n)
    return batches


def get_run_batches(batches):
    if RUN == 'random_batch':
        n_batches = len(batches)
        idx = np.random.randint(0, n_batches)
        run_batches = [batches[idx]]
    elif isinstance(RUN, int):
        run_batches = [batches[RUN]]
    else:
        run_batches = batches
    return run_batches
    

def get_min_start():
    min_start = TOMORROW - timedelta(MIN_YEARS * 365)
    if min_start.weekday() == 0:
        min_start -= timedelta(2)
    elif min_start.weekday() == 6:
        min_start -= timedelta(1)
    return min_start


def process_batch(dfs, batch, min_start, states, states_df, trim=0.02):
    try:
        print(batch[0], end='\r')
        pd.options.mode.chained_assignment = None
        data = download_data(batch)
        min_start = adjust_min_date(min_start, data.index)
        data = filter_by_min_date(data, min_start)
        data = filter_by_min_price(data)
        data = pd.concat([data, states_df], axis=1).loc[data.index, :]
        state_dars = get_daily_annualized_return_by_state(data, states, trim)
        pd.options.mode.chained_assignment = 'warn'
        dfs.append(state_dars)
    except BaseException as e:
        print(f'Unexpected failure for batch {batch[0]}\n{e}')


def download_data(symbols):
    std_out = sys.stdout
    null = open(os.devnull, 'w')
    sys.stdout = null
    try:
        data = (
            yf
            .download(symbols, start=START, end=TOMORROW)
            .rename(columns={'Adj Close': 'AdjClose'}))['AdjClose']
        data.index = pd.to_datetime(data.index)
        data = data.sort_index()
        data = data.loc[:, data.isnull().sum() != len(data)]
        missing_last = [
            col for col in list(data) if data[col].isnull()[-1]]
        data.drop(columns=missing_last, inplace=True)
        sys.stdout = std_out
        return data
    except BaseException as e:
        print(f'Failed to download data:\n{e}')


def adjust_min_date(min_date, dates):
    while pd.to_datetime(min_date) not in dates:
        min_date -= timedelta(1)
    return min_date


def filter_by_min_date(data, min_date):
    for col in data.columns:
        #if np.isnan(data.at[pd.to_datetime(min_date), col]):
        #    data.drop(columns=[col], inplace=True)
        if (
                data[col][data[col].notnull()].index[0] 
                > pd.to_datetime(min_date)):
            data.drop(columns=[col], inplace=True)
    return data


def filter_by_min_price(data):
    for col in data.columns:
        if (data[col][data[col].notnull()].to_numpy()[-1]
            < MIN_STOCK_PRICE):
            data.drop(columns=[col], inplace=True)
    return data


def get_annualized_daily_return(returns, trim=0.02):
    returns = np.array([1] + list((returns[returns.notnull()] + 1).to_numpy()))
    if trim:
        returns = trim_returns(returns, trim)
    amt = returns.cumprod()
    n = len(returns)
    if n > 2:
        daily_return = amt[-1]**(1/max(n - 1, 1))
    else:
        daily_return = 0
    return daily_return


def get_daily_annualized_return_by_state(data, states,trim=0.02):
    output = []
    stocks = [col for col in list(data) if col != 'state']
    for stock in stocks:
        stock_data = []
        returns = get_daily_returns(data[stock])
        annualized_daily_return = get_annualized_daily_return(returns)
        stock_data.append(annualized_daily_return)
        for state in states:
            daily_annualized_return_state = get_annualized_daily_return(
                returns[data.state == state], trim=trim)
            stock_data.append(daily_annualized_return_state)
        output.append(stock_data)
    df_out = pd.DataFrame(
        output,
        columns=['dar'] + [f'dar_{state}' for state in states],
        index=stocks)
    return df_out


def trim_returns(returns, trim):
    lower_q = trim / 2
    upper_q = 1 - lower_q
    qs = np.quantile(returns, q=[lower_q, upper_q])
    returns = returns[((returns > qs[0]) & (returns < qs[1]))]
    return returns


def get_daily_returns(x):
    idx = x.index
    n = len(x)
    x = np.array(x)
    returns = x[1:n] / x[0:(n-1)] - 1
    returns = pd.Series(returns, index=idx[1:])
    return returns


# Unused, but may return to:
def get_sharpe(returns, window=None):
    n = len(returns)
    if window is None:
        window = n
    if len(returns) > window:
        returns = returns[(n - window):n]
    returns = returns[~np.isnan(returns)]
    sharpe = (np.sqrt(252) * returns.mean()) / returns.std()
    return sharpe


def get_sharpes_by_state(data):
    output = []
    stocks = [col for col in list(data) if col != 'state']
    for stock in stocks:
        stock_data = []
        returns = get_daily_returns(data[stock])
        sharpe = get_sharpe(returns)
        stock_data.append(sharpe)
        for state in STATES:
            sharpe_state = get_sharpe(returns[data.state == state])
            stock_data.append(sharpe_state)
        output.append(stock_data)
    df_out = pd.DataFrame(
        output,
        columns=['sharpe'] + [f'sharpe_{state}' for state in STATES],
        index=stocks)
    return df_out



if __name__ == '__main__':
    main()
