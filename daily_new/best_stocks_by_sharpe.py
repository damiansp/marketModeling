from datetime import datetime, timedelta
import multiprocessing as mp
import os
import pickle
import sys

import numpy as np
import pandas as pd
import yfinance as yf


DATA = './data'
TOMORROW = (datetime.now() + timedelta(1)).date()
KEEP_N = 100
YEARS_OF_DATA = 50
MIN_YEARS = 10
START = str(TOMORROW - timedelta(YEARS_OF_DATA * 365))
print(f'START (init): {START}')
N_JOBS = 8


def get_best_stocks(outpath, manual_symbols=None):
    symbols = load_data()
    symbols |= manual_symbols
    symbols = sorted(list(symbols))
    batches = prepare_batches(symbols)
    print('n batches:', len(batches))
    min_start = get_min_start()

    # multiprocessing
    manager = mp.Manager()
    sharpe_list = manager.list()
    pool = mp.Pool(N_JOBS)
    for batch in batches:
        pool.apply_async(
            process_batch,
            args=(sharpe_list, batch, manual_symbols, min_start))
    pool.close()
    pool.join()

    sharpes = pd.concat(sharpe_list)
    if len(sharpes) > KEEP_N:
        sharpes.sort_values(ascending=False, inplace=True)
        sharpes = sharpes.iloc[:KEEP_N]
    sharpes = pd.DataFrame(sharpes, columns=['sharpe']).sort_index()
    sharpes.to_csv(outpath)
    final_candidates = manual_symbols | set(sharpes.index)
    print('Final candidates:', len(final_candidates), 'START:', START)
    n = 0
    while n == 0:
        data = (
            yf
            .download(final_candidates, start=START, end=TOMORROW)
            .rename(columns={'Adj Close': 'AdjClose'}))['AdjClose']
        data.index = pd.to_datetime(data.index)
        data = data.sort_index()
        n = len(data)
        print('n retrieved:', n)
    return data

    
def load_data():
    with open(f'{DATA}/all_symbols.pkl', 'rb') as f:
        symbols = pickle.load(f)
    return set(symbols)


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


def get_min_start():
    # must have at least 1 year of data
    min_start = TOMORROW - timedelta(MIN_YEARS * 365)
    if min_start.weekday() == 0:
        min_start -= timedelta(2)
    elif min_start.weekday() == 6:
        min_start -= timedelta(1)
    return min_start


def process_batch(sharpes_list, batch, manual_symbols, min_start):
    try:
        print(batch[0], end='\r')
        data = download_data(batch)
        min_start = adjust_min_date(min_start, data.index)
        data = filter_by_min_date(data, manual_symbols, min_start)
        sharpes = get_sharpes(data)
        sharpes_list.append(sharpes)
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
        #missing_last = [
        #    col for col in list(data) if data[col].isnull()[-1]]
        #data.drop(columns=missing_last, inplace=True)
        data.fillna(method='ffill', inplace=True)
        #data.to_csv(f'{data}/tmp/{symbols[0]}_{symbols[-1]}.csv')
        sys.stdout = std_out
        return data
    except BaseException as e:
        print(f'Failed to download data:\n{e}')


def adjust_min_date(min_date, dates):
    while pd.to_datetime(min_date) not in dates:
        min_date -= timedelta(1)
    return min_date


def filter_by_min_date(data, manual_symbols, min_date):
    for col in data.columns:
        if col in manual_symbols:
            continue
        if (data[col][data[col].notnull()].index[0] 
                > pd.to_datetime(min_date)):
            data.drop(columns=[col], inplace=True)
    return data


def get_sharpes(data):
    out = []
    for stock in list(data):
        returns = get_daily_returns(data[stock])
        sharpes = get_sharpe(returns)
        out.append(sharpes)
    return pd.Series(out, index=list(data))
    

def get_daily_returns(x):
    idx = x.index
    n = len(x)
    x = np.array(x)
    returns = x[1:n] / x[0:(n-1)] - 1
    returns = pd.Series(returns, index=idx[1:])
    return returns


def get_sharpe(returns, window=None):
    n = len(returns)
    if window is None:
        window = n
    if len(returns) > window:
        returns = returns[(n - window):n]
    returns = returns[~np.isnan(returns)]
    sharpe = (np.sqrt(252) * returns.mean()) / returns.std()
    return sharpe
