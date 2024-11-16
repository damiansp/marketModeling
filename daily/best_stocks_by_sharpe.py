from datetime import datetime, timedelta
import multiprocessing as mp
import os
import pickle
import sys
from time import sleep

import numpy as np
import pandas as pd
import yfinance as yf

from mod_utils import red


DATA = './data'
TOMORROW = (datetime.now() + timedelta(1)).date()
KEEP_N = 100
YEARS_OF_DATA = 50
MIN_YEARS = 10
START = TOMORROW - timedelta(round(YEARS_OF_DATA * 365.25))
MIN_PRICE = 2.
MAX_ATTEMPTS = 2
N_JOBS = 1
BATCH_SIZE = 100
THROTTLE_S = 2.5


def get_best_stocks(outpath, manual_symbols=None):
    symbols = load_data()
    symbols |= manual_symbols
    symbols = sorted(list(symbols))
    batches = prepare_batches(symbols, batch_size=BATCH_SIZE)
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
    try:
        sharpes.to_csv(f'{DATA}/full_sharpes_list.csv')
    except:
        print('Failed to save sharpes:', type(sharpes))
        print(sharpes)
    if len(sharpes) > KEEP_N:
        sharpes.sort_values(ascending=False, inplace=True)
        sharpes = sharpes.iloc[:KEEP_N]
    sharpes = pd.DataFrame(sharpes, columns=['sharpe']).sort_index()
    sharpes.to_csv(outpath)
    final_candidates = manual_symbols | set(sharpes.index)
    print('Final candidates:', len(final_candidates), 'START:', START)
    n = 0
    attempt = 0
    throttle_seconds = THROTTLE_S
    while n == 0:
        data = (
            yf
            .download(
                final_candidates,
                start=pd.to_datetime(START, utc=True),
                end=pd.to_datetime(TOMORROW, utc=True))
            .rename(columns={'Adj Close': 'AdjClose'}))['AdjClose']
        data.index = pd.to_datetime(data.index)
        data = data.sort_index()
        n = len(data)
        attempt += 1
        sleep(int(round(throttle_seconds)))
        throttle_seconds *= 2.22
        if attempt > MAX_ATTEMPTS:
            print(red(f'\n\n{MAX_ATTEMPTS} failed attempts. Aborting.\n\n'))
            return None
    sleep(THROTTLE_S)
    return data

    
def load_data():
    # file created by ../notebooks/stock_symbol_scrape.py
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
        if data is not None:
            min_start = adjust_min_date(min_start, data.index)
            data = filter_by_min_date(data, manual_symbols, min_start)
            sharpes = get_sharpes(data)
            sharpes_list.append(sharpes)
    except BaseException as e:
        print(f'Unexpected failure for batch {batch[0]}\n{e}')


def download_data(symbols):
    start = START
    if start.weekday() == 5:  # Sat
        start += timedelta(2)
    elif start.weekday() == 6:  # Sun
        start += timedelta(1)
    #std_out = sys.stdout
    #null = open(os.devnull, 'w')
    #sys.stdout = null
    try:
        n = 0
        attempt = 0
        throttle_seconds = THROTTLE_S
        while n == 0:
            data = (
                yf
                .download(
                    symbols,
                    start=pd.to_datetime(START, utc=True),
                    end=pd.to_datetime(TOMORROW, utc=True))
                .rename(columns={'Adj Close': 'AdjClose'}))['AdjClose']
            data.index = pd.to_datetime(data.index)
            data = data.sort_index()
            n = len(data)
            attempt += 1
            sleep(throttle_seconds)
            throttle_seconds *= 2
            if attempt > MAX_ATTEMPTS:
                print(
                    red(
                        f'\n\n{symbols[0]}-{symbols[-1]}: {MAX_ATTEMPTS} '
                        f'failed attempts. Aborting.\n\n'))
                return None
        sleep(THROTTLE_S)
        # drop any cols that are ALL null
        data = data.loc[:, data.isnull().sum() != len(data)]
        print(red(f'{symbols[0]}-{symbols[-1]} (days, stocks): {data.shape}'))
        #missing_last = [
        #    col for col in list(data) if data[col].isnull()[-1]]
        #data.drop(columns=missing_last, inplace=True)
        #print(
        #    'After dropping if last is NA: Shape (days, stocks):', data.shape)
        data = fillna(data)
        #data.fillna(method='ffill', inplace=True)
        if MIN_PRICE is not None and MIN_PRICE > 0:
            cheap = [col for col in list(data) if data[col][-1] < MIN_PRICE]
            data.drop(columns=cheap, inplace=True)
        #print(
        #    'After dropping cheap stocks: Shape (days, stocks):', data.shape)
        #print('\n\nDATE RANGE:', data.index[0], data.index[-1], '\n\n')
        #data.to_csv(f'{data}/tmp/{symbols[0]}_{symbols[-1]}.csv')
        return data
    except BaseException as e:
        print(f'Failed to download data:\n{e}')
    #finally:
    #    null.close()
    #    sys.stdout = std_out

    
def fillna(data):
    terminals = data.iloc[-1, :]
    last_null = terminals[terminals.isnull()].index
    for col in data.columns:
        if col in last_null:
            last_nonnull = data[col][data[col].notnull()].index[-1]
            data.loc[:last_nonnull, col] = (
                data.loc[:last_nonnull, col].fillna(method='ffill'))
        else:
                data[col] = data[col].fillna(method='ffill')
    return data


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
