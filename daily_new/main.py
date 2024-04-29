#!/usr/bin/env python3
from datetime import datetime, timedelta
import json
import os
from pprint import pprint

import numpy as np
import pandas as pd

from ask_bid_determination import NextDayMultiplier
import best_stocks_by_sharpe
from holdings_loading import HoldingsLoader
from stock_holdings_updating import StockHoldingsUpdater
from stock_metrics_calculating import StockMetricsCalculator
from transacting import TransactionDeterminer


# Daily inputs:
FID_VALUE =   226345  # [217831, 235694]
ET_VALUE =    176185  # [167274, 184216]
SCHWAB_VALUE = 15480  # [ 14775,  16214]
SIM1_VALUE =   99257 + 10043
SIM2_VALUE =   97989 + 29078
SIM3_VALUE =   98712 + 23070
DM_VALUE   =   20134
FRAC_IN = 0.9608
BEST_SIM = 2    # update weekly (on Fri)

TODAY = datetime.now().date()
TOMORROW = TODAY + timedelta(1)
INDICES = ['^GSPC', '^NYA', '^IXIC']
START = '1965-01-01'
HOME = os.environ['HOME']
DOWNLOADS = f'{HOME}/Downloads'

NEXT_DAY_DISTRIB_WINDOW = 750
PCT_TO_TRADE_DAILY = 0.2
N_STATE_BASED_STOCKS = 100
# increase values if trying to increase prob of on/offloading
P_STATS0_BUY = {
    'et':     {'buy': 0.01, 'sell': 0.04},  # incr by 4
    'fid':    {'buy': 0.01, 'sell': 0.08},  #         4
    'schwab': {'buy': 0.01, 'sell': 0.10},  #         5
    'sim1':   {'buy': 0.99, 'sell': 0.01},  #         3 adelaide 2024
    'sim2':   {'buy': 0.01, 'sell': 0.02},  #         2 aei
    'sim3':   {'buy': 0.01, 'sell': 0.06},  #         6 simsims
    'dm':     {'buy': 0.01, 'sell': 0.01}}  # static

PARAMS = {
    'et': {
        'status_weights': [1.1, 1, 1],   # RSI, fair_value_mult, geomean
        'sharpe_scaled_exp': 3.9,
        'max_prop_per_stock': 0.05,
        # adj how weighted score (on [0, 1] gets converted to (-inf, +inf)
        'scaler': 0.6,                   
        'level': 5},
    'fid': {
        'status_weights': [1, 1.1, 1],
        'sharpe_scaled_exp': 4,
        'max_prop_per_stock': 0.03,
        'scaler': 0.6,
        'level': 5},
    'schwab': {
        'status_weights': [1, 1, 1.1],
        'sharpe_scaled_exp': 4.1,
        'max_prop_per_stock': 0.01,
        'scaler': 0.6,
        'level': 5},
    'sim1': {
        'max_prop_per_stock': 0.1095,
        'sharpe_scaled_exp': 3.1903,
        'status_weights': [3.266, 2.139, 1.0],
        'scaler': 0.6,
        'level': 5},
    'sim2': {
        'max_prop_per_stock': 0.0934,
        'sharpe_scaled_exp': 3.0468,
        'status_weights': [2.838, 2.952, 1.0],
        'scaler': 0.6,
        'level': 5},
    'sim3': {
        'max_prop_per_stock': 0.1068,
        'sharpe_scaled_exp': 3.0592,
        'status_weights': [3.649, 1.0, 1.49],
        'scaler': 0.6,
        'level': 5}}
PARAMS['dm'] = PARAMS['fid']
param_tracker = {
    'max_prop': [
        0.0641, 0.0587, 0.1155, 0.1155, 0.0962, 0.0962, 0.1095],
    'exp': [
        3.7602, 3.629, 3.5379, 3.5379, 3.9815, 3.9815, 3.1903],
    'w_rsi': [
        1.186, 4.160, 5.215, 5.215, 2.661, 2.661, 3.266],
    'w_fairval': [
        1.485, 1.000, 1.000, 1.000, 1.030, 1.030, 2.139],
    'w_geomean': [
        1.000, 6.988, 3.819, 3.819, 1.000, 1.000, 1.000],
    'scaler': [
        0.6,   0.6,   0.6,   0.6,   0.6,   0.6,   0.6],
    'level': [
        5,     5,     5,     5,     5,     5,     5]}

'''
for param, ts in param_tracker.items():
    if param == 'max_prop':
        ts = [t * 100 for t in ts]
    if param == 'exp':
        ts = [t * 3 for t in ts]
        param = '3(exp)'
    if param == 'level':
        ts = [t / 5 for t in ts]
        param = 'level / 5'
    plt.plot(ts, label=param)
plt.legend()
plt.show()
exit()
'''

# File paths
DATA = './data'
CURRENT_STOCKS = f'{DATA}/current_stocks.json'
SHARPES = f'{DATA}/sharpes.csv'
BEST_STOCKS = f'{DATA}/best_stocks.csv'
NEXT_DAY_DISTRIBUTIONS = f'{DATA}/next_day_distributions.csv'
STOCK_METRICS = f'{DATA}/stock_metrics.csv'
TRANSACTIONS = f'{DATA}/transactions.csv'


def main(start='beginning'):
    make_sure_files_downloaded()
    holdings = HoldingsLoader().load()
    current_stocks = load_current_stocks()
    stock_set = set(holdings.index)
    for stock_group in current_stocks.values():
        stock_set |= set(stock_group)
    if start == 'beginning':
        best_stocks = best_stocks_by_sharpe.get_best_stocks(
            outpath=SHARPES, manual_symbols=stock_set)
        best_stocks.to_csv(BEST_STOCKS)
    else:
        best_stocks = pd.read_csv(BEST_STOCKS, index_col=0)
        best_stocks.index = pd.to_datetime(best_stocks.index)
    transactions = (pd.read_csv(TRANSACTIONS, index_col=0))
    # save backup
    transactions.to_csv(TRANSACTIONS.replace('.csv', '_bk.csv'), index=False)
    transactions = update_current_holdings(transactions, holdings)
    current_stocks, transactions = update_current_stocks(
        current_stocks, list(best_stocks.columns), transactions)
    print('\n\nBEGINNING COMPLETE\n\n')
    if start in  ['beggining', 'transactions']:
        get_next_day_distributions(current_stocks)
    next_day_distributions = pd.read_csv(NEXT_DAY_DISTRIBUTIONS)
    print('\n\nTRANSACTIONS COMPLETE\n\n')
    if start in ['beginning', 'transactions', 'metrics']:
        get_stock_metrics(current_stocks)
    print('\n\nMETRICS COMPLETE\n\n')
    stock_metrics = pd.read_csv(STOCK_METRICS, index_col=0)
    stock_metrics = join_metrics_and_transactions(stock_metrics, transactions)
    print_buy_sell_statuses()
    transactions = get_transactions(stock_metrics, next_day_distributions)
    transactions.sort_index().to_csv(TRANSACTIONS)
    print(f'Saved data to {TRANSACTIONS}')
    update_sim_vals()

    
def make_sure_files_downloaded():
    downloads = [x for x in os.listdir(DOWNLOADS) if x.endswith('.csv')]
    n_found = 0
    expected = ['PCRA', 'Portfolio', 'Positions', 'Dongmei']
    for file_start in expected:
        for f in downloads:
            if f.startswith(file_start):
                n_found += 1
                continue
    if n_found != len(expected):
        raise RuntimeError('One of more download missing.')
    sims = [x for x in downloads if x.startswith('Holdings')]
    if len(sims) != 3:
        raise RuntimeError('One or more download missing.')


def load_current_stocks():
    print('Loading current stock lists...')
    with open(CURRENT_STOCKS, 'r') as f:
        current_stocks = json.load(f)
    return current_stocks


def update_current_holdings(transactions, holdings):
    update_cols = holdings.columns
    transactions.drop(columns=update_cols, inplace=True)
    transactions = pd.concat([holdings, transactions], axis=1)
    transactions[update_cols] = transactions[update_cols].fillna(0)
    return transactions


def update_current_stocks(current_stocks, best_stocks, transactions):
    print('Updating current stock lists...')
    current_stocks, transactions = StockHoldingsUpdater(
        current_stocks, best_stocks, transactions
    ).update_current_stocks()
    return current_stocks, transactions


def get_next_day_distributions(current_stocks):
    print('Getting next-day distributions...')
    stocks = ['^GSPC']
    for s in current_stocks.values():
        stocks += s
    next_day_distributions = (
        NextDayMultiplier(sorted(stocks), n_days=NEXT_DAY_DISTRIB_WINDOW)
        .get_next_day_distributions())
    next_day_distributions.to_csv(NEXT_DAY_DISTRIBUTIONS, index=False)
    print(f'Saved next-day distributions to {NEXT_DAY_DISTRIBUTIONS}')


def get_stock_metrics(current_stocks):
    print('Getting stock metrics...')
    stocks = []
    for s in current_stocks.values():
        stocks += s
    metrics = StockMetricsCalculator(
        sorted(stocks),
        years_of_data=10,
    ).get_metrics()
    metrics.to_csv(STOCK_METRICS, index=False)
    print('Metrics saved to', STOCK_METRICS)


def join_metrics_and_transactions(stock_metrics, transactions):
    out = pd.concat(
        [stock_metrics, transactions.drop(columns=stock_metrics.columns)],
        axis=1)
    fill_0 = [
        'et', 'rollover', 'roth', 'simple', 'fid', 'schwab', 'dm', 'sim1',
        'sim2', 'sim3', 'owned', 'inEt', 'inFid']
    fill_1 = ['in_self_managed', 'currentlyActive']
    out[fill_0] = out[fill_0].fillna(0)
    out[fill_1] = out[fill_1].fillna(1)
    return out


def print_buy_sell_statuses(account=None):
    levels = {k: PARAMS[k]['level'] for k in PARAMS}
    for portfolio, data in P_STATS0_BUY.items():
        if account is not None and portfolio != account:
            continue
        print()
        print('-' * 25)
        print(portfolio)
        print('-' * 25)
        for action, prob in data.items():
            thresh, buy_frac, sell_frac = get_threshold(
                levels[portfolio], prob)
            direction = 'above'
            if action == 'sell':
                thresh *= -1
                direction = 'below'
            print(
                f'  {action}: {thresh:.4f} and {direction} '
                f'({buy_frac if action == "buy" else sell_frac:.4f})')
    print()


def get_threshold(max_init, current):
    diff = int(100 * (1 - 0.01))
    probs = np.linspace(0.01, 1, diff + 1)
    thresh = np.linspace(max_init, -5, diff + 1)
    buy_frac = np.linspace(1, 1.25, diff + 1)
    sell_frac = np.linspace(1, 0.75, diff + 1)
    ERR = 0.00001
    for i in range(diff + 1):
        if abs(probs[i] - current) < ERR:
            return thresh[i], buy_frac[i], sell_frac[i]
    raise RuntimeError('Should be unreachable')


def get_transactions(stock_metrics, next_day_distributions):
    print('Determining transactions...')
    determiner = TransactionDeterminer(
        stock_metrics, next_day_distributions, FRAC_IN, P_STATS0_BUY, PARAMS)
    determiner.compile_data()
    for account, amt in zip(
            ['et', 'fid', 'schwab', 'sim1', 'sim2', 'sim3', 'dm'],
            [ET_VALUE, FID_VALUE, SCHWAB_VALUE, SIM1_VALUE, SIM2_VALUE,
             SIM3_VALUE, DM_VALUE]):
        print('\n\n' + '=' * 50)
        print(f'{account.upper()} Transactions')
        print('=' * 50)
        determiner.get_target_amounts(account, amt)
        determiner.get_bid_ask_prices(account)
        determiner.get_n_shares_to_buy_or_sell(account)
        # total dollars that should be invested as of today
        invested_amt = amt * FRAC_IN
        # ideal amount ($) to buy/sell today
        daily_transaction_amt = PCT_TO_TRADE_DAILY * amt
        determiner.list_transactions(
            account, invested_amt, daily_transaction_amt, amt)
        print()
        print_buy_sell_statuses(account)
    return determiner.df


def update_sim_vals():
    n_sims = len([k for k in PARAMS.keys() if k.startswith('sim')])
    sim1 = PARAMS[f'sim{BEST_SIM}']
    out = {'sim1': sim1}
    for i in range(1, n_sims):
        sim = {
            'status_weights': update_status_weights(sim1['status_weights']),
            'sharpe_scaled_exp': update_sharpe_scaled_exp(
                sim1['sharpe_scaled_exp']),
            'max_prop_per_stock': update_max_prop_per_stock(
                sim1['max_prop_per_stock']),
            'scaler': update_scaler(sim1['scaler']),
            'level': update_level(sim1['level'])}
        out[f'sim{i + 1}'] = sim
    pprint(out)

    
def update_status_weights(current):
    a = np.array(current)
    factors = np.random.normal(scale=0.4, loc=1, size=len(current))
    factors = np.clip(factors, 0.01, 2)
    a *= factors
    a /= a.min()
    return list(a.round(3))


def update_sharpe_scaled_exp(current):
    SD = 0.4
    #MIN = 1
    a = current + np.random.normal(scale=SD)
    #a = max(round(a, 4), MIN)
    return round(a, 4)


def update_max_prop_per_stock(current):
    MIN = 0.005
    MAX = 0.2
    SD = 0.02
    prop = current + np.random.normal(scale=SD)
    return max(min(round(prop, 4), MAX), MIN)


def update_scaler(scaler):
    factor = np.random.normal(loc=1, scale=0.1)
    scaler *= factor
    return round(scaler, 4)

def update_level(level):
    factor = np.random.normal(loc=1, scale=0.1)
    level *= factor
    return round(level, 4)


if __name__ == '__main__':
    main('beginning')
    #main('transactions')
    #main('metrics')
    #main('transactions2')
