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


MAIN_START = ['beginning', 'transactions', 'metrics', 'transactions2'][-1]

# Daily inputs:
FID_VALUE =   239255  # [217831, 239255]
ET_VALUE =    187380  # [167274, 187380]
SCHWAB_VALUE = 16561  # [ 14775,  16774]
SIM1_VALUE =  105539
SIM2_VALUE =  234511
SIM3_VALUE =  242978
SIM4_VALUE =  195854
SIM5_VALUE =  192207
DM_VALUE   =   20134
BEST_SIM = 4  # update weekly (on Fri)
# 1 - 4 wk  # 1 since other
# 2 - 2 wk
# 3 - 0 wk
# 4 - 1 wk 
# 5 - 0 wk

#                     mine,   sp,     nas,    dow,    rus
fracs     = np.array([0.679,  0.3024, 1,      1,      1])
f_weights = np.array([0.225,  0.225,  0.25,   0.15,   0.15])
FRAC_IN = np.dot(fracs, f_weights)
print('FRAC IN:', FRAC_IN)

TODAY = datetime.now().date()
TOMORROW = TODAY + timedelta(1)
INDICES = ['^GSPC', '^NYA', '^IXIC']
START = '1965-01-01'
HOME = os.environ['HOME']
DOWNLOADS = f'{HOME}/Downloads'

NEXT_DAY_DISTRIB_WINDOW = 750
PCT_TO_TRADE_DAILY = 1.
N_STATE_BASED_STOCKS = 100
# increase values if trying to increase prob of on/offloading
P_STATS0_BUY = {
    'et':     {'buy': 0.01, 'sell': 0.20},  # incr by 4
    'fid':    {'buy': 0.01, 'sell': 0.12},  #         4
    'schwab': {'buy': 0.01, 'sell': 0.16},  #         4
    'sim1':   {'buy': 0.01, 'sell': 0.16},  #         4 adelaide 2024
    'sim2':   {'buy': 0.01, 'sell': 0.09},  #         3 aei
    'sim3':   {'buy': 0.01, 'sell': 0.18},  #         6 simsims
    'sim4':   {'buy': 0.01, 'sell': 0.06},  #         2 sim3
    'sim5':   {'buy': 0.01, 'sell': 0.24},  #         8 simz
    'dm':     {'buy': 0.01, 'sell': 0.01}}  # static

PARAMS = {
    'et': {
        'status_weights': [1.1, 1, 1],   # RSI, fair_value_mult, geomean
        'sharpe_scaled_exp': 3.9,
        'max_prop_per_stock': 0.05,
        # adj how weighted score (on [0, 1] gets converted to (-inf, +inf)
        'scaling': {'method': 'tan', 'scaler': 0.6},
        'buy_level': 5,
        'sell_level': 5},
    'fid': {
        'status_weights': [1, 1.1, 1],
        'sharpe_scaled_exp': 4,
        'max_prop_per_stock': 0.03,
        'scaling': {'method': 'tan', 'scaler': 0.6},
        'buy_level': 5,
        'sell_level': 5},
    'schwab': {
        'status_weights': [1, 1, 1.1],
        'sharpe_scaled_exp': 4.1,
        'max_prop_per_stock': 0.01,
        'scaling': {'method': 'tan', 'scaler': 0.6},
        'buy_level': 5,
        'sell_level': 5},
    'sim1': {
        'buy_level': 3.7777,
        'max_prop_per_stock': 0.1297,
        'scaling': {'intercept': 4.509, 'method': 'linear', 'slope': 0.258},
        'sell_level': 6.0425,
        'sharpe_scaled_exp': 3.1456,
        'status_weights': [2.387, 4.744, 1.0]},
    'sim2': {
        'buy_level': 4.1948,
        'max_prop_per_stock': 0.1528,
        'scaling': {'intercept': 4.7018, 'method': 'linear', 'slope': 2.0146},
        'sell_level': 6.9563,
        'sharpe_scaled_exp': 3.2404,
        'status_weights': [2.131, 3.204, 1.0]},
    'sim3': {
        'buy_level': 3.1517,
        'max_prop_per_stock': 0.1304,
        'scaling': {'intercept': -6.663, 'method': 'linear', 'slope': -2.8949},
        'sell_level': 6.308,
        'sharpe_scaled_exp': 3.1343,
        'status_weights': [539.816, 291.02, 1.0]},
    'sim4': {
        'buy_level': 3.4358,
        'max_prop_per_stock': 0.1297,
        'scaling': {'center': 0.5273, 'method': 'quadratic', 'negpos': 1},
        'sell_level': 6.3458,
        'sharpe_scaled_exp': 3.2943,
        'status_weights': [375.035, 549.973, 1.0]},
    'sim5': {
        'buy_level': 3.9154,
        'max_prop_per_stock': 0.1319,
        'scaling': {'center': 0.6335, 'method': 'quadratic', 'negpos': 1},
        'sell_level': 6.6527,
        'sharpe_scaled_exp': 3.6502,
        'status_weights': [542.406, 819.478, 1.0]}}


PARAMS['dm'] = PARAMS['schwab']

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
    if start in  ['beginning', 'transactions']:
        get_next_day_distributions(current_stocks)
    next_day_distributions = pd.read_csv(NEXT_DAY_DISTRIBUTIONS)
    print('\n\nTRANSACTIONS COMPLETE\n\n')
    if start in ['beginning', 'transactions', 'metrics']:
        get_stock_metrics(current_stocks)
    print('\n\nMETRICS COMPLETE\n\n')
    stock_metrics = pd.read_csv(STOCK_METRICS, index_col=0)
    stock_metrics = join_metrics_and_transactions(stock_metrics, transactions)
    for col in holdings.columns:
        stock_metrics[col] = holdings[col]
        stock_metrics[col].fillna(0, inplace=True)
    print_buy_sell_statuses()
    transactions = get_transactions(stock_metrics, next_day_distributions)
    transactions.sort_index().to_csv(TRANSACTIONS)
    print(f'Saved data to {TRANSACTIONS}')
    update_sim_vals()

    
def make_sure_files_downloaded():
    downloads = sorted(
        [x for x in os.listdir(DOWNLOADS) if x.endswith('.csv')])
    print('DOWNLOADS:', DOWNLOADS)
    print('Downloads:', downloads)
    expected = ['PCRA', 'Portfolio_Positions', 'Positions', 'Dongmei']
    for file_start in expected:
        found = False
        for f in downloads:
            if f.startswith(file_start):
                found = True
                continue
        if not found:
            raise RuntimeError(f'File starting with {file_start} not found')
    sims = [x for x in downloads if x.startswith('Holdings')]
    if len(sims) != 5:
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
    buy_levels = {k: PARAMS[k]['buy_level'] for k in PARAMS}
    sell_levels = {k: PARAMS[k]['sell_level'] for k in PARAMS}
    for portfolio, data in P_STATS0_BUY.items():
        if account is not None and portfolio != account:
            continue
        print()
        print('-' * 25)
        print(portfolio)
        print('-' * 25)
        # TODO: clean up -- do not need to get both
        for action, prob in data.items():
            thresh_b, buy_frac_b, sell_frac_b = get_threshold(
                buy_levels[portfolio], prob)
            thresh_s, buy_frac_s, sell_frac_s = get_threshold(
                sell_levels[portfolio], prob)
            direction = 'above'
            if action == 'sell':
                thresh = thresh_s
                thresh *= -1
                buy_frac = buy_frac_s
                sell_frac = sell_frac_s
                direction = 'below'
            else:
                thresh = thresh_b
                buy_frac = buy_frac_b
                sell_frac = sell_frac_b
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
            ['et', 'fid', 'schwab', 'sim1', 'sim2', 'sim3', 'sim4', 'sim5',
             'dm'],
            [ET_VALUE, FID_VALUE, SCHWAB_VALUE, SIM1_VALUE, SIM2_VALUE,
             SIM3_VALUE, SIM4_VALUE, SIM5_VALUE, DM_VALUE]):
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
            'scaling': update_scaling(sim1['scaling']),
            'buy_level': update_level(sim1['buy_level']),
            'sell_level': update_level(sim1['sell_level'])}
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


def update_scaling(scaling):
    method = scaling['method']
    i = ['linear', 'quadratic', 'tan'].index(method)
    ps = np.array([1., 1., 1.])
    ps[i] *= 2
    ps = ps / ps.sum()
    out_method = np.random.choice(['linear', 'quadratic', 'tan'], p=ps)
    if out_method != method:
        scaling = None
    return {
        'linear': update_linear_scaling,
        'quadratic': update_quadratic_scaling,
        'tan': update_tan_scaling
    }[out_method](scaling)


def update_linear_scaling(scaling):
    if scaling is None:
        intercept = round(np.random.uniform(-10, 10), 4)
        slope = round(np.random.uniform(-20, 20), 4)
    else:
        intercept = round(np.random.normal(scaling['intercept'], scale=1), 4)
        slope = round(np.random.normal(scaling['slope'], scale=1), 4)
    return {'method': 'linear', 'intercept': intercept, 'slope': slope}


def update_quadratic_scaling(scaling):
    if scaling is None:
        negpos = np.random.choice([-1, 1])
        center = round(np.random.uniform(0, 1), 4)
    else:
        negpos = scaling['negpos']
        flip = np.random.choice([True, False], p=[0.1, 0.9])
        if flip:
            negpos * -1
        center = round(np.random.normal(scaling['center'], scale=0.1), 4)
    return {'method': 'quadratic', 'negpos': negpos, 'center': center}
                       

def update_tan_scaling(scaling):
    if scaling is None:
        scaler = np.random.normal(0.6, scale=0.05)
    else:
        scaler = scaling['scaler']
        factor = np.random.normal(loc=1, scale=0.1)
        scaler *= factor
    return {'method': 'tan', 'scaler': round(scaler, 4)}


def update_level(level):
    factor = np.random.normal(loc=1, scale=0.1)
    level *= factor
    return round(level, 4)


if __name__ == '__main__':
    main(MAIN_START)
