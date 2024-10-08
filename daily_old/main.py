#!/usr/bin/env python3
from datetime import datetime, timedelta
import json
import os
from pprint import pprint

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from ask_bid_determination import NextDayMultiplier
import best_stock_by_state
from hmm_modeling import HMMModler
from holdings_appending import HoldingsAppender
from state_based_stock_selecting import StateBasedStockSelector
from stock_holdings_updating import StockHoldingsUpdater
from stock_metric_calculating import StockMetricsCalculator
from transacting import TransactionDeterminer

# Order of operations:
# Clear out downloads
# Get updated Nasdaq data
# Run R files
# Download files form brokerages
# Clean files
# Update daily inputs below


# Daily inputs:
FID_VALUE =   224974  # [217831, 235694]
ET_VALUE =    175121  # [167274, 184216]
SCHWAB_VALUE = 15293  # [ 14775,  16214]
SIM1_VALUE =  100031 #+ 100000
SIM2_VALUE =   98996 + 100000
SIM3_VALUE =   99660 + 100000
DM_VALUE   =   20134
FRAC_IN = 0.9849
BEST_SIM = 2    # update weekly (on Fri)
FID_MAX = 0.00  # max weight to give my picks in fid acct

TODAY = datetime.now().date()
TOMORROW = TODAY + timedelta(1)
INDICES = ['^GSPC', '^NYA', '^IXIC']
START = '1965-01-01'
DATA = '../data'
HOME = os.environ['HOME']
DOWNLOADS = f'{HOME}/Downloads'
# Model params
NEXT_DAY_DISTRIB_WINDOW = 750
PCT_TO_TRADE_DAILY = 0.2
N_STATE_BASED_STOCKS = 100
# increase values if trying to increase prob of on/offloading
P_STATS0_BUY = {
    'et':     {'buy': 0.05, 'sell': 0.01},  # incr by 4
    'fid':    {'buy': 0.01, 'sell': 0.05},  #         4
    'schwab': {'buy': 0.01, 'sell': 0.06},  #         5
    'sim1':   {'buy': 0.99, 'sell': 0.01},  #         3 adelaide 2024
    'sim2':   {'buy': 0.78, 'sell': 0.01},  #         2 aei
    'sim3':   {'buy': 0.99, 'sell': 0.01},  #         6 simsims
    'dm':     {'buy': 0.01, 'sell': 0.01}}  # static
PARAMS = {
    'et': {
        'status_weights': [1.1, 1, 1], # RSI, fair_value_mult, geomean
        'weighted_sharpe': False,
        'sharpe_scaled_exp': 3.9,
        'sharpe_adj_status_type': 'w_',  # '' | 'w_' | 'mean_'
        'max_prop_per_stock': 0.05},
    'fid': {
        'status_weights': [1, 1.1, 1],
        'weighted_sharpe': True,
        'sharpe_scaled_exp': 4,
        'sharpe_adj_status_type': '',
        'max_prop_per_stock': 0.03},
    'schwab': {
        'status_weights': [1, 1, 1.1],
        'weighted_sharpe': True,
        'sharpe_scaled_exp': 4.1,
        'sharpe_adj_status_type': 'mean_',
        'max_prop_per_stock': 0.01},
    'sim1': {
        'max_prop_per_stock': 0.1095,
        'sharpe_adj_status_type': '',
        'sharpe_scaled_exp': 3.1903,
        'status_weights': [3.266, 2.139, 1.0],
        'weighted_sharpe': True},
    'sim2': {
        'max_prop_per_stock': 0.0934,
        'sharpe_adj_status_type': 'mean_',
        'sharpe_scaled_exp': 3.0468,
        'status_weights': [2.838, 2.952, 1.0],
        'weighted_sharpe': False},
    'sim3': {
        'max_prop_per_stock': 0.1068,
        'sharpe_adj_status_type': 'mean_',
        'sharpe_scaled_exp': 3.0592,
        'status_weights': [3.649, 1.0, 1.49],
        'weighted_sharpe': True}}
PARAMS['dm'] = PARAMS[f'sim{BEST_SIM}']
param_tracker = {
    'max_prop': [
        0.0641, 0.0587, 0.1155, 0.1155, 0.0962, 0.0962, 0.1095],
    'exp': [
        3.7602, 3.629, 3.5379, 3.5379, 3.9815, 3.9815, 3.1903],
    'w0': [
        1.186, 4.160, 5.215, 5.215, 2.661, 2.661, 3.266],
    'w1': [
        1.485, 1.000, 1.000, 1.000, 1.030, 1.030, 2.139],
    'w2': [
        1.000, 6.988, 3.819, 3.819, 1.000, 1.000, 1.000]}

'''
for param, ts in param_tracker.items():
    if param == 'max_prop':
        ts = [t * 100 for t in ts]
    if param == 'exp':
        ts = [t * 3 for t in ts]
        param = '3(exp)'
    plt.plot(ts, label=param)
plt.legend()
plt.show()
exit()
'''

# File paths
CURRENT_STOCKS = f'{DATA}/current_stocks.json'
DAR_BY_STATE = f'{DATA}/dar_by_state.csv'  # daily average returns
HMM_EXPECTED_RETURNS = f'{DATA}/hmm_exp_returns.csv'
INDEX_HMM_MODS = f'{DATA}/hmm_mods_indices.pkl'
NEXT_DAY_DISTRIBUTIONS = f'{DATA}/next_day_distributions.csv'
STOCK_METRICS = f'{DATA}/stock_metrics.csv'
TRANSACTIONS = f'{DATA}/transactions.csv'
TRANSITION_PROBS = f'{DATA}/transition_probs.csv'
BUY_STATS = TRANSACTIONS


def main():
    make_sure_files_downloaded()
    current_stocks = load_current_stocks()
    run_hmm_models()  ##
    best_stock_by_state.main(outpath=DAR_BY_STATE)  ##
    current_best_stocks = select_state_based_stocks()
    transactions = (
        pd.read_csv(TRANSACTIONS).rename(columns={'Unnamed: 0': 'stock'}))
    # save backup
    transactions.to_csv(TRANSACTIONS.replace('.csv', '_bk.csv'), index=False)
    transactions = append_current_holdings(transactions)
    transactions.to_csv('/tmp/transactions.csv')
    current_stocks, buy_stats = update_current_stocks(
        current_stocks, current_best_stocks, transactions)
    print('Current stocks:')
    print(current_stocks)
    get_next_day_distributions(current_stocks)
    next_day_distributions = pd.read_csv(NEXT_DAY_DISTRIBUTIONS)
    get_stock_metrics(current_stocks)
    stock_metrics = pd.read_csv(STOCK_METRICS, index_col=0)
    stock_metrics = append_current_holdings(stock_metrics)
    print_buy_sell_statuses()
    transactions = get_transactions(
        stock_metrics, next_day_distributions, buy_stats)
    print_buy_sell_statuses()
    save_current_stocks(current_stocks)
    transactions.to_csv(TRANSACTIONS)
    print(f'Saved data to {TRANSACTIONS}')
    print('\n\n\nDON\'T FORGET TO UPDATE BUY/SELL STATS\n\n')
    print('DO NOT BUY: GGE, POWW, PTON, RDFN, XPEV (dm lingerers)')
    update_sim_vals()


def make_sure_files_downloaded():
    downloads = [x for x in os.listdir(DOWNLOADS) if x.endswith('.csv')]
    n_found = 0
    expected = ['PCRA', 'Portfolio', 'Positions']
    for file_start in expected:
        for f in downloads:
            if f.startswith(file_start):
                n_found += 1
                continue
    if n_found != len(expected):
        raise RuntimeError('One of more download missing.')
    sims = [x for x in downloads if x.startswith('Holdings')]
    if len(sims) != 3:
        raise RuntimeError('One of more download missing.')

def load_current_stocks():
    print('Loading current stock lists...')
    DONGMEI_ONLY = ['TECH', 'ULTA', 'UNH']
    with open(CURRENT_STOCKS, 'r') as f:
        current_stocks = json.load(f)
    for stock in DONGMEI_ONLY:
        if stock not in current_stocks['lingerers']:
            current_stocks['lingerers'] = current_stocks['lingerers'] + [stock]
    return current_stocks


def run_hmm_models():
    print('Running Hidden Markov Model...')
    hmm_symbols = {symbol: [START, str(TOMORROW)] for symbol in INDICES}
    modeler = HMMModler(
        hmm_symbols,
        state_range=range(3, 11),
        reps=6,
        max_iter=1000,
        model_data_path=INDEX_HMM_MODS)
    modeler.run_models()
    modeler.save_state_data(HMM_EXPECTED_RETURNS)
    modeler.save_transitions(TRANSITION_PROBS)
    modeler.plot_models(last_n_days=500)


def select_state_based_stocks():
    print('Selecting best stocks for current state...')
    selector = StateBasedStockSelector(
        states_path=HMM_EXPECTED_RETURNS,
        dar_by_state_path=DAR_BY_STATE,
        trans_path=TRANSITION_PROBS)
    current_best_stocks = selector.get_best_stocks(N_STATE_BASED_STOCKS)
    print('Current best stocks:', current_best_stocks)
    return current_best_stocks


def update_current_stocks(current_stocks, current_best_stocks, buy_stats):
    print('Updating current stock lists...')
    current_stocks, buy_stats = StockHoldingsUpdater(
        current_stocks, current_best_stocks, buy_stats, FID_MAX
    ).update_current_stocks()
    return current_stocks, buy_stats


def save_current_stocks(current_stocks):
    with open(CURRENT_STOCKS, 'w') as f:
        current_stocks = json.dump(current_stocks, f)
    print(f'Saved current stocks to {CURRENT_STOCKS}')

    
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
    print('TECH in get_stock_metrics:', 'TECH' in stocks)  ###
    metrics = StockMetricsCalculator(
        sorted(stocks),
        years_of_data=10,
        states_path=HMM_EXPECTED_RETURNS,
        trans_path=TRANSITION_PROBS
    ).get_metrics()
    metrics.to_csv(STOCK_METRICS, index=False)
    print('Metrics saved to', STOCK_METRICS)


def append_current_holdings(stock_metrics):
    print('Appending current holdings data...')
    stock_metrics = HoldingsAppender(stock_metrics).append_holdings()
    return stock_metrics


def get_transactions(stock_metrics, next_day_distributions, buy_stats):
    print('Determining transactions...')
    determiner = TransactionDeterminer(
        stock_metrics, next_day_distributions, buy_stats, FRAC_IN,
        P_STATS0_BUY, PARAMS)
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
    return determiner.df


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


def print_buy_sell_statuses():
    levels = {
        'et': 5,
        'fid': 5,
        'schwab': 5,
        'sim1': 5,
        'sim2': 5,
        'sim3': 5,
        'dm': 5}
    for portfolio, data in P_STATS0_BUY.items():
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


def update_sim_vals():
    n_sims = len([k for k in PARAMS.keys() if k.startswith('sim')])
    sim1 = PARAMS[f'sim{BEST_SIM}']
    out = {'sim1': sim1}
    for i in range(1, n_sims):
        sim = {
            'status_weights': update_status_weights(sim1['status_weights']),
            'weighted_sharpe': (
                sim1['weighted_sharpe'] if np.random.random() <= 0.6
                else not sim1['weighted_sharpe']),
            'sharpe_scaled_exp': update_sharpe_scaled_exp(
                sim1['sharpe_scaled_exp']),
            'sharpe_adj_status_type': update_sharpe_adj_status_type(
                sim1['sharpe_adj_status_type']),
            'max_prop_per_stock': update_max_prop_per_stock(
                sim1['max_prop_per_stock'])}
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
    MIN = 1
    a = current + np.random.normal(scale=SD)
    a = max(round(a, 4), MIN)
    return a


def update_sharpe_adj_status_type(current):
    options = ['', 'w_', 'mean_']
    alts = [opt for opt in options if opt != current]
    n = np.random.rand()
    if n <= 0.5:
        return current
    if n <= 0.75:
        return alts[0]
    return alts[1]


def update_max_prop_per_stock(current):
    MIN = 0.005
    MAX = 0.2
    SD = 0.02
    prop = current + np.random.normal(scale=SD)
    return max(min(round(prop, 4), MAX), MIN)


if __name__ == '__main__':
    main()
    #update_sim_vals()
    
