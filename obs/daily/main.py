#!/usr/bin/env python3
from datetime import datetime, timedelta
import json
from math import ceil
import os

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
FID_VALUE =   195907
ET_VALUE =    142127
SCHWAB_VALUE = 14319
RSI_VALUE =   103027
ADEL_VALUE =   90885
FRAC_IN = 0.70
FID_MAX = 0.00  # max weight to give my picks in fid acct

TODAY = datetime.now().date()
TOMORROW = TODAY + timedelta(1)
INDICES = ['^GSPC', '^NYA', '^IXIC']
START = '1965-01-01'
DATA = '../data_new'
HOME = os.environ['HOME']
DOWNLOADS = f'{HOME}/Downloads'
# Model params
NEXT_DAY_DISTRIB_WINDOW = 750
PCT_TO_TRADE_DAILY = 0.2
N_STATE_BASED_STOCKS = 100
# increase values if trying to increase prob of on/offloading
P_STATS0_BUY = {
    'et':   {'buy': 0.07, 'sell': 0.01},    # incr by 1
    'fid':  {'buy': 0.12, 'sell': 0.01},    #         2
    'schwab': {'buy': 0.01, 'sell': 0.48}}  #         3
TRANSACT_IF = {
    'et': {'curr': 5, 'opp': 5},
    'fid': {'curr': 4, 'opp': 4},
    'schwab': {'curr': 3, 'opp': 3}}


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
    ##########
    stock_metrics = append_current_holdings(stock_metrics)
    print_buy_sell_statuses()
    transactions = get_transactions(
        stock_metrics, next_day_distributions, buy_stats)
    print_buy_sell_statuses()
    save_current_stocks(current_stocks)
    transactions.to_csv(TRANSACTIONS)
    print(f'Saved data to {TRANSACTIONS}')
    td_updated()
    # Extras
    #append_game_data()
    print('\n\n\nDON\'T FORGET TO UPDATE BUY/SELL STATS\n\n')


def load_current_stocks():
    print('Loading current stock lists...')
    with open(CURRENT_STOCKS, 'r') as f:
        current_stocks = json.load(f)
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
        P_STATS0_BUY, TRANSACT_IF)
    determiner.compile_data()
    for account, amt in zip(
            ['et', 'fid', 'schwab'], [ET_VALUE, FID_VALUE, SCHWAB_VALUE]):
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
            account, invested_amt, daily_transaction_amt)
    return determiner.df


def get_threshold(max_init, current):
    diff = int(100 * (1 - 0.01))
    probs = np.linspace(0.01, 1, diff + 1)
    thresh = np.linspace(max_init, -5, diff + 1)
    ERR = 0.00001
    for i in range(diff + 1):
        if abs(probs[i] - current) < ERR:
            return thresh[i]
    raise RuntimeError('Should be unreachable')


def print_buy_sell_statuses():
    for portfolio, data in P_STATS0_BUY.items():
        print()
        print('-' * 25)
        print(portfolio)
        print('-' * 25)
        for action, prob in data.items():
            thresh = get_threshold(TRANSACT_IF[portfolio]['curr'], prob)
            direction = 'above'
            if action == 'sell':
                thresh *= -1
                direction = 'below'
            print(f'  {action}: {thresh} and {direction}')
    print()


def td_updated():
    print('\n\n')
    print('=' * 40)
    print('SCHWAB NEW')
    print('=' * 40)
    df = pd.read_csv(TRANSACTIONS, index_col=0)
    df['schw_sharpe2'] = df.sharpe * (df.sharpe > 0) * df.currentlyActive
    df['schw_norm'] = df.schw_sharpe2 / df.schw_sharpe2.sum()
    df['schw_target'] = SCHWAB_VALUE * df.schw_norm
    df['schw_delt'] = df.schw_target - df.schwab
    df['scaler'] = df.status_scaled.apply(get_status_scaler)
    df['schw_amt'] = df.schw_delt * df.scaler
    df['schw_shares'] = (df.schw_amt / df.schwab_bid_ask).round().astype(int)
    buys = df.schw_shares[df.schw_shares > 0][df.scaler > 0]
    sells = df.schw_shares[df.schw_shares < 0][df.scaler < 0][df.schwab > 0]
    for buy, idx in zip(buys, buys.index):
        print(
            f'BUY {buy:4d} shares of {idx:5s} at '
            f'{df.loc[idx, "schwab_bid_ask"]:7.2f}')
    print()
    for sell, idx in zip(sells, sells.index):
        print(
            f'SELL {-sell:4d} shares of {idx:5s} at '
            f'{df.loc[idx, "schwab_bid_ask"]:7.2f}')
    df.to_csv('~/Desktop/test.csv')


def get_status_scaler(val):
    if val <= -5:
        return -1
    if val <= -4:
        return -0.5
    if val >= 5:
        return 1
    if val >= 4:
        return 0.6
    if val >= 3:
        return 0.4
    if val >= 2:
        return 0.2
    if val >= 1:
        return 0.1
    return 0


def append_game_data():
    df = pd.read_csv(
        TRANSACTIONS, index_col=0
    )[['direction', 'inEt', 'RSI', 'weighted_sharpe', 'status_scaled']]
    rsi_file = f'{DOWNLOADS}/Holdings - Damian Satterthwaite-Phillips.csv'
    adel_file = rsi_file.replace('.', '(1).')
    df = append_file(df, rsi_file, 'rsi_mod')
    df = append_file(df, adel_file, 'adel')
    for field in ['RSI', 'weighted_sharpe', 'status_scaled']:
        df[f'z_{field}'] = get_rescaled_zscore(df[field])
    df.z_RSI = 1 - df.z_RSI  # reverse order
    df['score'] = df.z_RSI * df.z_weighted_sharpe * df.z_status_scaled
    df = get_deltas(df, 'adel', 'z_RSI', ADEL_VALUE)
    df = get_deltas(df, 'rsi_mod', 'score', RSI_VALUE)
    df.to_csv('~/Desktop/game.csv')


def append_file(df, path, name):
    other_df = pd.read_csv(path, index_col=0)[['Value']]
    other_df.Value = other_df.Value.apply(
        lambda x: float(x.strip('$').replace(',', '')))
    other_df.Value.fillna(0., inplace=True)
    df = pd.concat([df, other_df], axis=1)
    df.Value.fillna(0., inplace=True)
    df.rename(columns={'Value': name}, inplace=True)
    return df
    

def get_rescaled_zscore(series):
    mu = series.mean()
    sig = series.std()
    series =  (series - mu) / sig
    series -= series.min()
    series /= series.max()
    return series
    

def get_deltas(df, current_amt, criterion, total):
    PORTFOLIO_SIZE = 20
    df.sort_values(criterion, inplace=True, ascending=False)
    df[criterion][PORTFOLIO_SIZE:] = 0
    df[criterion] *= df.inEt
    df[criterion] /= df[criterion].sum()
    df[f'{current_amt}_target'] = df[criterion] * total
    df[f'{current_amt}_diff'] = df[f'{current_amt}_target'] - df[current_amt]
    return df
    


if __name__ == '__main__':
    main()