#!/usr/bin/env python3
from datetime import datetime, timedelta
import json

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
FID_VALUE = 213398
ET_VALUE = 150891
TDAM_VALUE = 15850
FRAC_IN = 0.4900
FID_MAX = 0.00  # max weight to give my picks in fid acct
RSI_VALUE = 113304
ADEL_VALUE = 101336

TODAY = datetime.now().date()
TOMORROW = TODAY + timedelta(1)
INDICES = ['^GSPC', '^NYA', '^IXIC']#, '^W5000']
START = '1965-01-01'
DATA = '../data'
DOWNLOADS = '/Users/damiansp/Downloads'
# Model params
NEXT_DAY_DISTRIB_WINDOW = 750
PCT_TO_TRADE_DAILY = 1  #0.2
# increase values if trying to increase prob of on/offloading
P_STATS0_BUY = {'et': 0.01, 'fid': 0.01, 'tdam': 0.01}
TRANSACT_IF = {
    'et': {'curr': 3, 'opp': 4},
    'fid': {'curr': 1, 'opp': 2},
    'tdam': {'curr': 2, 'opp': 3}}

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
    #run_hmm_models()
    best_stock_by_state.main()
    current_best_stocks = select_state_based_stocks(20)
    #current_best_stocks = 
    transactions = (
        pd.read_csv(TRANSACTIONS).rename(columns={'Unnamed: 0': 'stock'}))
    # save backup
    transactions.to_csv(TRANSACTIONS.replace('.csv', '_bk.csv'), index=False)
    transactions = append_current_holdings(transactions)
    current_stocks, buy_stats = update_current_stocks(
        current_stocks, current_best_stocks, transactions)
    print('Current stocks:')
    print(current_stocks)
    get_next_day_distributions(current_stocks)
    next_day_distributions = pd.read_csv(NEXT_DAY_DISTRIBUTIONS)
    get_stock_metrics(current_stocks)
    stock_metrics = pd.read_csv(STOCK_METRICS, index_col=0)
    stock_metrics = append_current_holdings(stock_metrics)
    transactions = get_transactions(
        stock_metrics, next_day_distributions, buy_stats)
    save_current_stocks(current_stocks)
    transactions.to_csv(TRANSACTIONS)
    print(f'Saved data to {TRANSACTIONS}')
    td_updated()
    # Extras
    append_game_data()


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


def select_state_based_stocks(n):
    print('Selecting best stocks for current state...')
    selector = StateBasedStockSelector(
        states_path=HMM_EXPECTED_RETURNS,
        dar_by_state_path=DAR_BY_STATE,
        trans_path=TRANSITION_PROBS)
    current_best_stocks = selector.get_best_stocks(n)
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
            ['et', 'fid', 'tdam'], [ET_VALUE, FID_VALUE, TDAM_VALUE]):
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


def td_updated():
    print('\n\n')
    print('=' * 40)
    print('TDAM NEW')
    print('=' * 40)
    df = pd.read_csv(TRANSACTIONS, index_col=0)
    df['td_sharpe2'] = df.sharpe * (df.sharpe > 0) * df.currentlyActive
    df['td_norm'] = df.td_sharpe2 / df.td_sharpe2.sum()
    df['td_target'] = TDAM_VALUE * df.td_norm
    df['td_delt'] = df.td_target - df.tdam
    df['scaler'] = df.status_scaled.apply(get_status_scaler)
    df['td_amt'] = df.td_delt * df.scaler
    df['td_shares'] = (df.td_amt / df.tdam_bid_ask).round().astype(int)
    buys = df.td_shares[df.td_shares > 0][df.scaler > 0]
    sells = df.td_shares[df.td_shares < 0][df.scaler < 0][df.tdam > 0]
    for buy, idx in zip(buys, buys.index):
        print(
            f'BUY {buy:4d} shares of {idx:5s} at '
            f'{df.loc[idx, "tdam_bid_ask"]:7.2f}')
    print()
    for sell, idx in zip(sells, sells.index):
        print(
            f'SELL {-sell:4d} shares of {idx:5s} at '
            f'{df.loc[idx, "tdam_bid_ask"]:7.2f}')
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
