from datetime import datetime, timedelta
import json

import pandas as pd

from ask_bid_determination import NextDayMultiplier
import best_stock_by_state
from hmm_modeling import HMMModler
from holdings_appending import HoldingsAppender
from state_based_stock_selecting import StateBasedStockSelector
from stock_holdings_updating import StockHoldingsUpdater
from stock_metric_calculating import StockMetricsCalculator
from transacting import TransactionDeterminer

# Daily inputs:
ET_VALUE = 147_197
FID_VALUE = 203_374
TDAM_VALUE = 10_474
FRAC_IN = 0.6800

TODAY = datetime.now().date()
TOMORROW = TODAY + timedelta(1)
INDICES = ['^GSPC', '^NYA', '^IXIC', '^W5000']
START = '1965-01-01'
DATA = '../data'
# Model params
FID_MAX = 0.3
NEXT_DAY_DISTRIB_WINDOW = 120
PCT_TO_TRADE_DAILY = 0.05

# File paths
BUY_STATS = f'{DATA}/buyStats_csv.csv' # to be replaced by transactions
CURRENT_STOCKS = f'{DATA}/current_stocks.json'
DAR_BY_STATE = f'{DATA}/dar_by_state.csv'  # daily average returns
HMM_EXPECTED_RETURNS = f'{DATA}/hmm_exp_returns.csv'
INDEX_HMM_MODS = f'{DATA}/hmm_mods_indices.pkl'
NEXT_DAY_DISTRIBUTIONS = f'{DATA}/next_day_distributions.csv'
STOCK_METRICS = f'{DATA}/stock_metrics.csv'
TRANSACTIONS = f'{DATA}/transactions.csv'
TRANSITION_PROBS = f'{DATA}/transition_probs.csv'


def main():
    current_stocks = load_current_stocks()
    #run_hmm_models()
    #best_stock_by_state.main()
    #current_best_stocks = select_state_based_stocks(20)
    current_best_stocks = ['AREN', 'AXON', 'BB', 'CHUY', 'CSIQ', 'CUBI', 'DYAI', 'FSM', 'LFMD', 'LVS', 'OPNT', 'OSTK', 'PBF', 'PFIE', 'SBSW', 'SSTK', 'TSLA', 'URBN', 'VTSI', 'WYNN']
    buy_stats = pd.read_csv(BUY_STATS)
    current_stocks, buy_stats = update_current_stocks(
        current_stocks, current_best_stocks, buy_stats)
    print('Current stocks:')
    print(current_stocks)
    #get_next_day_distributions(current_stocks)
    next_day_distributions = pd.read_csv(NEXT_DAY_DISTRIBUTIONS)
    print(f'next day distrib:\n{next_day_distributions.head()}')
    #get_stock_metrics(current_stocks)
    stock_metrics = pd.read_csv(STOCK_METRICS, index_col=0)
    stock_metrics = append_current_holdings(stock_metrics)
    get_transactions(stock_metrics, next_day_distributions, buy_stats)


def load_current_stocks():
    print('Loading current stock lists...')
    with open(CURRENT_STOCKS, 'rb') as f:
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
        stock_metrics, next_day_distributions, buy_stats, FRAC_IN)
    determiner.compile_data()
    for account, amt in zip(
            ['et', 'fid', 'tdam'], [ET_VALUE, FID_VALUE, TDAM_VALUE]):
        determiner.get_target_amounts(account, amt)
        determiner.get_bid_ask_prices(account)
        determiner.get_n_shares_to_buy_or_sell(account)
        determiner.list_transactions(account, PCT_TO_TRADE_DAILY * amt)
        
if __name__ == '__main__':
    main()
