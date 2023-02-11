from datetime import datetime, timedelta
import json

from ask_bid_determination import NextDayMultiplier
import best_stock_by_state
from hmm_modeling import HMMModler
import pandas as pd
from state_based_stock_selecting import StateBasedStockSelector
from stock_holdings_updating import StockHoldingsUpdater
from stock_metric_calculating import StockMetricsCalculator


TODAY = datetime.now().date()
TOMORROW = TODAY + timedelta(1)
INDICES = ['^GSPC', '^NYA', '^IXIC', '^W5000']
START = '1965-01-01'
DATA = '../data'


def main():
    current_stocks = load_current_stocks()
    #run_hmm_models()
    #best_stock_by_state.main()
    #current_best_stocks = select_state_based_stocks(20)
    current_best_stocks = [
        'AREN', 'AXON', 'BB', 'CHUY', 'CSIQ', 'CUBI', 'DYAI', 'ICPT', 'LFMD',
        'LIXT', 'OPNT', 'PBF', 'PCRX', 'PFIE', 'RFP', 'SSNT', 'TREE',
        'TSLA', 'VTSI', 'WFG']
    buy_stats = pd.read_csv(f'{DATA}/buyStats_csv.csv')
    current_stocks, buy_stats = update_current_stocks(
        current_stocks, current_best_stocks, buy_stats, fid_max=0.3)
    print('Current stocks:')
    print(current_stocks)
    #next_day_distributions = get_next_day_distributions(current_stocks)
    #get_stock_metrics(current_stocks)


def load_current_stocks():
    with open('current_stocks.json', 'rb') as f:
        current_stocks = json.load(f)
    return current_stocks


def run_hmm_models():
    hmm_symbols = {symbol: [START, str(TOMORROW)] for symbol in INDICES}
    model_data_path = f'{DATA}/hmm_mods_indices.pkl'
    modeler = HMMModler(
        hmm_symbols,
        state_range=range(3, 11),
        reps=6,
        max_iter=1000,
        model_data_path=model_data_path)
    modeler.run_models()
    modeler.save_state_data(f'{DATA}/hmm_exp_returns.csv')
    modeler.save_transitions(f'{DATA}/transition_probs.csv')
    modeler.plot_models(last_n_days=500)


def select_state_based_stocks(n):
    selector = StateBasedStockSelector(
        states_path=f'{DATA}/hmm_exp_returns.csv',
        dar_by_state_path=f'{DATA}/dar_by_state.csv',
        trans_path=f'{DATA}/transition_probs.csv')
    current_best_stocks = selector.get_best_stocks(n)
    print('Current best stocks:', current_best_stocks)
    return current_best_stocks


def update_current_stocks(
        current_stocks, current_best_stocks, buy_stats, fid_max):
    current_stocks, buy_stats = StockHoldingsUpdater(
        current_stocks, current_best_stocks, buy_stats, fid_max
    ).update_current_stocks()
    return current_stocks, buy_stats
    

def get_next_day_distributions(current_stocks):
    stocks = ['^GSPC']
    for s in current_stocks.values():
        stocks += s
    next_day_distributions = (
        NextDayMultiplier(sorted(stocks), n_days=60)
        .get_next_day_distributions())
    return next_day_distributions


def get_stock_metrics(current_stocks):
    stocks = []
    for s in current_stocks.values():
        stocks += s
    metrics = StockMetricsCalculator(
        sorted(stocks),
        years_of_data=10,
        states_path=f'{DATA}/hmm_exp_returns.csv',
        trans_path=f'{DATA}/transition_probs.csv'
    ).get_metrics()
    path = f'{DATA}/stock_metrics.csv'
    metrics.to_csv(path, index=False)
    print('metrics saved to', path)
    
    

if __name__ == '__main__':
    main()
