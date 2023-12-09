from datetime import datetime, timedelta
import sys

import pandas as pd

sys.path.append('..')
from utils.stock_loader import Loader
from utils.utils import get_macd


TOMORROW = (datetime.now() + timedelta(1)).date()


class NextDayMultiplier:
    '''For a set of stock symbols, get the distribution of next-day values as a
    multiple of current day closing price
    '''
    def __init__(self, stocks, n_days):
        self.stocks = stocks
        self.start = str(TOMORROW - timedelta(n_days))

    def get_next_day_distributions(self):
        self.data = (
            Loader(self.stocks, self.start, str(TOMORROW), verbose=True)
            .download()
            .rename(columns={'Adj Close': 'AdjClose'}))
        distr_dfs = []
        for stock in self.stocks:
            distr_dfs.append(self._get_next_day_distribution(stock))
        distr_df = pd.concat(distr_dfs, axis=1)
        return distr_df

    def _get_next_day_distribution(self, stock_name):
        stock = self._get_stock_data(stock_name)
        macd = get_macd(stock, up_down=True)
        next_high = stock.High.shift(-1)
        next_low = stock.Low.shift(-1)
        high_mult = next_high / stock.Close
        low_mult = next_low / stock.Close
        return pd.DataFrame({
            f'{stock_name}_trend': macd,
            f'{stock_name}_high_mult': high_mult,
            f'{stock_name}_low_mult': low_mult})

    def _get_stock_data(self, stock):
        sub = self.data.loc[:, [x for x in list(self.data) if x[1] == stock]]
        first_val = sub.Value[stock][sub.Value[stock].notnull()].index[0]
        sub = sub.loc[first_val:, :]
        sub.columns = sub.columns.to_series().apply(lambda x: x[0])
        sub.fillna(method='ffill', inplace=True)
        return sub

        
                         
