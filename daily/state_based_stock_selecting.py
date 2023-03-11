from datetime import datetime, timedelta
from math import ceil

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import yfinance as yf

from best_stock_by_state import get_daily_returns


TOMORROW = (datetime.now() + timedelta(1)).date()
# symbols to exclude (e.g., some are state or municipality specific and cannot
# be purchased externally
ALWAYS_EXCLUDE = ['EP', 'NXN', 'NXP', 'SVA']
MAX_ITER = 10
FIGSIZE = [20, 12]
LABEL_SIZE = 20


# NOTE: dar = 'daily average return'
class StateBasedStockSelector:
    def __init__(
            self, states_path, dar_by_state_path, trans_path, years_of_data=50,
            min_years=10):
        self.states_path = states_path
        self.dar_by_state_path = dar_by_state_path
        self.dar_df = self._get_dar_df()
        self.trans_path = trans_path
        self.transitions = self._get_transitions()
        self.YEARS_OF_DATA = years_of_data
        self.MIN_YEARS = min_years
        self.states = self._get_states_data()
        self.STATES = sorted(self.states.state.unique())
        self.CURRENT_STATE = self.states.state.to_numpy()[-1]
        print('Current state:', self.CURRENT_STATE)
        self.START = (
            TOMORROW - timedelta(int(round(self.YEARS_OF_DATA * 365.25))))
        print('Data start date:', self.START)
        self.iters = 0

    def _get_dar_df(self):
        dar_df = pd.read_csv(self.dar_by_state_path, index_col=0)
        return dar_df
    
    def _get_transitions(self):
        trans = pd.read_csv(self.trans_path)
        print('transitions:')
        print(trans)
        return trans
    
    def _get_states_data(self):
        states = pd.read_csv(self.states_path)
        states = states[['Date', 'state']]
        states.index = pd.to_datetime(states.Date)
        states.drop(columns='Date', inplace=True)
        print('states:', states.tail())
        return states
        
    def get_best_stocks(self, n):
        self._get_state_weighted_dar()
        self._select_stocks(n)
        return self.best_stocks

    def _get_state_weighted_dar(self):
        self.dar_df['weighted'] = 0
        for state, prob in zip(
                self.transitions.next_state, self.transitions.prob):
            self.dar_df['weighted'] += prob * self.dar_df[f'dar_{state}']
        self.dar_df.sort_values('weighted', ascending=False, inplace=True)
                                                 
    def _select_stocks(self, n, exclude=ALWAYS_EXCLUDE, stock_data=None):
        self.iters += 1
        print('Iteration:', self.iters)
        best_weighted = [s for s in self.dar_df.index if s not in exclude][:n]
        print('best weighted:', sorted(best_weighted))
        needed = self._determine_needed_stock_data(
            stock_data, exclude, best_weighted)
        stock_data = self._update_stock_downloads(stock_data, needed)
        exclude += self._exclude_by_overall_performance(stock_data)
        exclude += self._exclude_by_current_state_performance(stock_data)
        exclude = set(exclude)  # in case of duplicates
        if (set(list(stock_data)).intersection(exclude)
            and self.iters < MAX_ITER):
            self._select_stocks(n, list(exclude), stock_data)
        else:
            print('stock data:', stock_data)
            if exclude:
                exclude = [x for x in exclude if x in list(stock_data)]
                stock_data.drop(columns=list(exclude), inplace=True)
            self.best_stocks = sorted(
                [x for x in list(stock_data) if x != 'state'])
            print('best stocks:', self.best_stocks)
            self._plot_stocks(stock_data)
            return #[x for x in list(stock_data) if x != 'state']

    @staticmethod
    def _determine_needed_stock_data(stock_data, exclude, best_weighted):
        if stock_data is not None:
            downloaded = list(stock_data)
            for drop in exclude:
                if drop in downloaded:
                    stock_data.drop(columns=[drop], inplace=True)
            needed = [x for x in best_weighted if x not in downloaded]
        else:
            needed = best_weighted
        return needed

    def _update_stock_downloads(self, stock_data, needed):
        keep_col = needed[0] if len(needed) == 1 else 'AdjClose'
        get = pd.DataFrame(
            (
                yf
                .download(needed, start=self.START, end=TOMORROW)
                .rename(columns={'Adj Close': keep_col})
            )[keep_col])
        get = self._normalize_stock_values(get)
        if stock_data is not None:
            stock_data = pd.concat([stock_data, get], axis=1)
        else:
            stock_data = get
        stock_data.index = pd.to_datetime(stock_data.index)
        stock_data = stock_data.sort_index()
        if 'state' not in list(stock_data):
            stock_data = pd.concat([stock_data, self.states], axis=1)
        return stock_data

    @staticmethod
    def _normalize_stock_values(df):
        '''Normalize by dividing by the first non-null value, so that all will
        represent the value of $1 invested at the start
        '''
        cols = [col for col in list(df) if col != 'state']
        for col in cols:
            first_val = df[col][df[col].notnull()].to_numpy()[0]
            df[col] /= first_val
        return df

    @staticmethod
    def _exclude_by_overall_performance(df):
        exclude = []
        cols = [col for col in list(df) if col != 'state']
        for col in cols:
            if df[col].to_numpy()[-1] <= 1.:
                exclude.append(col)
        print('Excluding due to poor overall performance:', exclude)
        return exclude
    
    def _exclude_by_current_state_performance(self, df):
        exclude = []
        cols = [col for col in list(df) if col != 'state']
        for col in cols:
            current_state_returns = self._get_current_state_returns(df, col)
            if current_state_returns[-1] <= 1.:
                exclude.append(col)
        print('Excluding due to poor performance in current state:', exclude)
        return exclude

    def _get_current_state_returns(self, df, col):
        dr = get_daily_returns(df[col])
        dr = dr[df.state == self.CURRENT_STATE]
        dr = dr[dr.notnull()]
        dr = (dr + 1).to_numpy()
        dr = np.array([1] + list(dr))
        cum_returns = dr.cumprod()
        return cum_returns 

    def _plot_stocks(self, df):
        stocks = [col for col in list(df) if col != 'state']
        self._plot_returns(df, stocks, method='Overall')
        self._plot_returns(df, stocks, method='Current State')

    def _plot_returns(self, df, stocks, method):
        n = len(stocks)
        cols = 8
        rows = ceil(n / cols)
        plt.figure(figsize=FIGSIZE)
        plt.rc('axes', labelsize=LABEL_SIZE)
        for i, stock in enumerate(stocks):
            if method == 'Overall':
                x = df[stock]
                x = x[x.notnull()]
            elif method == 'Current State':
                x = self._get_current_state_returns(df, stock)
            plt.subplot(rows, cols, i + 1)
            plt.plot(x)
            plt.axhline(y=1, color='k')
            plt.xlabel(stock)
            plt.yscale('log')
            if i == 0:
                plt.title(method)
        plt.show()
