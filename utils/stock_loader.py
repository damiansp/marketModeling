from datetime import datetime, timedelta

import numpy as np
import pandas as pd
import yfinance as yf


TODAY = datetime.now().date()


class Loader:
    def __init__(self, symbols, start, end=None, verbose=False):
        '''
        Get and prepare stock data for stock over a specified date range
        Args:
        - symbol 
          (str): Stock ticker symbol (as recognized by Yahoo finance)
          OR
          (array of str): List of ticker symbols
        - start/end (str 'YYYY-MM-DD') start and end dates for stock data. 
            <end> defaults to <TODAY>
        '''
        self.symbols = (
            [symbols] if type(symbols) is str else sorted(symbols))
        self.start = self.str_to_datetime(start)
        self.end = (
            self.str_to_datetime(end) if end is not None
            else TODAY + timedelta(1))
        self.df = None
        self.verbose = verbose

    @staticmethod
    def str_to_datetime(s):
        return datetime.strptime(s, '%Y-%m-%d')

    @property
    def data(self):
        return self.df

    def download(self, append=None):
        df = (
            yf
            .download(self.symbols, start=self.start, end=self.end)
            .drop('Volume',  axis=1)
            .rename(columns={'Adj Close': 'Value'})
            .sort_index())
        if len(self.symbols) == 1:
            df.columns  = pd.MultiIndex.from_tuples(
                [(x, self.symbols[0]) for x in list(df)])
        df['Date'] = df.index
        if append is not None:
            try:
                df = pd.concat([df, append], axis=1)
                self.symbols += list(append['Value'])
            except:
                raise
        n = df.shape[0]
        df.index = range(n)
        df = self._get_derived_columns(df, n)
        self.df = df
        return df

    def _get_derived_columns(self, df, n):
        print('Generating derived columns...')
        for s in self.symbols:
            if self.verbose:
                print(f'{s}...', end=' ')
            if (df.Open[s] == 0).any():
                df = self._fill_missing_open(df, s)
            df['LogValue', s] = np.log(df.Value[s])
            df['IntradayChange', s] = df.Close[s] / df.Open[s]
            df['DayToDayChange', s] = np.nan
            df['OvernightChange', s] = np.nan
            first_value = df.Value[s][df.Value[s].notna()].index[0]
            for day in range(first_value + 1, n):
                df.loc[(day), ('DayToDayChange', s)] = (
                    df.loc[(day), ('Value', s)]
                    / df.loc[(day - 1), ('Value', s)])
                df.loc[(day), ('OvernightChange', s)] = (
                    df.loc[(day), ('Open', s)]
                    / df.loc[(day - 1), ('Close', s)])
        if self.verbose:
            print()
        return df

    @staticmethod
    def _fill_missing_open(df, s):
        no_open = df.Open[s][((df.Open[s] == 0) | df.Open[s].isnull())].index
        no_open = no_open[no_open > 0]
        prev = no_open - 1
        df.loc[(no_open), ('Open', s)] = df.loc[(prev), ('Close', s)]
        if df.loc[0, ('Open', s)] == 0:
            df.loc[0, ('Open', s)] = (
                (df.loc[0, ('High', s)] + df.loc[0, ('Low', s)]) / 2)
        return df


# Test
if __name__ == '__main__':
    symbols = ['TSLA', 'NVDA']
    start = '2005-01-01'
    loader = Loader(symbols, start, verbose=True)
    data = loader.download()
    print(data.head())
    print(loader.data.tail())
    
