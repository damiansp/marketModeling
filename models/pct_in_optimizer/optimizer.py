from   datetime import datetime, timedelta
import json
from   pprint import pprint

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import yfinance as yf

pd.options.mode.chained_assignment = None


NOW = datetime.now()
ABS_MAX_WINDOW = 1000
MIN_WINDOW = 20
DEFAULT_SD = 1.024
MAX_SD = 1.024


class PercentInOptimizer:
    def __init__(
            self, n_params: int, years_of_data: float, symbol: str):
        self.n_params = n_params
        n_days = int(round(years_of_data * 365.25))
        self.start = str((NOW - timedelta(days=n_days)).date())
        print('Using start date:', self.start)
        self.symbol = symbol
        self._params = {
            'params': {'amounts': None, 'qs': None},
            'window': None,
            'sd': None}
        self.max_window = ABS_MAX_WINDOW # will change if > 1/2 data rows

    @property
    def params(self):
        return self._params

    @params.setter
    def params(self, vals: dict):
        try:
            for k, v in vals.items():
                self._params[k] = v
            assert len(vals['params']['amounts']) == len(vals['params']['qs']),\
                'Amounts and qs must be of the same length'
        except (KeyError, AssertionError):
            print('Params not valid')

    def get_data(self, df=None):
        if df is None:
            df = yf.download(self.symbol, start=self.start)
        else:
            df = pd.read_csv(df, index_col=0)
            df = df[df.index >= self.start]
            df.index = pd.to_datetime(df.index)
        df = df[['Open', 'Close', 'Adj Close']]
        df.rename(columns={'Adj Close': 'Value'}, inplace=True)
        df.loc[:, 'Date'] = df.index.copy()
        df.index = range(df.shape[0])
        df = self._verify_open(df)
        df = self._get_change_values(df)
        self.start_date = df.loc[0, 'Date'] # not always the same as 'start'
        df['LogValue'] = np.log(df.Value)
        init_amount = df.loc[0, 'Close']
        df['Reserve'] = np.nan
        df['Invested'] = np.nan
        df.loc[0, 'Reserve'] = 0
        df.loc[0, 'Invested'] = init_amount
        df.loc[:, 'Total'] = df.Reserve + df.Invested
        self.df = df
        self.n = df.shape[0]
        if self.max_window > self.n / 2:
            self.max_window = self.n // 2
        

    @staticmethod
    def _verify_open(df):
        n = df.index.max()
        if df.loc[n, 'Open'] == 0:
            print("No 'Open' data. Using yesterday's 'Close' value")
            df.loc[n, 'Open'] = df.loc[n - 1, 'Close']
        return df

    @staticmethod
    def _get_change_values(df):
        df['IntradayChange'] = df.Close / df.Open
        df.loc[:, 'DayToDayChange'] = np.nan
        df.loc[:, 'OvernightChange'] = np.nan
        for day in range(1, df.shape[0]):
            df.loc[day, 'DayToDayChange'] = (
                df.copy(deep=True).loc[day, 'Value']
                / df.copy(deep=True).loc[day -1, 'Value'])
            df.loc[day, 'OvernightChange'] = (
                df.copy(deep=True).loc[day, 'Open']
                / df.copy(deep=True).loc[day - 1, 'Close'])
        return df

    def run_sim(self, iters, best_data=None, specified=None):
        best_yet = 0
        if self._params['window'] is None:
            self._init_default_params()
        plt.figure(figsize=[10, 8])
        plt.plot(self.df.Date, self.df.LogValue, 'k-', linewidth=3)
        best_window = self._params['window']
        best_params = self._params['params']
        sd = self._params['sd']
        for i in range(iters):
            if i == 0:
                window = best_window
                params = best_params
            else:
                if specified is None:
                    window, params = self._init_values(window, params)
                else:
                    window, params = self._init_specific_values(
                        window, params, specified)
            self._generate_actions(window, params)
            self._predict_performance()
            total = np.log(self.df.Total.copy())
            plt.plot(self.df.Date, total, alpha=0.2);
            final_value = self.df.Total.tolist()[-1]
            end = '\r'
            if i == 0:
                best_yet = final_value
                end='\n'
            print(f'{i + 1:2d}: Final value: ${final_value:8.2f} '
                  f'(window = {window}; sd = {sd:.3f})', end=end)
            if final_value > best_yet:
                print('\nNew best model!')
                self._print_params(params)
                best_yet = final_value
                best_params = params
                best_window = window
                sd *= 1.5
                sd = min(sd, MAX_SD)
                best_data = self.df.copy()
            else:
                if sd <= 0.002:
                    sd = MAX_SD
                else:
                    sd *= 0.99
                    sd = max(sd, 0.001)
                if best_data is None:
                    best_data = self.df.copy()
        print()
        self._params['window'] = best_window
        self._params['params'] = best_params
        self._params['sd'] = sd
        return best_data

    def _init_default_params(self):
        amounts = [0] * self.n_params
        qs = np.random.uniform(-1., 1., size=self.n_params)
        self._params = {
            'params': {'amounts': amounts, 'qs': qs},
            'window': MIN_WINDOW,
            'sd': DEFAULT_SD}        
        print('Initial params:')
        self._print_params(self._params)
        
    def _init_values(self, window, params, mindiff=0.05):
        amounts = params['amounts']
        qs = params['qs']
        sd = self._params['sd']
        window = int(
            round(
                self._trunc_normal(
                    window,
                    400 * sd,
                    MIN_WINDOW,
                    self.max_window)))
        params = {
            'qs': [self._trunc_normal(mean, sd, exclude0=True) for mean in qs],
            'amounts': [self._trunc_normal(mean, 2 * sd) for mean in amounts]}
        # Don't allow amounts to be all positive or all negative
        amts = np.array(params['amounts'])
        if (amts > 0).all() or (amts < 0).all():
            i = np.random.choice(self.n_params)
            params['amounts'][i] *= -1
        qs = np.array(sorted(params['qs']))
        diffs = qs[1:] - qs[:-1]
        try:
            if (diffs[diffs < mindiff]).any():
                self._init_values(window, params, mindiff)
        except RecursionError:
            return window, params
        return window, params

    def _init_specific_values(
            self, init_window, init_params, specified, mindiff=0.05):
        sd = self._params['sd']
        params = init_params
        if specified['window']:
            window = int(
                round(
                    self._trunc_normal(
                        init_window, 400 * sd, MIN_WINDOW, self.max_window)))
        else:
            window = init_window
        for q in specified['qs']:
            params['qs'][q] = self._trunc_normal(
                params['qs'][q], sd, exclude0=True)
        for amt in specified['amounts']:
            params['amounts'][amt] = self._trunc_normal(
                params['amounts'][amt], sd)
        qs = np.array(sorted(specified['qs']))
        diffs = qs[1:] - qs[:-1]
        try:
            if (diffs[diffs < mindiff]).any():
                self._init_specific_values(window, params, specified, mindiff)
        except RecursionError:
            return init_window, init_params
        return window, params

    def _trunc_normal(
            self, mean, sd, minval=-0.99, maxval=0.99, exclude0=False):
        val = np.random.normal(mean, scale=sd)
        val = max(min(val, maxval), minval)
        if exclude0:
            if -0.01 < val <= 0:
                val = -0.01
            elif 0 <= val < 0.01:
                val = 0.01
        return val

    def _generate_actions(self, window, params):
        '''
        Given a <window> to calculate the MA over, and <params> for 
        <moving_dev_signals()>, return <sp> with an <Actions> field appended, 
        each value for which should be a list of actions (%s in out) each day,
        based on the signal(s) that occurred each day.
        '''
        qs = np.array(params['qs'])
        amounts = np.array(params['amounts'])
        self._get_devs_relative_to_trend(window)
        up_qs = qs[qs >= 0]
        down_qs = -qs[qs < 0]
        up_amounts = amounts[qs >= 0]
        down_amounts = amounts[qs < 0]
        x = self.df.q
        n = len(x)
        self.df.index = range(n)
        self.df['Actions'] = pd.Series([[0] for i in range(n)])
        for i in range(window, n):
            signals = []
            for q, action in zip(up_qs, up_amounts):
                if x[i - 1] < q and x[i] >= q:
                    signals.append(action)
            for q, action in zip(down_qs, down_amounts):
                if x[i - 1] >= q and x[i] < q:
                    signals.append(action)
            signals = signals or [0]
            try:
                self.df.at[i, 'Actions'] = signals
            except BaseException as e:
                print(e)
                print(self.df.at[i, 'Actions'], '<-', signals)

    def _get_devs_relative_to_trend(self, window):
        #ma = np.log(df.loc[:, 'LogValue']).rolling(window=window).mean()
        ma = self.df.loc[:, 'LogValue'].rolling(window=window).mean()
        #df['difference'] = np.log(df['LogValue']) - ma
        self.df['difference'] = self.df['LogValue'] - ma
        qs = list(np.linspace(1, 0, self.n))
        x = [0] * self.n
        for i in self.df.sort_values('difference').index:
            x[i] = qs.pop()
        self.df['q'] = x

    def _predict_performance(self):
        for day in range(1, self.n):
            self._get_next_days_values(day)
            self.df.loc[day, 'Invested'] *= self.df.loc[day, 'IntradayChange']
            self.df.Total = self.df.Reserve + self.df.Invested

    def _get_next_days_values(self, day):
        row = self.df.loc[day - 1, :]
        overnight_change = self.df.loc[day, 'OvernightChange']
        reserve = row['Reserve']
        invested = row['Invested']
        invested *= overnight_change # value at next day open
        money_per_action = 0
        for action in row['Actions']:
            if action > 0: # Buy
                money_per_action = reserve * action
            if action < 0: # Sell
                money_per_action = invested * action    
            reserve -= money_per_action
            invested += money_per_action # value at morning after transactions
        self.df.loc[day, ['Reserve', 'Invested']] = reserve, invested

    def save_params(self, path):
        params = self._params
        for k, v in params['params'].items():
            params['params'][k] = list(v)
        with open(path, 'w') as f:
            json.dump(params, f, indent=4)
        print('Wrote params to', path)

    def load_params(self, path):
        try:
            with open(path, 'r') as f:
                self.params = json.load(f)
            print('Successfully loaded params from', path)
            self._print_params()    
        except:
            print('Failed to load params from', path)
            
    def _print_params(self, params=None):
        if params is None:
            params = self._params
        pprint(params, indent=4)


def plot_results(best_data, start_date=None):
    VIZ_START_DATE = pd.to_datetime('1960-01-01')
    start_date = (pd.to_datetime(start_date) if start_date is not None 
                  else VIZ_START_DATE)
    best_data['fraction_in'] = (best_data.Invested 
                                / (best_data.Invested + best_data.Reserve))
    sub = best_data.loc[best_data.Date >= start_date, :]
    plt.figure(figsize=[10, 4])
    plt.plot(sub.Date, sub.LogValue, 'k-')
    plt.plot(sub.Date, np.log(sub.Total), 'b-')
    plt.figure(figsize=[10, 8])
    plt.subplot(221)
    plt.plot(sub.Date, sub.LogValue, 'k-')
    plt.subplot(222)
    plt.plot(sub.Date, np.log(sub.Total), 'b-')
    plt.subplot(223)
    plt.plot(sub.Date, sub.fraction_in);
    plt.subplot(224)
    plt.plot(sub.Date, np.log(sub.fraction_in));
    percent_in_now = best_data.fraction_in.tolist()[-1]
    print('% in now:', percent_in_now)
    actions_taken = (
        sub.Actions
        .apply(lambda x: sum(x) != 0 if isinstance(x, list) else False)
        .sum())
    print('Actions taken:', actions_taken)
    return percent_in_now
