from datetime import datetime, timedelta
import multiprocessing as mp

import numpy as np
import pandas as pd
import yfinance as yf


TOMORROW = (datetime.now() + timedelta(1)).date()
N_JOBS = 20


class StockMetricsCalculator:
    def __init__(
            self, stocks, years_of_data, states_path, trans_path,
            macd_params=(60, 90, 80), rsi_window=14, lmb=0.9):
        self.stocks = stocks
        self.start = TOMORROW - timedelta(int(round(years_of_data * 365.25)))
        self.states = self._get_states(states_path)
        self.trans = pd.read_csv(trans_path)
        self.STATES = sorted(self.states.state.unique())
        self.CURRENT_STATE = self.states.state[-1]
        print('Current state:', self.CURRENT_STATE)
        self.data = self._get_data()
        self.macd_params = macd_params
        self.rsi_window = rsi_window
        self.lmb = lmb                

    @staticmethod
    def _get_states(path):
        states = pd.read_csv(path)
        states.index = pd.to_datetime(states.Date)
        return states
    
    def _get_data(self):
        data = (
            yf
            .download(self.stocks, start=str(self.start), end=str(TOMORROW))
            .rename(columns={'Adj Close': 'AdjClose'})
            .sort_index())
        data['state'] = self.states.state
        return data

    def get_metrics(self, plot=False):
        print('Getting metrics...')
        manager = mp.Manager()
        out = manager.list()
        pool = mp.Pool(N_JOBS)
        for stock in self.stocks:
            pool.apply_async(self._process_stock, args=(stock, out))
        pool.close()
        pool.join()
        out = out[:]  # convert managed list to regular list
        out_df = self._format_output(out)
        return out_df

    def _process_stock(self, stock, out):
        stock_data = self._get_stock_data(stock)
        n = len(stock_data.AdjClose[stock_data.AdjClose.notnull()])
        stock_data['direction'] = self._get_macd(stock_data, *self.macd_params)
        stock_data['rsi'] = self._get_rsi(stock_data)
        stock_data.rsi = stock_data.rsi.rank(pct=True)
        stock_data.fillna(method='ffill', inplace=True)
        stock_data.fillna(method='bfill', inplace=True)
        daily_returns = self._get_daily_returns(stock_data.AdjClose)
        sharpe = self._get_sharpe(daily_returns)
        weighted_sharpe = self._get_weighted_sharpe(daily_returns)
        stock_data = self._get_updown_geomean(stock_data, n)
        stock_data = self._get_fair_value(stock_data, n)
        stats = (
            stock_data.AdjClose.tolist()[-1],
            stock_data.direction.tolist()[-1],
            stock_data.rsi.tolist()[-1],
            stock_data.resid.tolist()[-1],  # 'fair value'
            stock_data.geomean.tolist()[-1],
            sharpe,
            weighted_sharpe)
        out.append([stock, *stats])
        
    def _get_stock_data(self, stock):
        sub = self.data.loc[:, [x for x in list(self.data) if x[1] == stock]]
        first_val = sub.AdjClose[stock][sub.AdjClose[stock].notnull()].index[0]
        sub = sub.loc[first_val:, :]
        sub.columns = sub.columns.to_series().apply(lambda x: x[0])
        sub.fillna(method='ffill', inplace=True)
        return sub

    @staticmethod
    def _get_macd(stock_data, fast=60, slow=90, signal=80):
        df = stock_data.copy()
        exp1 = df.AdjClose.ewm(span=fast, adjust=False).mean()
        exp2 = df.AdjClose.ewm(span=slow, adjust=False).mean()
        macd = exp1 - exp2
        exp3 = macd.ewm(span=signal, adjust=False).mean()
        histo = macd - exp3
        hdiff = histo.diff(1)
        is_rising = 1 * (hdiff > 0)
        return is_rising

    def _get_rsi(self, stock_data):
        window = self.rsi_window
        df = stock_data.copy()
        df['deltas'] = df.AdjClose.diff(1)
        df['gains'] = df.deltas.clip(lower=0)
        df['losses'] = df.deltas.clip(upper=0).abs()
        df['avg_gain'] = (
            df
            .gains
            .rolling(window=window, min_periods=window)
            .mean()[:window + 1])
        df['avg_loss'] = (
            df
            .losses
            .rolling(window=window, min_periods=window)
            .mean()[:window + 1])
        # avg gains
        pd.options.mode.chained_assignment = None
        for i, row in enumerate(df.avg_gain.iloc[window + 1:]):
            df.avg_gain.iloc[i + window + 1] = (
                (df.avg_gain.iloc[i + window] * (window - 1) 
                 + df.gains.iloc[i + window + 1]) 
                / window)
        # avg losses
        for i, row in enumerate(df.avg_loss.iloc[window + 1:]):
            df.avg_loss.iloc[i + window + 1] = (
                (df.avg_loss.iloc[i + window] * (window - 1)
                 + df.losses.iloc[i + window + 1])
                / window)
        df['rs'] = df.avg_gain / df.avg_loss
        df['rsi'] = 100 - (100 / (1. + df.rs))
        return df.rsi

    @staticmethod
    def _get_daily_returns(x):
        idx = x.index
        n = len(x)
        x = np.array(x)
        returns = x[1:n] / x[0:(n-1)] - 1
        returns = pd.Series(returns, index=idx[1:])
        return returns

    @staticmethod
    def _get_sharpe(returns, window=None):
        n = len(returns)
        if window is None:
            window = n
        if len(returns) > window:
            returns = returns[(n - window):n]
        returns = returns[returns.notnull()]
        sharpe = (np.sqrt(252) * returns.mean()) / returns.std()
        return sharpe

    def _get_weighted_sharpe(self, daily_returns):
        out = 0
        for state, prob in zip(self.trans.next_state, self.trans.prob):
            if len(daily_returns[self.data.state == state]):
                state_sharpe = self._get_sharpe(
                    daily_returns[self.data.state == state])        
                out += prob * state_sharpe
        return out

    @staticmethod
    def _get_updown_geomean(stock_data, n):
        WINDOW = min(n // 2, 125)
        stock_data['ann_high'] = (
            stock_data.AdjClose.rolling(window=WINDOW).max())
        stock_data['ann_low'] = (
            stock_data.AdjClose.rolling(window=WINDOW).min())
        stock_data['drop'] = stock_data.AdjClose / stock_data.ann_high
        stock_data['climb'] = stock_data.AdjClose / stock_data.ann_low
        stock_data['geomean'] = np.sqrt(stock_data['drop'] * stock_data.climb)
        stock_data.geomean = stock_data.geomean.rank(pct=True)
        return stock_data

    def _get_fair_value(self, stock_data, n):
        stock_data['W'] = [self.lmb ** t for t in range(n)][::-1]
        stock_data['n_samp'] = (stock_data.Volume / 100)  * stock_data.W
        stock_data['value'] = (
            (stock_data.AdjClose
             + stock_data.High
             + stock_data.Low
             + stock_data.Open) 
            / 4)
        stock_data['est'] = np.nan
        for day in stock_data.index:
            sub = stock_data[stock_data.index <= day]
            weights = sub.n_samp
            weights = weights / weights.sum()
            values = sub.value
            est = np.dot(weights, values)
            stock_data.loc[day, 'est'] = est
        stock_data['resid'] = stock_data.AdjClose / stock_data.est
        stock_data.resid = stock_data.resid.rank(pct=True)
        return stock_data

    @staticmethod
    def _format_output(out):
        out_df = pd.DataFrame(
            data=out,
            columns=[
                'stock', 'price', 'direction', 'RSI', 'fair_value_mult',
                'geomean', 'sharpe', 'weighted_sharpe'])
        out_df.RSI.fillna(out_df.RSI.median())
        out_df['RSIRev'] = 1 - out_df.RSI
        out_df = out_df[[
            'stock', 'price', 'direction', 'RSI', 'RSIRev', 'fair_value_mult',
            'geomean', 'sharpe', 'weighted_sharpe']]
        out_df.weighted_sharpe.fillna(
            out_df.weighted_sharpe.mean(), inplace=True)
        out_df.sort_values('stock', inplace=True)
        return out_df
